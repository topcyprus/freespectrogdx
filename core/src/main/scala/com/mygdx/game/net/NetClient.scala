package com.mygdx.game.net

import java.io._
import java.net._
import java.security.{MessageDigest, NoSuchAlgorithmException}
import java.util.Base64

import collection._
import scala.concurrent.Promise
import scala.util.{Failure, Success}
import priv.util.Utils.thread
import com.mygdx.game.{RemoteGameScreenContext, Screens}
import priv.sp._
import priv.util.Log

import scala.concurrent.ExecutionContext.Implicits.global

class NetClient(host : String, port : Int, val user : String,
                screens : Screens,
                logMsg : String => Unit,
                logDuelRequest : String => Unit,
                setPlayerList : List[PlayerInfo] => Unit) {

  import screens._

  val socket  = new Socket(host, port)
  val out     = socket.getOutputStream()
  val in      = socket.getInputStream()
  val pout    = new PrintWriter(out, true)
  val pending = mutable.HashMap.empty[Int, Promise[Any]]
  val messageQueue = new java.util.concurrent.LinkedBlockingQueue[Any]
  val log     = new Log(this)
  val checksum = getChecksum()
  def kryo    = GameKryoInstantiator.kryo

  var running = true
  thread("net client") {
    while (running) {
      getMessage() foreach { message =>
        if (message.header.answerId > 0) {
          pending.get(message.header.answerId) match {
            case Some(prom) => prom.success(message)
            case None => handle(message)
          }
        } else {
          handle(message)
        }
      }
    }
    release()
  }

  def handle( message : Message ) = {
    log.debug("Handle " + message)
    message.header.messageType match {
      case MessageType.Welcome =>
        message.body foreach ( x => logMsg(new String(x)) )
        send(Message(Header(MessageType.Name), Some(user.getBytes)))
      case MessageType.ListPlayers =>
        message.body foreach { bytes =>
          setPlayerList(new String(bytes).split(";").toList.map(PlayerInfo.decode))
        }
      case MessageType.NewGame =>
        // ask opponent name and house preferences
        val prom = proxyAsk(new AskOpponentInfo)
        prom.future onComplete {
          case Failure(t) => logMsg(t.getMessage) ; t.printStackTrace()
          case Success(OpponentInfo(oppName, oppHouses, oppChecksum)) =>
            if (oppChecksum != checksum) {
              val msg = "Checksum mismatch " + checksum + "/" + oppChecksum
              log.error(msg)
              logMsg(msg)
              Thread.sleep(1000)
            }
            // a new game has been requested
            val seed = GameSeed.create(gameResources, user, oppName, oppHouses)
            // send the seed to the opponent
            proxyMessage(seed)
            screenResources.beforeProcess invoke {
              gameScreen.select()
              val opp = new RemoteOpponent(gameResources, this, oppName, opponent, owner, seed)
              new RemoteGameScreenContext(gameScreen, opp)
            }
          case msg => log.debug("unknown msg " + msg)
        }
      case MessageType.ExitDuel =>
        screenResources.beforeProcess.invoke {
          gameScreen.returnToStart()
        }
      case MessageType.RequestDuel =>
        message.body foreach { bytes =>
          logDuelRequest(new String(bytes))
        }
      case MessageType.Proxy =>
        message.body foreach { bytes =>
          val pMessage = kryo fromBytes bytes
          pMessage match {
            case seed : GameSeed =>
              screenResources.beforeProcess.invoke {
                gameScreen.select()
                val opp = new RemoteOpponent(gameResources, this, seed.name, owner, owner, seed)
                new RemoteGameScreenContext(gameScreen, opp)
              }
            case ChatMessage(chatMsg) => logMsg(chatMsg)
            case ProxyAsk(msg, id) =>
                msg match {
                  case _ : AskOpponentInfo =>
                    proxyMessage(ProxyAnswer(id,
                      OpponentInfo(user, gameResources.playerChoices(owner).map(_.houseId), checksum)))
                  case _ => log.debug("unknown msg " + msg)
                }
            case ProxyAnswer(answerId, msg) =>
              pending(answerId).success(msg)
            case _ =>
              log.debug("message " + pMessage)
              messageQueue put pMessage
              log.debug("message " + pMessage + " enqueued")
          }
        }
      case MessageType.RequestFailed =>
        logMsg("request failed")
    }
  }

  def send(message : Message) = {
    val header = message.header.copy(length = message.body.map(_.length) getOrElse 0)
    pout.println(header.toStr)
    message.body foreach out.write
    out.flush()
  }

  def proxify(m : Any) : Message = {
    val bytes : Array[Byte] = kryo.toBytesWithClass(m)
    Message(Header(MessageType.Proxy), Some(bytes))
  }

  def proxyMessage(m : Any) = send(proxify(m))

  def proxyAsk(m : Any) : Promise[Any] = {
    val msg = ProxyAsk(m)
    send(proxify(msg))
    val prom = Promise[Any]()
    pending.put(msg.id, prom : Promise[Any])
    prom
  }

  def getMessage() : Option[Message] = {
    Header.fromStr(readLine()) map { header =>
      if (header.length > 0) {
        val bytes = new Array[Byte](header.length)
        in read bytes
        Message(header, Some(bytes))
      } else Message(header, None)
    }
  }

  def readLine() : String = {
    var result = ""
    var c = in.read()
    while (c != -1 && c.toChar != '\n') {
      result += c.toChar
      c = in.read()
    }
    result
  }

  def release() {
    running = false
    socket.close()
    logMsg("Disconnected")
    setPlayerList(Nil)
    messageQueue.clear()
    pending.clear()
  }

  def getChecksum() : String = {
    val currentJavaJarFile = new File(this.getClass.getProtectionDomain().getCodeSource().getLocation().getPath())
    val filepath = currentJavaJarFile.getAbsolutePath()
    try {
      val md = MessageDigest.getInstance("SHA-256")
      val fis = new FileInputStream(filepath)
      val dataBytes = new Array[Byte](1024)
      var n = fis read dataBytes

      while ( n != -1 ) {
        md.update(dataBytes, 0, n)
        n = fis read dataBytes
      }

      new String(md.digest())
    } catch { case e : Exception =>
      e.printStackTrace()
      "0"
    }
  }

}
