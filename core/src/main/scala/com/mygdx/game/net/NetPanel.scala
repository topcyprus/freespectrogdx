package com.mygdx.game.net

import com.badlogic.gdx.scenes.scene2d.ui.TextField.TextFieldListener
import com.badlogic.gdx.scenes.scene2d.ui.{List => _, _}
import com.mygdx.game._
import com.mygdx.game.gui._

class NetPanel(
  screens : Screens, buttons : ButtonPanel) {

  import screens._
  import screenResources.{skin2 => skin}

  val name = new TextField(System.getProperty("user.name"), skin)
  val host = new TextField("localhost", skin)
  val port = new TextField("12345", skin)
  val logs = new TextArea("", skin)
  val chat = new TextField("" , skin)
  val nbRows = 20
  logs setPrefRows nbRows

  val playerList = new TextArea("", skin)
  playerList setPrefRows nbRows

  val table = new Table
  table.add(name).colspan(2).left()
  table.row()
  table.add(row(host, port)).colspan(2).left()
  table.row()
  table.add(logs).fillX().expandX()
  table.add(playerList)
  table.row()
  table.add(chat).colspan(2).fillX()

  val panel = table
  table.pad(5).bottom().pack()

  buttons.ConnectButton addListener onClick {
    screenResources.clientOption foreach { client => client.release()  }
    try {
      val client = new NetClient(
        host.getText, port.getText.toInt, name.getText,
        screens,
        logText, setPlayerList)
      screenResources.clientOption = Some(client)
      logText("Connected")
    } catch { case e : Exception =>
      logText(e.getMessage)
      screens.lastE = Some(e)
    }
  }

  buttons.getButton("Random opponent") addListener onClick {
    screenResources.clientOption foreach { client =>
      logText("Searching...")
      client send Message(Header(MessageType.RequestDuel))
    }
  }

  chat setTextFieldListener new TextFieldListener {
    override def keyTyped(textField: TextField, c: Char) : Unit = {
      if (c == '\r' || c == '\n') {
        screenResources.clientOption foreach { client =>
          client proxyMessage ChatMessage(client.user + ": " + chat.getText)
          chat setText ""
        }
      }
    }
  }

  def setPlayerList(players : List[PlayerInfo]) = {
    playerList setText players.map{ p =>
      p.status match {
        case PlayerStatus.Duelling => p.name + "(" + p.status + ")"
        case _ => p.name
      }
    }.mkString("\n")
  }

  def logText(s : String) = {
    logs.appendText(s + "\n")
  }
}
