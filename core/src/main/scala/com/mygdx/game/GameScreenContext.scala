package com.mygdx.game

import collection.JavaConverters._
import com.badlogic.gdx.scenes.scene2d.{Actor, Group}
import com.badlogic.gdx.{Gdx, Input, InputAdapter, InputMultiplexer}
import com.mygdx.game.net.RemoteOpponent
import priv.sp.Local
import priv.util.Utils._

trait GameScreenContext {
  def gameScreen : GameScreen

  def releaseLocks(gameInit : GameInit) = {
    gameInit.session.gameLock.release()
    gameInit.listener.lock.release()
  }

  def initInput(handleKey : Int => Boolean) = {
    Gdx.input.setInputProcessor(
      new InputMultiplexer(Gdx.input.getInputProcessor,
        gameScreen.screens.screenResources.stage,
        new InputAdapter(){
          override def keyDown(k : Int) = {
            handleKey(k)
          }

          override def scrolled(amount : Int) = {
            scroll(-30 * amount)
            true
          }

          private var y = 0
          private val h = 768
          private def scroll(delta : Int) = {
            val newy = y + delta
            val dy =
              if (newy<0) -y
              else if (newy > 2 *h) 2 * h -y
              else delta
            y = y + dy
            gameScreen.screens.screenResources.stage.getCamera.translate(0, dy, 0)
          }
        }))
  }

}

class RemoteGameScreenContext(val gameScreen: GameScreen, opponent : RemoteOpponent) extends GameScreenContext {
  import gameScreen.screens._
  import net._
  val gameInit = new GameInit(screenResources, gameResources, opponent)
  initInput(_ => false)
  gameInit.userMenu.clearListeners()

  gameInit.userMenu.quitButton addListener onClick {
    opponent.client send Message(Header(MessageType.ExitDuel))
    releaseLocks(gameInit)
    gameScreen.returnToStart()
  }
}

class LocalGameScreenContext(val gameScreen: GameScreen) extends GameScreenContext {
  import gameScreen.screens._
  var currentGame = createGame()

  def createGame(): GameInit = gameScreen.resetScreen {
    if (currentGame != null) {
      releaseLocks(currentGame)
    }
    val gameInit = new GameInit(screenResources, gameResources, new Local(gameResources))
    screenResources.disconnectIfNeeded()

    gameInit.userMenu.newButton addListener onClick {
      currentGame = createGame()
    }
    gameInit.userMenu.surrenderButton addListener onClick {
      releaseLocks(currentGame)
      gameScreen.returnToStart()
    }
    localInit(gameInit)
  }


  def localInit(gameInit : GameInit) = {
    new LocalGameInit(gameInit)
    gameInit
  }

  class LocalGameInit(gameInit : GameInit) {
    import gameInit._

    userMenu.restartButton.addListener(onClick {
      println("restart")
      switchNewGame {
        session.server.reset()
      }
    })

    var isDebug = false
    initInput{ k : Int =>
      if (k == Input.Keys.F5) {
        Gdx.app.log("input", "reload resources")
        screenResources.reload()
        true
      } else if (k == Input.Keys.F6) {
        isDebug = !isDebug
        Gdx.app.log("input", "set debug " + isDebug)
        setDebug(board.panel)
        setDebug(startScreen.startBoard.panel)
        true
      } else if (k == Input.Keys.F8) {
        Gdx.app.log("input", "give mana")
        session.giveMeMana()
        true
      } else if (k == Input.Keys.F7) {
        println(session.state)
        true
      } else false
    }

    def switchNewGame(f : => Unit) {
      screenResources.engine.getSystems.toArray.foreach(_.setProcessing(false))
      board.cardPanels foreach (_.setEnabled(false))
      session.gameLock.release()
      session.gameLock = new priv.util.RichLock
      listener.lock.release()
      listener.lock = new priv.util.RichLock
      screenResources.engine.removeAllEntities()
      f
      screenResources.engine.getSystems.toArray.foreach(_.setProcessing(true))
      gameResources.gameExecutor submit runnable {
        session persistState session.server.initState
        session.start()
      }
    }

    def setDebug(group : Group) : Unit = {
      group setDebug isDebug
      group.getChildren.asScala foreach {
        case g : Group => setDebug(g)
        case a : Actor => a.setDebug(isDebug)
      }
    }
  }

}

