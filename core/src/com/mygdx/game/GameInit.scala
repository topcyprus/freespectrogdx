package com.mygdx.game

import com.badlogic.gdx.scenes.scene2d.actions.TemporalAction
import com.badlogic.gdx.scenes.scene2d.ui.Image
import com.badlogic.gdx.scenes.scene2d.{Group, Actor}

import collection.JavaConverters._
import com.badlogic.gdx.{InputMultiplexer, Input, InputAdapter, Gdx}
import com.mygdx.game.gui._
import priv.sp._
import priv.sp.update.UpdateListener
import priv.util.TVar
import priv.util.Utils._


class GameInit(screenResources : ScreenResources, gameResources : GameResources) {
  val spGame = new SpGame(new Local(gameResources), gameResources)
  val descriptionPanel = new DescriptionPanel(spGame, screenResources)
  val slotPanels = playerIds.map{ playerId =>
    new SlotPanel(playerId, spGame, screenResources)
  }
  val cardPanels = playerIds.map{ playerId =>
    new CardPanel(playerId, spGame, descriptionPanel, screenResources)
  }
  val userMenu         = new UserMenu(screenResources)
  val board            = new Board(spGame.myPlayerId, slotPanels, cardPanels, descriptionPanel, userMenu)
  screenResources.stage addActor board.panel
  val commandRecorder = new CommandRecorder(spGame, board)
  spGame.controller   = new UserGameController(board, commandRecorder, screenResources)
  spGame.updater.updateListener = new GameUpdateListener(board, spGame, screenResources)
  slotPanels foreach (_.init(commandRecorder))
  cardPanels foreach (_.init(commandRecorder))

  userMenu.skipButton.addListener(onClick {
    println("skip")
    if (spGame.state.checkEnded.isEmpty && commandRecorder.cont.isDefined) {
      commandRecorder.skip()
    }
  })

  userMenu.restartButton.addListener(onClick {
    println("restart")
    board.cardPanels foreach (_.setEnabled(false))
    spGame.gameLock.release()
    spGame.gameLock = new priv.util.RichLock
    spGame.server.reset()
    gameResources.gameExecutor submit runnable {
      spGame.persistState(spGame.server.initState)
      spGame.start()
    }
  })

  userMenu.settingsButton.addListener(onClick {
    screenResources.stage.addActor(new GameSettings(gameResources, screenResources.skin))
  })

  var isDebug = false
  Gdx.input.setInputProcessor(
    new InputMultiplexer(Gdx.input.getInputProcessor,
      screenResources.stage,
      new InputAdapter(){
        override def keyDown(k : Int) = {
          if (k == Input.Keys.F5) {
            Gdx.app.log("input", "reload resources")
            screenResources.reload()
            true
          } else if (k == Input.Keys.F6) {
            isDebug = ! isDebug
            Gdx.app.log("input", "set debug " + isDebug)
            setDebug(board.panel)
            true
          } else false
        }
      }))

  gameResources.gameExecutor submit runnable(spGame.start())

  def setDebug(group : Group) : Unit = {
    group setDebug isDebug
    group.getChildren.asScala foreach {
      case g : Group => setDebug(g)
      case a : Actor => a.setDebug(isDebug)
    }
  }
}


class UserGameController(board : Board, commandRecorder : CommandRecorder, screenResources : ScreenResources)
  extends SpGameController {
  import screenResources._

  def endGame(msg : String) = {
    screenResources.stage.addActor(new EndMessage(msg, skin))
    commandRecorder.cont foreach (_.set(None))
  }
  def disableSlots() : Unit = {
    board.slotPanels.foreach(_.disable())
  }
  def setCardEnabled(player : PlayerId, enabled : Boolean) : Unit = {
    board.cardPanels(player).setEnabled(enabled)
  }

  def notifyPlayed(card : Option[Card]) : Unit = {

  }

  def setPhase(player : PlayerId, phase : Option[String]): Unit = {
    board.slotPanels(player).lifeLabel setPhase phase
  }

  def refresh(silent : Boolean) : Unit = board refresh silent

  def addVisibleCard(player : PlayerId, card : Card) : Unit = {
    board.cardPanels(player)
  }

  def waitForUser(c: TVar[Option[Command]]) = {
    commandRecorder.startWith(c) {
      board.cardPanels(commandRecorder.game.myPlayerId) setEnabled true
    }
  }
}


private class GameUpdateListener(board : Board, spGame : SpGame, screenResources : ScreenResources) extends UpdateListener {
  import board._
  import spGame._

  def focus(num: Int, playerId: PlayerId, blocking: Boolean) {
    val slotButton = slotPanels(playerId).slots(num)
    waitAction(slotButton.focus())
  }
  def move(num: Int, dest: Int, playerId: PlayerId) {
    val slotButton = slotPanels(playerId).slots(num)
    gameLock waitLock { lock ⇒
      //world addTask new slotButton.MoveAnimTask(dest, lock)
    }
  }
  def runSlot(num: Int, playerId: PlayerId) {
    val slotButton = slotPanels(playerId).slots(num)
    waitAction(slotButton.run())
  }
  def summon(num: Int, slot: SlotState, playerId: PlayerId) {
    //val sourceCoord = (cardPanels(playerId).getPositionOf(slot.card) orElse cardPanels(other(playerId)).getPositionOf(slot.card)).getOrElse(Coord2i(0, 0))
    val slotButton = slotPanels(playerId).slots(num)
    //spawn(slotButton.summon(sourceCoord, slot), blocking = true)
    persistUpdater()
    controller.refresh(silent = true)
  }
  def die(num: Int, playerId: PlayerId) {
    val slotButton = slotPanels(playerId).slots(num)
    waitAction(slotButton.fade())
  }
  def refresh(silent: Boolean) = {
    persistUpdater()
    controller refresh silent
    //state.checkEnded foreach(endGame _)
  }
  def spellPlayed(c: Command) {
    notifySpell(c, sp.houses.getHouseById(c.card.houseId))
    /**
       val sourceCoord = cardPanels(c.player) getPositionOf(c.card) getOrElse Coord2i(0, 0)
       val targetPlayer = if (c.card.inputSpec == Some(SelectOwnerCreature)) {
         c.player
       } else other(c.player)
       gameLock.waitLock { lock ⇒
         world.doInRenderThread {
           slotPanels(targetPlayer).summonSpell(c, sourceCoord, lock)
         }
       }*/
  }
  def triggerAbility(o: AnyRef) {

  }

  private def notifySpell(c : Command, house: House) : Unit = {
    val path = CardActors.getPath(c.card, house)
    val sprite = screenResources.atlas createSprite path
    val image = new Image(sprite)
    image.setPosition(50, 500)
    board.panel.addAction(new SpellPlayed(board.panel, image))
  }
}


class SpellPlayed(group : Group, image : Image) extends TemporalAction {
  setDuration(1f)

  protected override def begin() {
    group.addActor(image)
  }

  protected def update(percent: Float) = {
  }

  protected override def end(): Unit = {
    group.removeActor(image)
  }
}
