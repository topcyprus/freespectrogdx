package com.mygdx.game

import com.mygdx.game.gui._
import priv.sp._
import priv.sp.update.UpdateListener
import priv.util.TVar
import priv.util.Utils._


class GameInit(screenResources : ScreenResources) {
  val gameResources = new GameResources
  val spGame = new SpGame(new Local(gameResources), gameResources)
  val slotPanels = playerIds.map{ playerId =>
    new SlotPanel(playerId, spGame, screenResources)
  }
  val cardPanels = playerIds.map{ playerId =>
    new CardPanel(playerId, spGame, screenResources)
  }
  val descriptionPanel = new DescriptionPanel(spGame, screenResources)
  val userMenu         = new UserMenu(screenResources)
  val board            = new Board(spGame.myPlayerId, slotPanels, cardPanels, descriptionPanel, userMenu)
  screenResources.stage addActor board.panel
  val commandRecorder = new CommandRecorder(spGame, board)
  spGame.controller   = new UserGameController(board, commandRecorder)
  spGame.updater.updateListener = new GameUpdateListener(board, spGame)
  slotPanels foreach (_.init(commandRecorder))
  cardPanels foreach (_.init(commandRecorder))

  userMenu.skipButton.addListener(onClick {
    if (spGame.state.checkEnded.isEmpty && commandRecorder.cont.isDefined) {
      commandRecorder.skip()
    }
  })

  userMenu.restartButton.addListener(onClick {
    //world.forEntity[EndMessage](world.unspawn(_))
    board.cardPanels foreach (_.setEnabled(false))
    spGame.gameLock.release()
    spGame.gameLock = new priv.util.RichLock
    spGame.server.reset()
    gameResources.gameExecutor submit runnable {
      spGame.persistState(spGame.server.initState)
      spGame.start()
    }
  })


  gameResources.gameExecutor submit runnable(spGame.start())

}


class UserGameController(board : Board, commandRecorder : CommandRecorder) extends SpGameController {

  def endGame(msg : String) = {
    //world spawn new EndMessage(msg)
    commandRecorder.cont foreach (_.set(None))
  }
  def disableSlots() : Unit = {
    board.slotPanels.foreach(_.disable())
  }
  def setCardEnabled(enabled : Boolean) : Unit = {
    board.cardPanels.foreach(_.setEnabled(enabled))
  }

  def notifyPlayed(card : Option[Card]) : Unit = {

  }

  def setPhase(player : PlayerId, phase : Option[String]): Unit = {
    board.slotPanels(player).lifeLabel setPhase phase
  }

  def refresh(silent : Boolean) : Unit = {

  }
  def addVisibleCard(player : PlayerId, card : Card) : Unit = {
    board.cardPanels(player)
  }

  def waitForUser(c: TVar[Option[Command]]) = {
    commandRecorder.startWith(c) {
      board.cardPanels(commandRecorder.game.myPlayerId) setEnabled true
    }
  }
}


private class GameUpdateListener(board : Board, spGame : SpGame) extends UpdateListener {
  import board._
  import spGame._

  def focus(num: Int, playerId: PlayerId, blocking: Boolean) {
    val slotButton = slotPanels(playerId).slots(num)
    //spawn(new slotButton.Focus(), blocking)
  }
  def move(num: Int, dest: Int, playerId: PlayerId) {
    val slotButton = slotPanels(playerId).slots(num)
    gameLock waitLock { lock ⇒
      //world addTask new slotButton.MoveAnimTask(dest, lock)
    }
  }
  def runSlot(num: Int, playerId: PlayerId) {
    val slotButton = slotPanels(playerId).slots(num)
    //spawn(Running(slotButton.location, slotButton.direction), blocking = true)
  }
  def summon(num: Int, slot: SlotState, playerId: PlayerId) {
    //val sourceCoord = (cardPanels(playerId).getPositionOf(slot.card) orElse cardPanels(other(playerId)).getPositionOf(slot.card)).getOrElse(Coord2i(0, 0))
    val slotButton = slotPanels(playerId).slots(num)
    //spawn(slotButton.summon(sourceCoord, slot), blocking = true)
    persistUpdater()
    controller.refresh(silent = true)
  }
  def die(num: Int, playerId: PlayerId) {
    /**
     *     val slotButton = slotPanels(playerId).slots(num)
     * spawn(new slotButton.Fade, blocking = true)
     */
  }
  def refresh(silent: Boolean) = {
    persistUpdater()
    controller refresh silent
    //state.checkEnded foreach(endGame _)
  }
  def spellPlayed(c: Command) {
    /**spawn(new SpellNotif(sp, c.card))
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
}