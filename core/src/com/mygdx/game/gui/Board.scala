package com.mygdx.game.gui

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.scenes.scene2d.Group
import com.badlogic.gdx.scenes.scene2d.ui.VerticalGroup
import com.badlogic.gdx.utils.Align
import priv.sp._

class Board(
             val playerId: PlayerId,
             val slotPanels: List[SlotPanel],
             val cardPanels : List[CardPanel],
             descriptionPanel : DescriptionPanel,
             userMenu : UserMenu) {

  val rightPane = new VerticalGroup()

  rightPane align Align.right
  rightPane addActor cardPanels(other(playerId)).panel
  rightPane addActor column(
      slotPanels(other(playerId)).panel,
      slotPanels(playerId).panel)
  rightPane addActor cardPanels(playerId).panel
  rightPane.pack()

  val panel = new Group
  panel addActor rightPane
  panel addActor userMenu.panel
  panel addActor descriptionPanel.panel

  userMenu.panel.setPosition(10, 350)
  rightPane.setX(Gdx.graphics.getWidth - rightPane.getWidth - 10)

  def refresh(silent : Boolean) = {
    slotPanels foreach (_.refresh())
    cardPanels foreach (_.refresh(silent))
  }
}

import priv.util.TVar

class CommandRecorder(val game : SpGame, board : Board) {
  private var value = Option.empty[Command]
  var cont = Option.empty[TVar[Option[Command]]]

  def setCommand(command: Command) {
    board.slotPanels foreach(_.disable())
    value = Some(command)
    nextStep()
  }

  def startWith(c: TVar[Option[Command]])(f: ⇒ Unit) {
    value = None
    cont = Some(c)
    f
  }

  def addInput(x: SlotInput) = {
    value foreach { command ⇒
      setCommand(command.copy(input = Some(x)))
    }
  }

  def skip() {
    continue(None)
  }

  private def continue(c: Option[Command]) = {
    cont.foreach(_.set(c))
    cont = None
  }

  private def nextStep() {
    value foreach { command ⇒
      if (command.card.inputSpec.size == command.input.size) {
        continue(Some(command))
      } else {
        import game._
        import board._
        def addInputOrEnable(playerId: PlayerId, slots: Traversable[Int]) {
          if (slots.size == 1) {
            addInput(new SlotInput(slots.head))
          } else {
            slotPanels(playerId) setSlotEnabled slots
          }
        }

        command.card.inputSpec.get match {
          case SelectOwner(f) ⇒
            addInputOrEnable(myPlayerId, f(myPlayerId, state))
          case SelectOwnerSlot ⇒
            addInputOrEnable(myPlayerId, PlayerState.openSlots(state.players(myPlayerId)))
          case SelectOwnerCreature ⇒
            addInputOrEnable(myPlayerId, state.players(myPlayerId).slots.keys.toList)
          case SelectTarget(f) ⇒
            addInputOrEnable(otherPlayerId, f(otherPlayerId, state))
          case SelectTargetSlot ⇒
            addInputOrEnable(otherPlayerId, PlayerState.openSlots(state.players(otherPlayerId)))
          case SelectTargetCreature ⇒
            addInputOrEnable(otherPlayerId, state.players(otherPlayerId).slots.keys.toList)
        }
      }
    }
  }
}