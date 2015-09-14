package com.mygdx.game.gui

import com.badlogic.gdx.scenes.scene2d.Actor
import com.badlogic.gdx.scenes.scene2d.ui.HorizontalGroup
import com.badlogic.gdx.utils.Align
import com.mygdx.game._
import priv.sp._

class SlotPanel(playerId: PlayerId, val game: SpGame,  resources : ScreenResources) {
  val lifeLabel = new LifeLabel(game.names(playerId), game.state.players(playerId).life, resources.skin)
  val slots =
    baseSlotRange.map(num ⇒
      new SlotButton(
      num,
      playerId,
      {
        val p = game.state.players(playerId)
        (p.slots get num, p.slotList contains num)
      }, game, resources)).toList
  val elts : List[Actor] = lifeLabel.panel :: slots.map(_.group)

  val panel = new HorizontalGroup()
  elts foreach panel.addActor
  panel.pack()

  def init(commandRecorder: CommandRecorder) = {
    def listenEvent(slotButton: SlotButton) {
      slotButton.group.addListener(onClick{
        if (slotButton.enabled){
          commandRecorder addInput new SlotInput(slotButton.num)
        }
      })
    }
    slots foreach listenEvent
  }

  //def otherPanel = board.slotPanels(other(playerId))

  def setSlotEnabled(s: Traversable[Int]) {
    val nums = s.toSet
    slots.foreach { slot ⇒ slot.enabled = nums contains slot.num }
  }
  def disable() { slots foreach (_.enabled = false) }
  def refresh() {
    lifeLabel.lifeLabel.refresh()
    slots foreach (_.refresh())
  }
}
