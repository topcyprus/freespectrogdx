package com.mygdx.game.gui

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.scenes.scene2d.ui.{Table, Skin, Dialog}
import com.badlogic.gdx.scenes.scene2d.ui.{List => UIList}
import priv.sp._

object Dialogs {
  def center(dialog : Dialog) : Unit = {

    dialog.setPosition(
      (Gdx.graphics.getWidth - dialog.getWidth) / 2,
      (Gdx.graphics.getHeight - dialog.getHeight) / 2)
  }
}

class EndMessage(msg: String, skin : Skin) extends Dialog(msg, skin){
  button("ok")

  Dialogs.center(this)

}


class GameSettings(resources : GameResources,skin : Skin) extends Dialog("game settings", skin) {
  Dialogs.center(this)

  // TODO multi selection
  class PlayerChoice(id: PlayerId) {
    val table = new Table

    val specials = resources.sp.houses.special
    val randomLabel = "<random>"
    val choices = (randomLabel :: specials.map(_.name)).toArray
    val l = new UIList[String](skin)
    l.getSelection
    l.setItems(choices : _*)
    resources.playerChoices(id) foreach (h =>l.setSelected(h.name))
    updateResources()
    table add l

    def updateResources() : Unit = {
      if (l.getSelectedIndex != -1) {
        val choices = if (l.getSelected == randomLabel) Nil else specials.filter(x ⇒ l.getSelected == x.name)
        resources.playerChoices = resources.playerChoices.updated(id, choices)
      }
    }


  }

  val choices = playerIds map { id ⇒
    val c = new PlayerChoice(id)
    add(c.table)
    c
  }

  button("ok", 'ok)
  button("cancel", 'cancel)

  protected override def result (obj: AnyRef) : Unit = {
    obj match {
      case 'ok =>
        choices.foreach(_.updateResources())
      case _ =>
    }
  }

  pack()
}
