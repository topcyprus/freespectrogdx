package com.mygdx.game.gui


import com.badlogic.gdx.graphics.Color
import com.badlogic.gdx.scenes.scene2d.Group
import com.badlogic.gdx.scenes.scene2d.actions.{TemporalAction, MoveToAction}
import com.badlogic.gdx.scenes.scene2d.ui.Label.LabelStyle
import com.badlogic.gdx.scenes.scene2d.ui.{VerticalGroup, Skin, Label}
import com.badlogic.gdx.utils.Align
import priv.sp.House

class LifeLabel(name: String, getLife : => Int, skin : Skin) {
  val panel = new VerticalGroup()
  val label      = new Label(name + " ", skin)
  val lifeLabel  = DamagableInt(getLife, skin)
  val phaseLabel = new Label("", skin)
  val lifeRow    = row(label, lifeLabel.panel)
  lifeLabel.panel.setY(0)
  lifeRow.align(Align.bottom)

  lifeRow    align    Align.bottomLeft
  label      setColor Color.WHITE
  phaseLabel setColor Color.GRAY
  panel      addActor lifeRow

  panel.pack()

  val setPhase : Option[String] => Unit = {
    case None => panel removeActor phaseLabel
    case Some(s) =>
      phaseLabel setText s
      panel addActor phaseLabel
  }
}


class HouseLabel(getMana : => Int, val house: House, skin : Skin, flip: Boolean = false) {
  val label = new Label(house.name + " ", skin)
  val direction = if (flip) -1 else 1
  val manaLabel = DamagableInt(getMana, skin, if (flip) -1 else 1)
  val panel = row(label, manaLabel.panel)
  manaLabel.panel.setY(0)
  panel.align(Align.bottom)

  /**on {
    case MouseMoved(_) ⇒
      if (house.description.nonEmpty) {
        game.descriptionPanel.describedOption = Some(house)
      }
  }*/
}

object DamagableInt {

  def apply(getValue: ⇒ Int, skin : Skin, direction : Int = 1) = {
    val label = new Label("0", skin)
    val damagable = new DamagableInt(getValue, label, direction)
    label.setText(damagable.current.toString)
    damagable.panel.addActor(label)
    label.setAlignment(Align.bottom)
    damagable
  }
}

class DamagableInt(getValue: ⇒ Int, label : Label, direction : Int = 1)  {
  var current = getValue
  val panel = new Group

  def refresh(silent: Boolean = false) {
    val old = current
    current = getValue
    label.addAction(new UpdateAction[Label](_.setText(current.toString)))
    val d = current - old
    if (d != 0 && !silent) {
      panel.addAction(new DamageAction(d, panel, label.getStyle, direction))
    }
  }
}


class DamageAction(d : Int, group : Group, style : LabelStyle, direction : Int = 1) extends TemporalAction {
  setDuration(1f)
  val damageLabel = new Label(if (d>0) "+"+d else d.toString, style)
  damageLabel.setColor(if (d > 0) Color.GREEN else Color.RED)

  protected override def begin() {
    group.addActor(damageLabel)
    val moveAction = new MoveToAction()
    moveAction.setPosition(0, 20 * direction)
    moveAction setDuration 1f
    damageLabel addAction moveAction
  }

  protected def update(percent: Float) = {
  }

  protected override def end(): Unit = {
    group.removeActor(damageLabel)
  }
}

