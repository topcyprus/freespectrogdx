package com.mygdx.game.gui


import com.badlogic.gdx.graphics.Color
import com.badlogic.gdx.scenes.scene2d.{Action, Group, Actor}
import com.badlogic.gdx.scenes.scene2d.actions.{VisibleAction, MoveToAction}
import com.badlogic.gdx.scenes.scene2d.ui.{VerticalGroup, Skin, Label}
import priv.sp.{HouseState, House}

class LifeLabel(name: String, skin : Skin) {
  val panel              = new VerticalGroup()
  private val label      = new Label(name, skin)
  private val phaseLabel = new Label("", skin)

  label      setColor Color.WHITE
  phaseLabel setColor Color.GRAY
  panel      addActor label
  panel.setSize(100, 100)

  val setPhase : Option[String] => Unit = {
    case None => panel removeActor phaseLabel
    case Some(s) =>
      phaseLabel setText s
      panel addActor phaseLabel
  }

}


class HouseLabel(getMana : => Int, house: House, skin : Skin, flip: Boolean = false) {
  val label = new Label("", skin)
  //val size = Coord2i(102, 54)
  val direction = if (flip) -1 else 1
  val legend = house.name + " : " + getMana
  label setText legend

  def refresh() {
    label setText (house.name + " : " + getMana)
    //glColor4f(1, 1, 1, 1)
    //Fonts.font.draw(offset, 22, legend + mana.current, 'white)
    //label setText legend + mana.current
    /**mana.getDamageAnimOpt foreach { anim ⇒
      Fonts.font.draw(20, 22 - direction * (10 + anim.delta(world.time)), anim.text, anim.color)
    }*/
  }

  /**on {
    case MouseMoved(_) ⇒
      if (house.description.nonEmpty) {
        game.descriptionPanel.describedOption = Some(house)
      }
  }*/
}


class DamagableInt(getValue: ⇒ Int, skin : Skin, group : Group, direction : Int = 1)  {
  var current = getValue

  val label = new Label("", skin)
  label.setVisible(false)
  group.addActor(label)

  val visibleFalse = new VisibleAction()
  visibleFalse.setVisible(false)

  val visibleTrue = new VisibleAction()
  visibleFalse.setVisible(true)

  def refresh(silent: Boolean = false) {
    val old = current
    current = getValue
    val d = current - old
    if (d != 0 && !silent) {
      label addAction UpdateAction[Label](_.setText(d.toString))
      label addAction visibleTrue
      val action = new MoveToAction()
      action.setPosition(0, 20 * direction)
      action setDuration 1000
      label addAction action
      label addAction visibleFalse
    }
  }

}

case class UpdateAction[A <: Actor](f : A => Unit) extends Action {

  def act (delta : Float) = {
    f(target.asInstanceOf[A])
    true
  }

}
