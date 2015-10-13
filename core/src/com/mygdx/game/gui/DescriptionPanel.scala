package com.mygdx.game.gui

import com.badlogic.gdx.graphics.Color
import com.badlogic.gdx.scenes.scene2d.utils.ClickListener
import com.badlogic.gdx.scenes.scene2d.{Actor, InputEvent}
import com.badlogic.gdx.scenes.scene2d.ui.{Table, Container, VerticalGroup, Label}
import com.badlogic.gdx.utils.Align
import com.mygdx.game.ScreenResources
import priv.sp._

class DescriptionPanel(resources : ScreenResources,
                       color : Color = Color.WHITE,
                       descWidth : Float = 350,
                       displayCost : Boolean = false) {
  val panel = new Table()
  val title = new Label("", resources.skin)
  title setFontScale 1.1f
  val subtitle = new Label("", resources.skin)
  val description = new Label("", resources.skin)
  description setWrap true
  panel align Align.bottomLeft
  panel.add(title).left
  panel.add(subtitle).left
  panel.row()
  panel.add(description).left() width descWidth colspan 2
  List(title, subtitle, description) foreach (_.setColor(color))

  val update : Option[Described] => Unit = {
    case None =>
      panel.setVisible(false)
    case Some(described) =>
      described match {
        case c : Card if displayCost => title.setText(described.name + " ("+c.cost+")")
        case _ => title setText described.name
      }

      described match {
        case c: Creature ⇒ subtitle.setText("Life : " + c.life + "  Attack : " + c.attack.base.getOrElse("X"))
        case _ ⇒ subtitle.setText("")
      }
      description.setText(described.description)
      panel.setVisible(true)
  }
}

trait HoverToDesc { _ : ClickListener =>
  def descPanel : DescriptionPanel
  def described : Option[Described]

  override def enter(event: InputEvent, x: Float, y: Float, pointer : Int, fromActor : Actor) : Unit = {
    descPanel.update(described)
  }
  override def exit(event: InputEvent, x: Float, y: Float, pointer : Int, fromActor : Actor) : Unit = {
    descPanel.update(None)
  }
}
