package com.mygdx.game.gui

import com.badlogic.gdx.graphics.Color
import com.badlogic.gdx.scenes.scene2d.utils.ClickListener
import com.badlogic.gdx.scenes.scene2d.{Actor, InputEvent}
import com.badlogic.gdx.scenes.scene2d.ui.{VerticalGroup, Label}
import com.badlogic.gdx.utils.Align
import com.mygdx.game.ScreenResources
import priv.sp._

class DescriptionPanel(game: SpGame, resources : ScreenResources, color : Color = Color.WHITE) {
  val panel = new VerticalGroup()
  val title = new Label("", resources.skin)
  val subtitle = new Label("", resources.skin)
  val description = new Label("", resources.skin)
  description.setWrap(true) // FIXME
  description.setFontScale(0.8f)
  description.setWidth(200)
  panel.setSize(200, 200)
  panel.align(Align.bottomLeft)
  List(title, subtitle, description) foreach (_.setColor(color))

  val update : Option[Described] => Unit = {
    case None =>
      panel.clear()
    case Some(described) =>
      panel.clear()
      title.setText(described.name)
      panel.addActor(title)
      described match {
        case c: Creature ⇒
          subtitle.setText("Life : " + c.life + "  Attack : " + c.attack.base.getOrElse("X"))
          panel.addActor(subtitle)
        case _ ⇒
      }
      panel.addActor(description)
      description.setText(described.description)

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
