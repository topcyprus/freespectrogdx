package com.mygdx.game.gui

import com.badlogic.gdx.scenes.scene2d.ui.{VerticalGroup, Label}
import com.badlogic.gdx.utils.Align
import com.mygdx.game.ScreenResources
import priv.sp._

class DescriptionPanel(game: SpGame, resources : ScreenResources) {
  val panel = new VerticalGroup()
  val title = new Label("", resources.skin)
  val subtitle = new Label("", resources.skin)
  val description = new Label("", resources.skin)
  description.setFontScale(0.8f)
  panel.setSize(200, 200)
  panel.align(Align.bottomLeft)

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
      description.setText(described.description)
      panel.addActor(description)

  }
}

/**
object DescriptionPanel {
  def show(offset: Int, color: Symbol)(described: Described) = {
    var cur = offset
    Fonts.big.draw(0, cur, described.name, color)
    cur += 25
    described match {
      case c: Creature ⇒
        Fonts.font.draw(0, cur, "Life : " + c.life + "  Attack : " + c.attack.base.getOrElse("X"), color)
        cur += 12
      case _ ⇒
    }
    if (described.description.size < 60) {
      Fonts.font.draw(0, cur, described.description, color)
    } else {
      described.description.split('\n').foreach { line ⇒
        Fonts.font.draw(0, cur, line, color)
        cur += 12
      }
    }
    cur
  }
}
class DescriptionPanel(game: Game) extends GuiElem {
  val size = Coord2i(200, 200)
  var describedOption = Option.empty[Described]
  val show = DescriptionPanel.show(0, 'white) _

  def render() {
    describedOption foreach show
  }
}

class InfoPanel(game: Game) extends GuiElem {
  val size = Coord2i(200, 200)
  var cardOption = Option.empty[Card]
  val show = DescriptionPanel.show(12, 'gray) _

  def add(c: Card) {
    cardOption = Some(c)
  }

  def render() {
    Fonts.font.draw(0, 0, "last play : ", 'gray)
    cardOption foreach show
  }
}
*/