package com.mygdx.game.gui

import com.badlogic.gdx.graphics.Color
import com.badlogic.gdx.graphics.g2d.Batch
import com.badlogic.gdx.scenes.scene2d.Actor
import com.badlogic.gdx.scenes.scene2d.ui.{Image, Label}
import com.mygdx.game.ScreenResources
import priv.sp.{Creature, Spell, House, Card}

object CardActors {

  def getPath(card : Card, house : House) = {
    val houseFolder = if (card.houseIndex < 4) "base" else house.name.toLowerCase.replaceAll(" ", "")
    "cards/" + houseFolder + "/" + card.name
  }
}

class CardActors(val card: Card, house : House, val resources : ScreenResources) {
  val path = CardActors.getPath(card, house)
  val sprite = resources.atlas createSprite path

  if (sprite == null) throw new Exception("sprite not found " + path)

  val cardImage = new Image(sprite)
  if (card.isSpell) cardImage.setPosition(-1, -1)
  else              cardImage.setPosition(3, 14)

  val borderName = if (card.isSpell) "rakaSpell" else "raka"
  val borderTex = resources.atlas findRegion ("combat/"+borderName)

  val costLabel = new Label(card.cost.toString, resources.skin)
  costLabel setColor Color.BLUE
  val labels :List[Label] = card match {
    case spell: Spell ⇒
      costLabel.setPosition(65, 75)
      List(costLabel)
    case creature: Creature ⇒
      costLabel.setPosition(65, 80)
      val attackLabel = new Label(creature.attack.base.map(_.toString) getOrElse "?", resources.skin)
      val lifeLabel = new Label(creature.life.toString, resources.skin)
      attackLabel setColor Color.RED
      attackLabel.setPosition(2, 2)
      lifeLabel setColor Color.GREEN
      lifeLabel.setPosition(65, 2)
      List(costLabel, attackLabel, lifeLabel)
  }


  val borderImage = new Image(borderTex)

  val actors = cardImage :: borderImage :: labels

  assert(sprite != null, "sprite not defined " + path)

  def updateZIndex(): Unit ={
    cardImage.setZIndex(0)
    borderImage.setZIndex(2)
    labels.foreach(_.setZIndex(3))
  }

}