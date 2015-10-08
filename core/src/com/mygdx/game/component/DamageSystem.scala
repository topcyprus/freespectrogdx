package com.mygdx.game.component

import com.badlogic.ashley.core.{Engine, Component, Family, ComponentMapper}
import com.badlogic.gdx.graphics.Color
import com.badlogic.gdx.graphics.g2d.Batch
import com.badlogic.gdx.math.Vector2
import com.badlogic.gdx.scenes.scene2d.actions.MoveToAction
import com.badlogic.gdx.scenes.scene2d.ui.Label
import com.badlogic.gdx.scenes.scene2d.ui.Label.LabelStyle

class DamageComponent(val d : Int, pos : Vector2, style : LabelStyle, direction : Int = 1) extends Component {

  val damageLabel = new Label(if (d>0) "+"+d else d.toString, style)
  damageLabel.setColor(if (d > 0) Color.GREEN else Color.RED)
  damageLabel.setPosition(pos.x, pos.y)

  val moveAction = new MoveToAction()
  moveAction.setPosition(pos.x, pos.y + 20 * direction)
  moveAction setDuration 1f
  damageLabel addAction moveAction

  def isCompleted = moveAction.getTime >= moveAction.getDuration
}

class DamageSystem(engine : Engine, batch : Batch) extends BaseSystem {
  protected val family = Family.all(classOf[DamageComponent]).get()
  private val dm = ComponentMapper.getFor(classOf[DamageComponent])

  override def update(deltaTime : Float) = {
    val ite = entities.iterator()
    batch.begin()
    try {
      while (ite.hasNext) {
        val entity = ite.next()
        val damage = dm get entity
        damage.damageLabel act deltaTime
        if (damage.isCompleted) {
          engine removeEntity entity
        } else {
          damage.damageLabel.draw(batch, 1)
        }
      }
    } finally batch.end()
  }
}


