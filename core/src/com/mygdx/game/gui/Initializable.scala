package com.mygdx.game.gui

import java.util.concurrent.atomic.AtomicBoolean

import com.badlogic.gdx.graphics.g2d.Batch
import com.badlogic.gdx.math.{Vector2, Matrix4}
import com.badlogic.gdx.scenes.scene2d.Actor

trait Initializable {
  val initialized = new AtomicBoolean()
  def init(batch : Batch): Unit = {
    if (initialized.compareAndSet(false, true)) {
      initialize(batch)
    }
  }

  def initialize(batch : Batch): Unit = {}
}

trait StaticAbsoluteProjMatrix extends Initializable { _ : Actor =>

  var absoluteProjMatrix : Matrix4 = _
  override def initialize(batch : Batch) = {
    absoluteProjMatrix = getAbsoluteProjMatrix(this, batch)
    super.initialize(batch)
  }

}