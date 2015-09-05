package com.mygdx.game

import com.badlogic.gdx.graphics.GL20
import com.badlogic.gdx.graphics.g2d.{TextureAtlas, SpriteBatch}
import com.badlogic.gdx.scenes.scene2d.Stage
import com.badlogic.gdx.scenes.scene2d.ui.Skin
import com.badlogic.gdx.{Gdx, ScreenAdapter, Game}

class MyGdxGame extends Game {

  override def create() {
    setScreen(new GameScreen(this))
  }
}


class GameScreen(game :Game) extends ScreenAdapter {
  val screenResources = new ScreenResources
  val gameAdapter = new GameInit(screenResources)

  override def render (delta : Float): Unit = {
    import screenResources._
    Gdx.gl glClear GL20.GL_COLOR_BUFFER_BIT

    batch.begin()
    // Drawing goes here!
    stage act delta
    stage.draw()

    batch.end()
  }
  override def resize (width : Int, height : Int) {
    screenResources.stage.getViewport.update(width, height, true)
  }

}

class ScreenResources {
  val batch = new SpriteBatch
  val stage = new Stage()
  val atlas = new TextureAtlas(Gdx.files.internal("pack/images.pack.atlas"))
  val skin  = new Skin(Gdx.files.internal("data/uiskin.json"))

  Gdx.input setInputProcessor stage

}