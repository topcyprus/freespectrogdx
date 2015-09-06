package com.mygdx.game

import com.badlogic.gdx.graphics.GL20
import com.badlogic.gdx.graphics.g2d.{TextureAtlas, SpriteBatch}
import com.badlogic.gdx.graphics.glutils.ShapeRenderer
import com.badlogic.gdx.scenes.scene2d.Stage
import com.badlogic.gdx.scenes.scene2d.ui.Skin
import com.badlogic.gdx.{Gdx, ScreenAdapter, Game}
import com.mygdx.game.effects._

import scala.util.control.NonFatal

class MyGdxGame extends Game {

  override def create() {
    setScreen(new GameScreen(this))
  }
}


class GameScreen(game :Game) extends ScreenAdapter {
  val screenResources = new ScreenResources
  val gameAdapter = new GameInit(screenResources)

  var lastE = Option.empty[Throwable]

  override def render (delta : Float): Unit = {
    import screenResources._
    Gdx.gl glClear GL20.GL_COLOR_BUFFER_BIT


    batch.begin()
    try {
      stage act delta
      stage.draw()
      lastE = None
    } catch { case NonFatal(e) =>
      if (lastE != Some(e)){
        lastE = Some(e)
        e.printStackTrace()
      }
    }
    batch.end()
  }
  override def resize (width : Int, height : Int) {
    screenResources.stage.getViewport.update(width, height, true)
  }
  override def dispose(): Unit ={
    screenResources.dispose()
  }

}

class ScreenResources {
  val batch = new SpriteBatch
  val stage = new Stage()
  val renderer = new ShapeRenderer()
  val atlas = new TextureAtlas(Gdx.files.internal("pack/images.pack.atlas"))
  val skin  = new Skin(Gdx.files.internal("data/uiskin.json"))

  Gdx.input setInputProcessor stage

  var shaders = new BaseShaders(new Shaders, this)

  def reload() = {
    val old = shaders
    shaders = new BaseShaders(new Shaders, this)
    old.shaders.clean()
  }

  def dispose(): Unit ={
    shaders.shaders.clean()
    batch.dispose()
    renderer.dispose()
    skin.dispose()
    atlas.dispose()
  }
}

class BaseShaders(val shaders: Shaders, resources: ScreenResources) {

  /**val hoverGlow = shaders.getOrElseUpdate("hoverglow", _ ⇒ new HoverShader("nz", resources))
  val fade = shaders.getOrElseUpdate("fade", _ ⇒ new FadeShader("fade"))
  val ripple = shaders.getOrElseUpdate("ripple", _ ⇒ new RippleShader)*/
  val grey = shaders get "grey"
  val selected = shaders.getOrElseUpdate("sel", _ ⇒ new SelectedShader("sel", 200))
}