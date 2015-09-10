package com.mygdx.game

import com.badlogic.gdx.graphics.{Color, OrthographicCamera, GL20}
import com.badlogic.gdx.graphics.g2d.{TextureAtlas, SpriteBatch}
import com.badlogic.gdx.graphics.glutils.ShapeRenderer
import com.badlogic.gdx.scenes.scene2d.Stage
import com.badlogic.gdx.scenes.scene2d.ui.Skin
import com.badlogic.gdx.{Gdx, ScreenAdapter, Game}
import com.mygdx.game.actor.Repere
import com.mygdx.game.effects._
import com.typesafe.config.ConfigFactory

import scala.util.control.NonFatal

class MyGdxGame extends Game {

  override def create() {
    setScreen(new GameScreen(this))
  }
}


class GameScreen(game :Game) extends ScreenAdapter {
  val screenResources = new ScreenResources
  val gameAdapter = new GameInit(screenResources)
  val repere = new Repere(screenResources)
  var lastE  = Option.empty[Throwable]

  screenResources.stage addActor repere


  init()

  def init(){
    import GL20._
    Gdx.gl.glDisable(GL_DEPTH_TEST)
    Gdx.gl.glDisable(GL20.GL_CULL_FACE)
    Gdx.gl.glEnable(GL_BLEND)
    Gdx.gl.glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    Gdx.gl.glClearColor(0f, 0f, 0f, 0f)
  }

  override def render (delta : Float): Unit = {
    import screenResources._
    Gdx.gl glClear GL20.GL_COLOR_BUFFER_BIT

    try {
      stage act delta

      batch.setColor(Color.WHITE)
      stage.draw()
      lastE = None
    } catch { case NonFatal(e) =>
      if (lastE != Some(e)){
        lastE = Some(e)
        e.printStackTrace()
      }
    }
  }

  override def resize (width : Int, height : Int) {
    screenResources.stage.getViewport.update(width, height, true)
  }
  override def dispose(): Unit ={
    screenResources.dispose()
  }

}

class ScreenResources {
  val stage    = new Stage()
  val batch    = stage.getBatch
  //stage.getViewport.setCamera(new OrthographicCamera(Gdx.graphics.getWidth(), Gdx.graphics.getHeight()))
  stage.getViewport.getCamera.asInstanceOf[OrthographicCamera].setToOrtho(false,Gdx.graphics.getWidth(), Gdx.graphics.getHeight())

  val renderer = new ShapeRenderer()
  val atlas    = new TextureAtlas(Gdx.files.internal("pack/images.pack.atlas"))
  val skin     = new Skin(Gdx.files.internal("data/uiskin.json"))

  var config   = loadConfig()
  var shaders = new BaseShaders(new Shaders, this)

  Gdx.input setInputProcessor stage

  def reload() = {
    try {
      ConfigFactory.invalidateCaches()
      config = loadConfig()
      val old = shaders
      shaders = new BaseShaders(new Shaders, this)
      old.shaders.clean()
    } catch { case NonFatal(t) => t.printStackTrace() }
  }

  def dispose(): Unit ={
    shaders.shaders.clean()
    stage.dispose()
    batch.dispose()
    renderer.dispose()
    skin.dispose()
    atlas.dispose()
  }

  def loadConfig() = ConfigFactory.parseFile(Gdx.files.internal("application.conf").file())
}

class BaseShaders(val shaders: Shaders, resources: ScreenResources) {

  /**val hoverGlow = shaders.getOrElseUpdate("hoverglow", _ ⇒ new HoverShader("nz", resources))
  val fade = shaders.getOrElseUpdate("fade", _ ⇒ new FadeShader("fade"))
  val ripple = shaders.getOrElseUpdate("ripple", _ ⇒ new RippleShader)*/
  val grey     = shaders get "grey"
  val test     = shaders get "test"
  val repere   = shaders get "repere"
  val selected = shaders.getOrElseUpdate("sel", _ ⇒ new SelectedShader("sel", resources.config.getConfig("shaders.sel")))
}