package com.mygdx.game

import com.badlogic.gdx.graphics.Texture.TextureFilter
import priv.sp.GameResources

import collection.JavaConverters._
import com.badlogic.gdx.graphics.{Color, GL20}
import com.badlogic.gdx.graphics.g2d.TextureAtlas
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
  val gameResources = new GameResources
  val repere = new Repere(screenResources)
  var lastE  = Option.empty[Throwable]
  var currentGame = createGame()

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
    gameResources.dispose()
  }

  def createGame(): GameInit ={
    screenResources.stage.clear()
    screenResources.stage addActor repere
    if (currentGame != null) {
      currentGame.spGame.gameLock.release()
    }
    val gameInit = new GameInit(screenResources, gameResources)
    gameInit.userMenu.surrenderButton.addListener(onClick {
      currentGame = createGame()
    })
    gameInit
  }

}

class ScreenResources {
  val stage    = new Stage()
  val batch    = stage.getBatch
  val renderer = new ShapeRenderer()
  val atlas    = new TextureAtlas(Gdx.files.internal("pack/images.pack.atlas"))
  val skin     = new Skin(Gdx.files.internal("data/uiskin.json"))

  lazy val shapes = new ShapeRenderer()

  var config   = loadConfig()
  var effectResources = new EffectResources(new Shaders, this)

  atlas.getTextures.asScala.foreach(_.setFilter(TextureFilter.Linear, TextureFilter.Linear)) // smooth textures, to fix glitches when card moves and viewport resizing
  Gdx.input setInputProcessor stage

  def reload() = {
    try {
      ConfigFactory.invalidateCaches()
      config = loadConfig()
      val old = effectResources
      effectResources = new EffectResources(new Shaders, this)
      old.shaders.clean()
    } catch { case NonFatal(t) => t.printStackTrace() }
  }

  def dispose(): Unit ={
    effectResources.shaders.clean()
    stage.dispose()
    batch.dispose()
    renderer.dispose()
    skin.dispose()
    atlas.dispose()
  }

  def loadConfig() = ConfigFactory.parseFile(Gdx.files.internal("application.conf").file())
}

class EffectResources(val shaders: Shaders, resources: ScreenResources) {

  /**val hoverGlow = shaders.getOrElseUpdate("hoverglow", _ ⇒ new HoverShader("nz", resources))
  val fade = shaders.getOrElseUpdate("fade", _ ⇒ new FadeShader("fade"))
  val ripple = shaders.getOrElseUpdate("ripple", _ ⇒ new RippleShader)*/
  val grey     = shaders get "grey"
  val test     = shaders get "test"
  val repere   = shaders get "repere"
  val selected = shaders.getOrElseUpdate("sel", _ ⇒ new SelectedShader("sel", resources.config.getConfig("card.selection")))
}