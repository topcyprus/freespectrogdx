package com.mygdx.game

import com.badlogic.ashley.core.{Entity, Engine}
import com.badlogic.gdx.graphics.Texture.TextureFilter
import com.badlogic.gdx.graphics.g2d.freetype.FreeTypeFontGenerator
import com.badlogic.gdx.graphics.g2d.freetype.FreeTypeFontGenerator.FreeTypeFontParameter
import com.mygdx.game.component._
import com.mygdx.game.gui.BasicAction
import priv.sp.GameResources

import collection.JavaConverters._
import com.badlogic.gdx.graphics.{Color, GL20}
import com.badlogic.gdx.graphics.g2d.{BitmapFont, Batch, TextureAtlas}
import com.badlogic.gdx.graphics.glutils.ShapeRenderer
import com.badlogic.gdx.scenes.scene2d.{Actor, Stage}
import com.badlogic.gdx.scenes.scene2d.ui.{Label, Skin}
import com.badlogic.gdx.{Gdx, ScreenAdapter, Game}
import com.mygdx.game.actor.Repere
import com.mygdx.game.effects._
import com.typesafe.config.ConfigFactory

import scala.concurrent.Promise
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

  /**
  Gdx.gl glClearDepthf 1
  Gdx.gl glClear (GL20.GL_COLOR_BUFFER_BIT | GL20.GL_DEPTH_BUFFER_BIT)
  Gdx.gl glEnable      GL20.GL_DEPTH_TEST
  Gdx.gl glDepthFunc   GL20.GL_LESS
  Gdx.gl glDepthMask   true
    */

  override def render (delta : Float): Unit = {
    import screenResources._
    try {
      beforeProcess()
      stage act delta

      Gdx.gl glClear GL20.GL_COLOR_BUFFER_BIT

      //      batch.setColor(Color.WHITE)
      stage.draw()
      engine update delta
      lastE = None
    } catch {
      case NonFatal(e) =>
        if (lastE != Some(e)) {
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
    screenResources.engine.getSystems.toArray.foreach(_.setProcessing(false))
    // FIXME should wait here
    screenResources.stage.clear()
    screenResources.stage addActor repere
    screenResources.engine.removeAllEntities()
    if (currentGame != null) {
      currentGame.spGame.gameLock.release()
      currentGame.listener.lock.release()
    }
    val gameInit = new GameInit(screenResources, gameResources)
    gameInit.userMenu.surrenderButton.clearListeners()
    gameInit.userMenu.surrenderButton.addListener(onClick {
      currentGame = createGame()
    })
    screenResources.engine.getSystems.toArray.foreach(_.setProcessing(true))
    gameInit
  }

}


class ScreenResources {

  var config   = loadConfig()
  val stage    = new Stage()
  val batch    = stage.getBatch
  val renderer = new ShapeRenderer()
  val atlas    = new TextureAtlas(Gdx.files.internal("pack/images.pack.atlas"))
  var skin     = loadSkin()
  val engine   = new Engine()
  val renderSystem   = new RenderSystem(batch, stage.getCamera)
  val scriptSystem   = new ScriptSystem()
  val particleSystem = new ParticleSystem(engine)
  val slotSystem     = new SlotSystem(batch)
  val timedSystem    = new TimedSystem(engine)

  engine addSystem scriptSystem
  engine addSystem timedSystem
  engine addSystem particleSystem
  engine addSystem slotSystem
  engine addSystem renderSystem

  val shapes = new ShapeRenderer()

  var effectResources = new EffectResources(new Shaders, this)
  val beforeProcess = new BeforeProcess

  atlas.getTextures.asScala.foreach(_.setFilter(TextureFilter.Linear, TextureFilter.Linear)) // smooth textures, to fix glitches when card moves and viewport resizing
  Gdx.input setInputProcessor stage

  def addEndMessage(msg : String) = beforeProcess invoke {
    val endMessageLabel = new Label(msg, skin)
    endMessageLabel.setPosition(10, 550)
    val endMessage = new Entity
    endMessage add new VisualComponent(new Drawable(){
      override def draw(batch: Batch): Unit = {
        endMessageLabel.draw(batch, 1)
      }
    })
    engine addEntity endMessage
  }

  def reload() = {
    try {
      ConfigFactory.invalidateCaches()
      config = loadConfig()
      val old = effectResources
      effectResources = new EffectResources(new Shaders, this)
      old.shaders.dispose()
      skin = loadSkin()
    } catch { case NonFatal(t) => t.printStackTrace() }
  }

  def dispose(): Unit ={
    effectResources.shaders.dispose()
    effectResources.particles.dispose()
    stage.dispose()
    batch.dispose()
    renderer.dispose()
    skin.dispose()
    atlas.dispose()
  }

  def loadConfig() = ConfigFactory.parseReader(Gdx.files.internal("application.conf").reader())
  def configure(actor : Actor, name : String) = {
    if (config hasPath name) {
      val c = config getConfig name
      if (c hasPath "pos") {
        val pos = (c getDoubleList "pos").asScala.toList.map(_.toFloat)
        if (pos.size == 2) actor.setPosition(pos(0), pos(1))
        else actor.setPosition(pos(0), pos(1), pos(2).toInt)
      }
      if (c hasPath "color") {
        val col = (c getDoubleList "color").asScala.toList.map(_.toFloat)
        val alpha = if (col.size == 4) col(3) else 1f
        actor.setColor(col(0), col(1), col(2), alpha)
      }
    }
  }

  def loadSkin() = {
    val font = generateFont()
    val skin = new Skin()
    skin addRegions new TextureAtlas(Gdx.files.internal("data/uiskin.atlas"))
    skin.add("default-font", font)
    skin load Gdx.files.internal("data/uiskin.json")
    skin
  }

  def generateFont() = {
    val generator = new FreeTypeFontGenerator(Gdx.files.internal(config getString "font.name"))
    val parameter = new FreeTypeFontParameter()
    val c = config getConfig "font"
    parameter.size          = c getInt "size"
    parameter.shadowOffsetX = c getInt "shadowOffsetX"
    parameter.shadowOffsetY = c getInt "shadowOffsetY"
    val font = generator generateFont parameter
    generator.dispose()
    font
  }
}

class EffectResources(val shaders: Shaders, resources: ScreenResources) {

  //val ripple   = shaders.getOrElseUpdate("ripple", _ ⇒ new RippleShader)
  val grey     = shaders get "grey"
  //val test     = shaders get "test"
  val repere   = shaders get "repere"
  val selected = shaders.getOrElseUpdate("sel", _ ⇒ new SelectedShader("sel", resources.config.getConfig("card.selection")))
  val particles = new MyParticleEffects(resources.atlas)
}


class BeforeProcess {
  @volatile private var task = Option.empty[() => Unit]
  private val taskLock = new Object

  def invoke[A](f : => A) : A = {
    val prom = Promise[A]()
    val func = () => {
      val res = f
      prom success res
      ()
    }
    taskLock synchronized {
      task = Some(func)
      taskLock.wait()
      prom.future.value.get.get
    }
  }

  def apply() = {
    taskLock synchronized {
      task foreach { func =>
        func()
        task = None
        taskLock.notifyAll()
      }
    }
  }
}