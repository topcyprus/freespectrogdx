package com.mygdx.game.effects

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.VertexAttributes.Usage
import com.badlogic.gdx.graphics.{VertexAttribute, Mesh, Color}
import com.typesafe.config.Config

import collection._
import com.badlogic.gdx.graphics.glutils.ShaderProgram
import priv.util.{Utils, GBufferUtils, ResourceCache}
import com.mygdx.game.ScreenResources

class Shaders extends ResourceCache[String, Shader] {

  def load(path: String) = resources.getOrElseUpdate(path, new BasicShader(path))
  def clean() = resources.values foreach (res ⇒ res.program.dispose())

  class BasicShader(name: String) extends Shader {
    val program = Shader create name
  }
}

object Shader {

  def create(name: String) = {
    val program = new ShaderProgram(
      Gdx.files.internal(name + ".vert"),
      Gdx.files.internal(name + ".frag"))
    if(!program.isCompiled()) {
      Gdx.app.log("Problem loading shader " + name + ":", program.getLog());
    }
    program
  }
}

trait Shader {
  var pedantic = true

  def program: ShaderProgram

  def used[A](f: ⇒ A) = {
    ShaderProgram.pedantic = pedantic
    program.begin()
    try f finally program.end()
  }


  def getUniformLocations(names: String*): List[Int] = names.map(getUniformLocation(_))(breakOut)
  def getUniformLocation(name: String) = {
    val res = program.getUniformLocation(name)
    require(res != -1, name + " doesn't exists")
    res
  }
}

class HoverShader(name: String, resources: ScreenResources) extends Shader {
  val program = Shader create name
  val grad :: width :: height :: cursor :: _ = getUniformLocations("grads", "width", "height", "cursor")
  val fbuffer = GBufferUtils createFloatBuffer 200
  val texture = resources.atlas.findRegion("combat/glow")

  fillBuffer(fbuffer)
  used {
    program.setUniform2fv(grad, fbuffer.array(), 0, 200)
    program.setUniformf(width, texture.getRegionWidth.toFloat)
    program.setUniformf(height, texture.getRegionHeight.toFloat)
  }

  private def fillBuffer(fbuffer: java.nio.FloatBuffer) {
    //useless cr*p
    def radial(cx: Int, cy: Int)(x: Int, y: Int, vx: Float, vy: Float): (Float, Float) = {
      @inline def pow2(v: Int) = v * v

      val dist = pow2(x - cx) + pow2(y - cy)
      val fac = 2f / (1 + dist / 10)
      (vx * fac, vy * fac)
    }

    val arr = Array.fill(100)(Utils.floatRand(math.Pi * 2))
    val rad = radial(5, 5) _
    for (i ← 0 until arr.length) {
      val y = i / 10
      val x = i - 10 * y
      val (gx, gy) = rad(x, y, math.cos(arr(i)).floatValue, math.sin(arr(i)).floatValue)
      fbuffer put gx
      fbuffer put gy
    }
    fbuffer.rewind()
  }
}

class SelectedShader(name: String, config : Config) extends Shader {
  pedantic = false
  val program = Shader create name
  val size :: cursor :: _ = getUniformLocations("size", "cursor")

  val animLength = config getInt "animlength"
  val sizeConfig = config getInt "size"
  val offsetx = config getInt "offset.x"
  val offsety = config getInt "offset.y"

  val mesh = createPoliQuad(offsetx, offsety, sizeConfig, sizeConfig)

  def createPoliQuad(x : Float, y : Float, w : Float, h : Float) = {
    val verts = Array[Float](
      x    , y + h, Color.toFloatBits(255, 255, 255, 128),
      x    , y    , Color.toFloatBits(255, 255, 255, 128),
      x + w, y + h, Color.toFloatBits(255, 255, 255, 128),
      x + w, y    , Color.toFloatBits(255, 255, 255, 128))


    val mesh = new Mesh( true, 4, 4,  // static mesh with 6 vertices and no indices
      new VertexAttribute( Usage.Position, 2, ShaderProgram.POSITION_ATTRIBUTE ),
      new VertexAttribute( Usage.ColorPacked, 4, ShaderProgram.COLOR_ATTRIBUTE ))

    mesh.setVertices( verts )
    mesh.setIndices(Array[Short]( 0, 1, 2, 3 ))
    mesh
  }

}

class FadeShader(name: String) extends Shader {
  val program = Shader create name
  val fact = getUniformLocation("fact")
}

class RippleShader extends Shader {
  val name = "ripple"
  val program = Shader create name
  val cursor = getUniformLocation("cursor")
}