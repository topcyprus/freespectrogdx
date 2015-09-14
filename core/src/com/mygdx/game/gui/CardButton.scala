package com.mygdx.game.gui

import com.badlogic.gdx.graphics.glutils.ShapeRenderer.ShapeType
import com.badlogic.gdx.graphics.{Color, GL20}
import com.badlogic.gdx.graphics.g2d.Batch
import com.badlogic.gdx.math.Matrix4
import com.badlogic.gdx.scenes.scene2d.{Actor, Group}
import com.mygdx.game.ScreenResources
import priv.sp._

case class CardButtonActors(desc : CardDesc, houseState : HouseState, cardActors : CardActors) {
  cardActors.costLabel.setText(desc.cost.toString)
  def isActive = desc isAvailable houseState
}

class CardButton(getDesc: ⇒ Option[CardDesc],
                 getHouseState: ⇒ HouseState,
                 houseDesc : PlayerHouseDesc,
                 resources : ScreenResources)  {

  var cardActorsOption = getCardActors

  var visible  = false
  var enabled  = false
  var selected = false
  var hovered  = false
  def isActive = cardActorsOption.exists(_.isActive && enabled)

  val group = new Group {

    override def draw(batch: Batch, parentAlpha: Float) = {
      if (visible) {
        if (!isActive && visible) {
          batch.setShader(resources.effectResources.grey.program)
        }
        try {
          super.draw(batch, parentAlpha)
        } finally {
          if (batch.getShader != null) batch.setShader(null)
        }
      }
    }

  }

  val raka = resources.atlas findRegion "combat/raka"

  group.setSize(raka.getRegionWidth, raka.getRegionHeight)
  group.setBounds(0, 0, raka.getRegionWidth, raka.getRegionHeight)

  refresh()

  def refresh() = {
    group.addAction(
      BasicAction {
        group.clearChildren()
        if (visible) {
          cardActorsOption = getCardActors
          cardActorsOption foreach { cardActors =>
            group addActor selectEffectActor
            cardActors.cardActors.actors foreach group.addActor
          }
        }
      })
  }

  private def getCardActors = getDesc.map(d ⇒ CardButtonActors(d, getHouseState, new CardActors(d.card, houseDesc.house, resources)))

  private val selectEffectActor = new Actor with StaticAbsoluteProjMatrix {
    var time = 0f
    val seletectedShader = resources.effectResources.selected

    // matrix for selection shader
    var absoluteSelProjMatrix : Matrix4 = _

    override def draw(batch: Batch, parentAlpha: Float) = {
      if (visible) {
        init(batch)
        if (selected) {
          import seletectedShader._
          val deltax = time * 10
          val animationCursor = deltax % animLength

          program.begin()
          program.setUniformf(cursor, animationCursor)
          program.setUniformf(size, seletectedShader.sizeConfig)
          program.setUniformMatrix("u_projTrans", absoluteSelProjMatrix)
          mesh.render(program, GL20.GL_TRIANGLE_STRIP)
          program.end()
          batch.setShader(null) // why the fuck
        } else if (isActive && hovered) {
// todo
        }
      } else if (cardActorsOption.isDefined) {
        import resources.shapes
        shapes.begin(ShapeType.Filled)
        shapes.setProjectionMatrix(absoluteProjMatrix)
        shapes.setColor(Color.DARK_GRAY)
        shapes.rect(0, 0, 85 ,97)
        shapes.end()
        shapes.flush()
        batch.setShader(null)
      }
    }

    override def initialize(batch : Batch): Unit = {
      super.initialize(batch)
      absoluteSelProjMatrix = absoluteProjMatrix.translate(seletectedShader.offsetx, seletectedShader.offsety, 0f)
    }

    override def act(delta : Float): Unit = {
      super.act(delta)
      time += delta
    }
  }

  /**
  var holder = getDesc.map(d ⇒ new CardHolder(d, getHouseState))
  val size = holder.map(_.borderTex).getOrElse(sp.baseTextures.borderTex).size
  private var hovered = false
  private val grey = sp.shaders get "grey"
  private val hoverGlow = sp.baseShaders.hoverGlow
  private val selectedGlow = sp.baseShaders.selectedGlow("selcard", 200)
  var selected = false

  def card = holder.map(_.desc.card)
  def isActive = holder.exists(_.isActive)

  class CardHolder(val desc: CardDesc, houseState: HouseState) {
    val cardTex = sp.textures.get("Images/Cards/" + desc.card.image)
    val borderTex = sp.baseTextures getBorder desc.card
    def isActive = (desc isAvailable houseState) && enabled
  }

  def refresh() {
    holder = getDesc.map(d ⇒ new CardHolder(d, getHouseState))
  }*/

  /**
  def render() {
    if (visible) {
      holder.map { h ⇒
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)

        val isActive = h.isActive
        glColor4f(1, 1, 1, 1)

        if (!isActive) {
          grey.begin()
        } else if (isActive && hovered && !selected) {
          glPushMatrix()
          glTranslatef(-5, -5, 0)
          hoverGlow.used {
            val deltax = getDelta() / 100f
            val animLength = 50
            val animationCursor = deltax.intValue % animLength
            glUniform1i(hoverGlow.cursor, animationCursor)
            tex.draw(sp.baseTextures.cardGlow)
          }
          glPopMatrix()
        }
      }
    }
  }
  on {
    case MouseMoved(_) ⇒
      game.descriptionPanel.describedOption = holder.map(_.desc.card)
      hovered = true
    case MouseLeaved(_) ⇒
      game.descriptionPanel.describedOption = None
      hovered = false
  }*/

}
