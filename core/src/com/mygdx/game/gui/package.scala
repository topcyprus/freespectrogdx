package com.mygdx.game

import com.badlogic.gdx.scenes.scene2d.Actor
import com.badlogic.gdx.scenes.scene2d.ui.{WidgetGroup, VerticalGroup, HorizontalGroup}


package object gui {

  def row(actors : Actor*) = {
    group(new HorizontalGroup(), actors : _*)
  }

  def column(actors : Actor*)= {
    group(new VerticalGroup(), actors : _*)
  }

  private def group[G <: WidgetGroup](g : G, actors : Actor*) = {
    actors foreach g.addActor
    g.pack()
    g
  }
}
