package com.mygdx.game

import com.badlogic.gdx.scenes.scene2d.{Action, Actor}
import com.badlogic.gdx.scenes.scene2d.ui.{WidgetGroup, VerticalGroup, HorizontalGroup}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Promise}
import scala.util.Success


package object gui {

  def row(actors : Actor*) = {
    group(new HorizontalGroup(), actors : _*)
  }

  def column(actors : Actor*)= {
    group(new VerticalGroup(), actors : _*)
  }

  case class UpdateAction[A <: Actor](f : A => Unit) extends Action {

    def act(delta : Float) = {
      f(target.asInstanceOf[A])
      true
    }
  }

  object BasicAction {
    def apply(f : => Unit) = new BasicAction(f)
  }
  class BasicAction(f : => Unit) extends Action {
    def act(delta : Float) = {
      f
      true
    }
  }

  def waitAction(actor : Actor) = {
    Await.result(promAction(actor).future, Duration.Inf)
  }

  def promAction(actor : Actor) : Promise[Unit] = {
    val prom = new PromiseAction
    actor addAction prom
    prom.prom
  }

  class PromiseAction extends Action {
    val prom = Promise[Unit]()

    def act(delta : Float) = {
      prom.complete(Success(()))
      true
    }
  }


  private def group[G <: WidgetGroup](g : G, actors : Actor*) = {
    actors foreach g.addActor
    g.pack()
    g
  }
}
