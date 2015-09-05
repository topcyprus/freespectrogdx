package com.mygdx.game.gui

import com.badlogic.gdx.scenes.scene2d.ui.{VerticalGroup, TextButton}
import com.mygdx.game.ScreenResources


class UserMenu(resources : ScreenResources) {
  val panel = new VerticalGroup()
  val surrenderButton = new TextButton("New game", resources.skin)
  val skipButton = new TextButton("Skip turn", resources.skin)
  val settingsButton = new TextButton("Settings", resources.skin)
  val restartButton = new TextButton("Restart", resources.skin)

  List(surrenderButton, skipButton, settingsButton, restartButton).foreach(panel.addActor)
}
