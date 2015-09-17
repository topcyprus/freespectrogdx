package com.mygdx.game.gui

import com.badlogic.gdx.scenes.scene2d.ui.{HorizontalGroup, VerticalGroup, TextButton}
import com.mygdx.game.ScreenResources


class UserMenu(resources : ScreenResources) {
  val panel = new HorizontalGroup()
  val surrenderButton = new TextButton("New", resources.skin)
  val skipButton = new TextButton("Skip", resources.skin)
  val settingsButton = new TextButton("Settings", resources.skin)
  val restartButton = new TextButton("Restart", resources.skin)

  List(surrenderButton, restartButton, settingsButton, skipButton).foreach(panel.addActor)
}
