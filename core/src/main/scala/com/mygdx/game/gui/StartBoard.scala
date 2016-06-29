package com.mygdx.game.gui

import com.mygdx.game._
import com.mygdx.game.net.NetPanel
import priv.util.GuiUtils._

class StartBoard(screens : Screens) {

  val buttonPanel    = new ButtonPanel(screens.screenResources.skin2)
  val newGameButton  = buttonPanel.getButton("Single game")
  val settingsButton = buttonPanel.SettingsButton
  val netPanel       = new NetPanel(screens, buttonPanel)
  val panel          = column(buttonPanel.panel, netPanel.panel)

  panel.fill()
  panel.setFillParent(true)
  panel.pack()
}

