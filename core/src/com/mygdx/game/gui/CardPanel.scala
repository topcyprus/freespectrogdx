package com.mygdx.game.gui

import com.badlogic.gdx.scenes.scene2d.Actor
import com.badlogic.gdx.scenes.scene2d.ui.Table
import com.mygdx.game._
import priv.sp._

class CardPanel(playerId: PlayerId, game: SpGame, resources : ScreenResources) {

  private val houseCardButtons = game.desc.players(playerId).houses.zipWithIndex map {
    case (houseDesc, idx) ⇒
      def getCard(i: Int) : Option[CardDesc] = {
        val cards = game.state.players(playerId).desc.get.houses(idx).cards
        if (i < cards.size) Some(cards(i)) else None
      }
      def getHouseState = game.state.players(playerId).houses(idx)
      new HouseLabel(getHouseState.mana, houseDesc.house, resources.skin) -> (0 to 3).map { i ⇒
        new CardButton(getCard(i), getHouseState, houseDesc, resources)
      }
  }
  val cardButtons = houseCardButtons flatMap (_._2)
  val houseLabels = houseCardButtons map (_._1)
  var lastSelected = Option.empty[CardButton]

  def init(commandRecorder : CommandRecorder) {
    if (playerId == game.myPlayerId) {
      cardButtons foreach { cardButton ⇒
        cardButton.visible = true
        cardButton.group.addListener(onClick {
          if (cardButton.isActive) {
            cardButton.cardActorsOption.foreach { h ⇒
              commandRecorder.setCommand(Command(game.myPlayerId, h.desc.card, None, h.desc.cost))
              if (h.desc.card.inputSpec.isDefined) {
                lastSelected.foreach(_.selected = false)
                cardButton.selected = true
                lastSelected = Some(cardButton)
              }
            }
          }
        })
      }
    }
  }


  val panel = new Table
  val houseActors : Seq[Actor] = houseCardButtons map { case (houseLabel, _) => houseLabel.label}
  val cardActors : List[Seq[Actor]] = (0 to 3).map { i =>
    val cards :Seq[CardButton] = houseCardButtons.map{_._2(i)}
    cards.map(_.group : Actor)
  }.toList
  val actorRows : List[Seq[Actor]] = if (playerId == game.myPlayerId) houseActors :: cardActors else cardActors :+ houseActors

  actorRows foreach { actors =>
    actors foreach (a => panel.add(a))
    panel.row()
  }

  panel.pack()

  def refresh(silent : Boolean): Unit ={
    //houseLabels foreach (_.mana.refresh(silent))
    cardButtons foreach (_.refresh())
    cardButtons foreach { cb ⇒
      cb.visible = (playerId == game.myPlayerId
      || (cb.cardActorsOption.isDefined && visibleCards.contains(cb.cardActorsOption.get.desc.card)))
    }
  }


  def setEnabled(flag: Boolean) {
    cardButtons foreach { btn ⇒
      btn.enabled = flag
      //lastSelected foreach (_.selected = false)
      //lastSelected = None
    }
  }

  var visibleCards = Set.empty[Card]
  def addVisibleCard(c: Card) {
    visibleCards = visibleCards + c
  }

/*
  val panel = Row(houseCardButtons.map {
    case (houseLabel, cardButons) ⇒
      Column(houseLabel :: cardButons.toList)
  })
  setEnabled(false)


  def getPositionOf(card: Card) = {
    val someCard = Some(card)
    cardButtons.find(_.card == someCard) map { cardButton ⇒
      cardButton.coord + (cardButton.size * 0.5)
    }
  }
  val specialCardButtons = houseCardButtons(4)._2
  def setEnabled(flag: Boolean) {
    cardButtons foreach { btn ⇒
      btn.enabled = flag
      lastSelected foreach (_.selected = false)
      lastSelected = None
    }
  }*/
}
/**
class TopCardPanel(playerId: PlayerId, game: Game) {
  val houseLabels = game.desc.players(playerId).houses.zipWithIndex map {
    case (houseDesc, idx) ⇒
      new HouseLabel(new DamagableInt(game.state.players(playerId).houses(idx).mana, game), houseDesc.house, game, flip = true)
  }
  val panel = Row(houseLabels)
  def refresh(silent: Boolean) { houseLabels.foreach(_.mana.refresh(silent)) }
}
*/
