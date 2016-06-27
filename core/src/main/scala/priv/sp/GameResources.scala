package priv.sp

import java.util.concurrent._

class GameResources {
  val sp           = new SpWorld
  val aiExecutor   = Executors.newSingleThreadExecutor
  val gameExecutor = Executors.newSingleThreadExecutor
  var ended        = false

  val base = sp.houses.others ++ sp.houses.sinist
  val baseEnemy = base
  var playerChoices: List[List[House]] = List(base, baseEnemy)
  def resolveChoices(choices : List[List[House]] = playerChoices) = choices map { o ⇒
    val l = if (o.isEmpty) sp.houses.special else o
    l(scala.util.Random.nextInt(l.size))
  }

  def dispose() {
    ended = true
    println("releasing resources")
    aiExecutor.shutdown()
    gameExecutor.shutdown()
  }
}


