package polyglot.a05b

import polyglot.a05b.Logics
import util.Sequences.*
import scala.util.Random

case class Pair(x: Int, y: Int)

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a05b/sol2/ */
class LogicsImpl(private val size: Int) extends Logics:
  private val start: Pair = Pair(Random.nextInt(size), Random.nextInt(size))
  private var _stars: Sequence[Pair] = Sequence(start)

  override def tick(): Unit =
    var i = start.x
    var j = start.y
    while (_stars.contains(Pair(i, j)))
      i += 1
      j += 1
    _stars = _stars.add(Pair(i, start.y)).add(Pair(specularX(i), start.y))
      .add(Pair(start.x, j)).add(Pair(start.x, specularY(j)))
      .add(Pair(i, j)).add(Pair(i, specularY(j)))
      .add(Pair(specularX(i), j)).add(Pair(specularX(i), specularY(j)))

  override def isOver: Boolean =
    !_stars.filter(p => p.x < 0 || p.y < 0 || p.x == size || p.y == size).isEmpty()

  override def hasElement(x: Int, y: Int): Boolean = _stars.contains(Pair(x, y))

  private def specularX(i: Int): Int = start.x - (i - start.x)
  private def specularY(j: Int): Int = start.y - (j - start.y)
