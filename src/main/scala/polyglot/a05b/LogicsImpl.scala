package polyglot.a05b

import util.Sequences.*
import scala.annotation.tailrec
import scala.util.Random

case class Pair(x: Int, y: Int)

object SeqMinMax:
  import Sequence.*
  extension (seq: Sequence[Int])
    @tailrec
    def min(m: Int = Int.MaxValue): Int = seq match
      case Cons(h, t) => t.min(if h < m then h else m)
      case _ => m
    @tailrec
    def max(m: Int = Int.MinValue): Int = seq match
      case Cons(h, t) => t.max(if h > m then h else m)
      case _ => m

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a05b/sol2/ */
class LogicsImpl(private val size: Int) extends Logics:
  private val start: Pair = Pair(Random.nextInt(size), Random.nextInt(size))
  private var _stars: Sequence[Pair] = Sequence(start)

  override def tick(): Unit =
    val bounds: Sequence[Pair] = boundingStars
    for
      i <- -1 to size
      j <- -1 to size
      if bounds hasNear Pair(i, j)
      if !_stars.contains(Pair(i, j))
      if isValid(Pair(i, j))
    yield
      _stars = _stars.add(Pair(i, j))

  override def isOver: Boolean =
    !_stars.filter(p => p.x < 0 || p.y < 0 || p.x == size || p.y == size).isEmpty()

  override def hasElement(x: Int, y: Int): Boolean = _stars.contains(Pair(x, y))

  private def isValid(cell: Pair): Boolean =
    cell.x == start.x || cell.y == start.y || (cell.x - start.x).abs == (cell.y - start.y).abs

  import SeqMinMax.*
  private def boundingStars: Sequence[Pair] =
    val (x, y) = (_stars.flatMap(p => Sequence(p.x)), _stars.flatMap(p => Sequence(p.y)))
    _stars.filter(p => p.x == x.max() || p.x == x.min() || p.y == y.max() || p.y == y.min())

  extension (seq: Sequence[Pair])
    private def hasNear(cell: Pair): Boolean =
      var neighbours: Sequence[Pair] = Sequence()
      for
        i <- (cell.x - 1) to (cell.x + 1)
        j <- (cell.y - 1) to (cell.y + 1)
        if Pair(i, j) != cell
      yield neighbours = neighbours.add(Pair(i, j))
      !seq.flatMap(e => Sequence(neighbours.contains(e))).find(_ == true).isEmpty