package polyglot.a01b

import polyglot.OptionToOptional
import util.Optionals.Optional as ScalaOptional
import util.Sequences.*

import scala.annotation.tailrec
import scala.util.Random
import scala.jdk.javaapi.OptionConverters

case class Pair(x: Int, y: Int)

trait Logics:
  def size: Int
  def mines: Int
  def hit(x: Int, y: Int): java.util.Optional[Integer]
  def won: Boolean

object Logics:
  def apply(size: Int, mines: Int): Logics = LogicsImpl(size, mines)

  import Sequence.*
  extension [A](seq: Sequence[A])
    @tailrec
    def size(n: Int = 0): Int = seq match
      case Cons(_, t) => t.size(n + 1)
      case _ => n

  /** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01b/sol2/ */
  private class LogicsImpl(override val size: Int, override val mines: Int) extends Logics:
    private var _cells: Sequence[Pair] = Sequence()
    private var _mines: Sequence[Pair] = Sequence()

    for
      i <- 0 until size
      j <- 0 until size
    do _cells = _cells.add(Pair(i, j))

    private var _m = Pair(-1, -1)
    for
      i <- 0 until mines
    do
      while (_mines.contains(_m) || _m == Pair(-1, -1))
        _m = Pair(Random.nextInt(size), Random.nextInt(size))
      _mines = _mines.add(_m)
    println(_mines)

    def hit(x: Int, y: Int): java.util.Optional[Integer] =
      if _mines.contains(Pair(x, y)) then
          OptionToOptional(ScalaOptional.Empty())
      else
        _cells = _cells.remove(Pair(x, y))
        OptionToOptional(ScalaOptional.Just(neighborsMines(Pair(x, y))))

    def won = _cells.size() == _mines.size()

    private def neighborsMines(cell: Pair): Int =
      var _neighbors: Sequence[Pair] = Sequence()
      for
        i <- (cell.x - 1) to (cell.x + 1)
        j <- (cell.y - 1) to (cell.y + 1)
        if (i >= 0) && (i < size) && (j >= 0) && (j < size)
        if (i, j) != (cell.x, cell.y)
      do _neighbors = _neighbors.add(Pair(i, j))
      _cells.filter(c => _neighbors.contains(c) && _mines.contains(c)).size()
