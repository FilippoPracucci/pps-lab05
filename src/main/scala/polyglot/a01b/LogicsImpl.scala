package polyglot.a01b

import polyglot.OptionToOptional
import util.Optionals.Optional as ScalaOptional
import util.Sequences.*

import scala.annotation.tailrec
import scala.util.Random
import scala.jdk.javaapi.OptionConverters

case class Cell(x: Int, y: Int)

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
    private var _cells: Sequence[Cell] = Sequence()
    private var _mines: Sequence[Cell] = Sequence()

    for
      i <- 0 until size
      j <- 0 until size
    do _cells = _cells.concat(Sequence(Cell(i, j)))

    private var _m = Cell(-1, -1)
    for
      i <- 0 until mines
    do
      while (_mines.contains(_m) || _m == Cell(-1, -1))
        _m = Cell(Random.nextInt(size), Random.nextInt(size))
      _mines = _mines.concat(Sequence(_m))
    println(_mines)

    def hit(x: Int, y: Int): java.util.Optional[Integer] =
      if _mines.contains(Cell(x, y)) then
          OptionToOptional(ScalaOptional.Empty())
      else
        _cells = _cells.remove(Cell(x, y))
        OptionToOptional(ScalaOptional.Just(neighborsMines(Cell(x, y))))

    def won = _cells.size() == _mines.size()

    private def neighborsMines(cell: Cell): Int =
      _cells.filter(c => (c.x == cell.x + 1 && c.y == cell.y) || (c.x == cell.x - 1 && c.y == cell.y) ||
        (c.x == cell.x && c.y == cell.y + 1) || (c.x == cell.x && c.y == cell.y - 1) ||
        (c.x == cell.x + 1 && c.y == cell.y + 1) || (c.x == cell.x + 1 && c.y == cell.y - 1) ||
        (c.x == cell.x - 1 && c.y == cell.y + 1) || (c.x == cell.x - 1 && c.y == cell.y - 1))
        .filter(c => _mines.contains(c)).size()
