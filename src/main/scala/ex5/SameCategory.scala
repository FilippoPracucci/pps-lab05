package ex5

import ex.*
import util.Sequences.*
import util.Sequences.Sequence.*

object SameCategory:
  def unapply(seq: Sequence[Course]): Option[String] = seq match
    case Cons(h, t) => if t.filter(_.category != h.category).isEmpty() then Some(h.category) else None
    case _ => None

@main def test: Unit =
  val scalaCourse = Course("SCALA01", "Functional Programming in Scala", "Prof. Odersky", "Programming")
  val pythonCourse = Course("PYTHON01", "Introduction to Python", "Prof. van Rossum", "Programming")
  val designCourse = Course("DESIGN01", "UI/UX Design Fundamentals", "Prof. Norman", "Design")
  val courses = Sequence(scalaCourse, pythonCourse, designCourse)
  val coursesSimilar = Sequence(pythonCourse, scalaCourse)
  val cat = "Programming"
  courses match
    case SameCategory(cat) => println(s"$courses have same category $cat")
    case _ => println(s"$courses have different categories")
  coursesSimilar match
    case SameCategory(cat) => println(s"$coursesSimilar have same category $cat")
    case _ => println(s"$courses have different categories")
