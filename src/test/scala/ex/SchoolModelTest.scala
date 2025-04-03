package ex

import org.junit.*
import org.junit.Assert.*
import SchoolModel.*
import util.Sequences.*
import Sequence.*

class SchoolModelTest:

  val schoolModel: SchoolModule = SchoolModule()
  import schoolModel.*

  val school = emptySchool

  val john = teacher("John")
  val math = course("Math")
  val physics = course("Physics")

  @Test def testCourses(): Unit =
    val school = schoolModel.emptySchool
    val school2 = school.setTeacherToCourse(john, math)
    assertEquals(Nil(), school.courses())
    assertEquals(Cons("Math", Nil()), school2.courses())

  @Test def testTeachers(): Unit =
    val school = schoolModel.emptySchool
    val school2 = school.setTeacherToCourse(john, math).setTeacherToCourse(john, physics)
    assertEquals(Nil(), school.teachers())
    assertEquals(Cons("John", Nil()), school2.teachers())

  @Test def testSetTeacherToCourse(): Unit =
    val school2 = school.setTeacherToCourse(john, math)
    assertEquals(Cons("John", Nil()), school2.teachers())
    assertEquals(Cons("Math", Nil()), school2.courses())

  @Test def testCoursesOfATeacher(): Unit =
    val school2 = school.setTeacherToCourse(john, math).setTeacherToCourse(john, physics)
    assertEquals(Cons("Math", Cons("Physics", Nil())), school2.coursesOfATeacher(john))

  @Test def testHasTeacher(): Unit =
    val school2 = school.setTeacherToCourse(john, math)
    assertTrue(school2.hasTeacher("John"))
    assertFalse(school2.hasTeacher("Mary"))

  @Test def testHasCourse(): Unit =
    val school2 = school.setTeacherToCourse(john, math)
    assertTrue(school2.hasCourse("Math"))
    assertFalse(school2.hasCourse("History"))
