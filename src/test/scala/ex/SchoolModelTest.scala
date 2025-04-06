package ex

import org.junit.*
import org.junit.Assert.*
import SchoolModel.*
import util.Sequences.*
import Sequence.*

class SchoolModelTest:

  val school = School()
  import school.*

  val john = Teacher("John")
  val math = Course("Math")
  val physics = Course("Physics")

  @Test def testCourses(): Unit =
    val school = School()
    val school2 = school.setTeacherToCourse(john, math)
    assertEquals(Sequence(), school.courses)
    assertEquals(Sequence(math), school2.courses)

  @Test def testTeachers(): Unit =
    val school = School()
    val school2 = school.setTeacherToCourse(john, math).setTeacherToCourse(john, physics)
    assertEquals(Sequence(), school.teachers)
    assertEquals(Sequence(john), school2.teachers)

  @Test def testSetTeacherToCourse(): Unit =
    val school2 = school.setTeacherToCourse(john, math)
    assertEquals(Sequence(john), school2.teachers)
    assertEquals(Sequence(math), school2.courses)

  @Test def testCoursesOfATeacher(): Unit =
    val school2 = school.setTeacherToCourse(john, math).setTeacherToCourse(john, physics)
    assertEquals(Sequence(math, physics), school2.coursesOfATeacher(john))

  @Test def testHasTeacher(): Unit =
    val school2 = school.setTeacherToCourse(john, math)
    assertTrue(school2.hasTeacher(john))
    assertFalse(school2.hasTeacher(Teacher("Mary")))

  @Test def testHasCourse(): Unit =
    val school2 = school.setTeacherToCourse(john, math)
    val history = Course("History")
    assertTrue(school2.hasCourse(math))
    assertFalse(school2.hasCourse(history))
