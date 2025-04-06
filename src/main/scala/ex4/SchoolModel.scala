package ex4

import util.Sequences.*

object SchoolModel:

  trait Teacher:
    def name: String

  object Teacher:
    def apply(name: String): Teacher = TeacherImpl(name)
    private case class TeacherImpl(override val name: String) extends Teacher

  trait Course:
    def name: String

  object Course:
    def apply(name: String): Course = CourseImpl(name)
    private case class CourseImpl(override val name: String) extends Course

  case class TeacherToCourse(teacher: Teacher, course: Course)

  trait School:
    def teachers: Sequence[Teacher]
    def teachers_=(teachers: Sequence[Teacher]): Unit
    def courses: Sequence[Course]
    def courses_=(courses: Sequence[Course]): Unit
    def teacherToCourse: Sequence[TeacherToCourse]
    def teacherToCourse_=(teacherToCourse: Sequence[TeacherToCourse]): Unit

    extension (school: School)
      /**
       * This method should return a new school with the teacher assigned to the course
       * e.g.,
       * emptySchool
       *   .setTeacherToCourse(teacher("John"), course("Math")) // => School(courses = Cons("Math", Nil()), teachers = Cons("John", Nil()), teacherToCourses = Cons(("John", "Math"), Nil()))
       *  */
      def setTeacherToCourse(teacher: Teacher, course: Course): School
      /**
       * This method should return the list of courses assigned to a teacher
       * e.g.,
       * emptySchool.coursesOfATeacher(teacher("John")) // => Nil()
       * emptySchool
       *   .setTeacherToCourse(teacher("John"), course("Math"))
       *   .coursesOfATeacher(teacher("John")) // => Cons("Math", Nil())
       * emptySchool
       *   .setTeacherToCourse(teacher("John"), course("Math"))
       *   .setTeacherToCourse(teacher("John"), course("Italian"))
       *   .coursesOfATeacher(teacher("John")) // => Cons("Math", Cons("Italian", Nil()))
       * @return the list of courses assigned to a teacher
       */
      def coursesOfATeacher(teacher: Teacher): Sequence[Course]
      /**
       * This method should return true if the teacher is present in the school
       * e.g.,
       * emptySchool.hasTeacher("John") // => false
       * emptySchool
       *  .setTeacherToCourse(teacher("John"), course("Math"))
       *  .hasTeacher("John") // => true
       *
       */
      def hasTeacher(teacher: Teacher): Boolean
      /**
       * This method should return true if the course is present in the school
       * e.g.,
       * emptySchool.hasCourse("Math") // => false
       * emptySchool
       *  .setTeacherToCourse(teacher("John"), course("Math"))
       *  .hasCourse("Math") // => true
       *
       */
      def hasCourse(course: Course): Boolean

  object School:
    def apply(): School = BasicSchool(Sequence(), Sequence(),Sequence())
    private case class BasicSchool(private var _teachers: Sequence[Teacher],
                                   private var _courses: Sequence[Course],
                                   private var _teacherToCourse: Sequence[TeacherToCourse]) extends School:
      import Sequence.*

      override def teachers: Sequence[Teacher] = _teachers
      override def teachers_=(teachers: Sequence[Teacher]): Unit = _teachers = teachers

      override def courses: Sequence[Course] = _courses
      override def courses_=(courses: Sequence[Course]): Unit = _courses = courses

      override def teacherToCourse: Sequence[TeacherToCourse] = _teacherToCourse
      override def teacherToCourse_=(teacherToCourse: Sequence[TeacherToCourse]): Unit =
        _teacherToCourse = teacherToCourse

      extension (school: School)
        def setTeacherToCourse(teacher: Teacher, course: Course): School =
          if !_teachers.contains(teacher) then this.teachers = _teachers.add(teacher)
          if !_courses.contains(course) then this.courses = _courses.add(course)
          this.teacherToCourse = _teacherToCourse.add(TeacherToCourse(teacher, course))
          this

        def coursesOfATeacher(teacher: Teacher): Sequence[Course] =
          _teacherToCourse.filter(_.teacher == teacher).map(_.course)

        def hasTeacher(teacher: Teacher): Boolean = _teachers.contains(teacher)

        def hasCourse(course: Course): Boolean = _courses.contains(course)

@main def examples(): Unit =
  import SchoolModel.*
  val school = School()
  import school.*
  println(school.teachers) // nil
  println(school.courses) // nil
  println(school.hasTeacher(Teacher("John"))) // false
  println(school.hasCourse(Course("Math"))) // false
  val john = Teacher("John")
  val math = Course("Math")
  val italian = Course("Italian")
  val school2 = school.setTeacherToCourse(john, math)
  println(school2.teachers) // John :: nil
  println(school2.courses) // Math :: nil
  println(school2.hasTeacher(john)) // true
  println(school2.hasCourse(math)) // true
  println(school2.hasCourse(italian)) // false
  val school3 = school2.setTeacherToCourse(john, italian)
  println(school3.teachers) // John :: nil
  println(school3.courses) // Math :: Italian :: nil
  println(school3.hasTeacher(john)) // true
  println(school3.hasCourse(math)) // true
  println(school3.hasCourse(italian)) // true
  println(school3.coursesOfATeacher(john)) // Math :: Italian :: nil
