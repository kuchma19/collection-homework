import scala.io.Source
import scala.util.{Try, Using}
import scala.util.matching.Regex

@main
def main(): Unit = {
  val studentsTry: Try[Seq[(String, Int)]] = Using(Source.fromFile("marks.txt")) { file =>
    val regexp: Regex = "(\\w+):(\\d+)".r

    def lineToStudent(line: String): Option[(String, Int)] =
      line match
        case regexp(name, markStr) => for {
          mark <- markStr.toIntOption
        } yield (name, mark)
        case _ => None

    file.getLines().map(lineToStudent).collect {
      case Some(value) => value
    }.toSeq
  }

  def sumMarks(students: Seq[(String, Int)]): Map[String, Int] =
    students.groupMapReduce(_._1)(_._2)(_ + _)

  def sortSumMarks(students: Seq[(String, Int)]): Seq[Int] =
    sumMarks(students).values.toSeq.sorted

  def studentsToMapWith[A](students: Seq[(String, Int)])(f: Seq[Int] => A): Map[String, A] = {
    students.groupMap(_._1)(_._2).map {
      case (name, marks) => (name, f(marks))
    }
  }

  def meanMarks(students: Seq[(String, Int)]): Map[String, Double] = {
    studentsToMapWith(students) { marks =>
      (BigDecimal(marks.sum) / BigDecimal(marks.length)).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
    }
  }

  def sumAllMarks(students: Seq[(String, Int)]): Long = students.map(_._2.toLong).sum

  def medianMarks(students: Seq[(String, Int)]): Map[String, Double] = {
    studentsToMapWith(students) { marks =>
      val sortedMarks = marks.sorted
      val lenMarks = marks.length
      val medianIndex = lenMarks / 2
      if (lenMarks % 2 == 1) {
        sortedMarks(medianIndex).toDouble
      } else {
        (sortedMarks(medianIndex) + sortedMarks(medianIndex + 1)).toDouble / 2.0
      }
    }
  }

  def theMostPopularMark(students: Seq[(String, Int)]): Int = {
    students.map(_._2).groupMapReduce(identity)(_ => 1)(_ + _).maxBy(_._2)._1
  }

  studentsTry.foreach { students =>
    println("Сумма баллов по фамилии:")
    pprint.pprintln(sumMarks(students))

    println("\nОтсортированный список по убыванию суммы баллов:")
    pprint.pprintln(sortSumMarks(students))

    println("\nСредний балл по студенту (с точностью как минимум до 2 знаков после запятой):")
    pprint.pprintln(meanMarks(students))

    println("\nСумма всех баллов: " + sumAllMarks(students))

    println("\nМедана по выставленным баллам:")
    pprint.pprintln(medianMarks(students))

    println("\nСамый часто выставленный балл: " + theMostPopularMark(students))

  }

}