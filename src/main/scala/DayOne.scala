import cats.*
import cats.implicits.*
import cats.syntax.*

import scala.io.Source
import scala.language.postfixOps
import scala.util.{Failure, Success, Try, Using}

object DayOne:

  val readingsFileName = "day-1-input.txt"

  def readLines(fileName: String): Try[List[String]] =
    Using( Source.fromFile(fileName) ) { bufferedSurce =>
      bufferedSurce.getLines.toList
    }

  def parseReadings(lines: List[String]): Try[List[Int]] =
    Try { lines map(_ toInt) }

  def tryToGetReadings: Try[List[Int]] =
    for
      lines <- readLines(readingsFileName)
      readings <- parseReadings(lines)
    yield readings

  def handleErrorGettingReadings(throwable: Throwable): Unit =
    println(s"ERROR: could not get readings due to the following exception: ${throwable.getMessage}")

  def handleNotEnoughReadings(numberOfReadings: Int): Unit =
    println(s"ERROR - expected: 3 or more readings; actual: $numberOfReadings readings.")

  def reportResult(functionNumber: Int, result: Int): Unit =
    println(s"counting function number $functionNumber returned $result")

  val countingFunctions: List[List[Int] => Int] =
    List(

      // zip, filter and size
      readings =>
        readings.zip(readings.tail).filter(_ < _).size,

      // zip, map and sum
      readings =>
        readings.zip(readings.tail).map((x,y) => if (x < y) then 1 else 0).sum,

      // sliding, filter and size
      readings =>
        readings.sliding(2).filter{ case List(x,y) => x < y }.size,

      // foldLeft
      readings =>
        readings.tail.foldLeft((readings.head,0)) {
          case ((x,n),y) => if x < y then (y,n + 1) else (y,n)
        }(1),

      // zip, map and combineAll* (starred methods are provided by cats)
      readings =>
        readings.zip(readings.tail).map { (x,y) => if x < y then 1 else 0 }.combineAll,

      // zip, foldMap* and orEmpty*
      readings =>
        (readings zip readings.tail) foldMap { (x,y) => Option.when(x < y)(1) } orEmpty,

      // zip and foldMap*
      readings =>
        (readings zip readings.tail).foldMap { (x,y) => if x < y then 1 else 0 },

      // zipWith*, reduce* and orEmpty*
      readings =>
        ( for
            all <- readings.toNel
            rest <- all.tail.toNel
          yield (all zipWith rest) { (x,y) => if x < y then 1 else 0 } reduce
        ) orEmpty,

      // zip and count - Jakub Kozłowski
      readings =>
        (readings zip readings.tail).count(_ < _)
    )

  def preProcess(readings: List[Int]): List[Int] =
    (readings zip readings.tail zip readings.tail.tail) map { case ((x,y),z) => x + y + z }

  // Jakub Kozłowski
  def preProcess2(readings: List[Int]): List[Int] =
    readings.sliding(3).map(_.sum).toList

  @main def dayOneMainPart1: Unit =
    tryToGetReadings map {
      case Nil => handleNotEnoughReadings(0)
      case List(_) => handleNotEnoughReadings(1)
      case readings =>
        for
          (f,n) <- countingFunctions.zipWithIndex
          result = f(readings)
        yield reportResult(n,result)
    } recover { handleErrorGettingReadings(_) }

  @main def dayOneMainPart2: Unit =
    tryToGetReadings map {
      case Nil => handleNotEnoughReadings(0)
      case List(_) => handleNotEnoughReadings(1)
      case List(_,_) => handleNotEnoughReadings(2)
      case readings =>
        for
        (f,n) <- countingFunctions.zipWithIndex
      result = f(preProcess(readings))
      yield reportResult(n,result)
    } recover { handleErrorGettingReadings(_) }