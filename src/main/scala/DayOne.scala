import cats.implicits.*
import cats.Monoid
import cats.Traverse
import cats.data.Const
import cats.instances._

import scala.io.Source
import scala.language.postfixOps
import scala.util.{Try, Using}

object DayOne:

  val countingFunctions: List[List[Int] => Int] = List(

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
        (readings zip readings.tail).map { (x,y) => if x < y then 1 else 0 } combineAll,

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

      // zip and count - seen in Jakub Kozłowski's 'Zip and slide! (Advent of Code day 1)' https://www.youtube.com/watch?v=AhcDxzjrUUI
      readings =>
        (readings zip readings.tail).count(_ < _),

      readings =>
        (Traverse[List].traverse(readings){ n => Const[CountIncrement,Any](CountIncrement(0,n,n))}).getConst.count
    )

  case class CountIncrement(count: Int, left: Int, right: Int)

  given countIncrementMonoid: Monoid[CountIncrement] =
    Monoid.instance[CountIncrement](
      CountIncrement(0,Int.MinValue,Int.MaxValue),
      (left: CountIncrement, right: CountIncrement) =>
        val inc = if (left.right < right.left) 1 else 0
        CountIncrement(left.count + right.count + inc, left.left, right.right)
    )

  @main def dayOnePart1: Unit =
    go(minReadings = 2)

  @main def dayOnePart2: Unit =
    go(preProcess, minReadings = 3)

  def go(preProcess: List[Int] => List[Int] = identity, minReadings: Int): Unit =
    tryToGetReadings map { readings =>
      if readings.size < minReadings
      then handleNotEnoughReadings(readings.size)
      else
        for
          (f,n) <- countingFunctions.zipWithIndex
          result = f(preProcess(readings))
        yield reportResult(n,result)
    } recover { handleErrorGettingReadings(_) }

  def preProcess(readings: List[Int]): List[Int] =
    (readings zip readings.tail zip readings.tail.tail) map { case ((x,y),z) => x + y + z }

  // sliding instead of zipping thrice
  // seen in Jakub Kozłowski's 'Zip and slide! (Advent of Code day 1)' https://www.youtube.com/watch?v=AhcDxzjrUUI
  def preProcess2(readings: List[Int]): List[Int] =
    readings.sliding(3).map(_.sum).toList

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