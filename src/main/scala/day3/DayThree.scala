package day3

import cats.implicits.*
import day3.BitCounts
import day3.BitCounts.*

import scala.io.Source
import scala.language.postfixOps
import scala.util.{Try, Using}

  @main def main: Unit = {
    for
      diagnostics <- tryGettingDiagnosticsFrom(diagnosticsFileName)
      aggregatedDiagnostics <- tryAggregating(diagnostics)
      gammaRate <- aggregatedDiagnostics.gammaRate
      epsilonRate <- aggregatedDiagnostics.epsilonRate
      powerConsumption = gammaRate * epsilonRate
    yield reportResult(powerConsumption)
  } recover( handleFailure(_) )

  def tryGettingDiagnosticsFrom(diagnosticsFileName: String): Try[List[BitCounts]] =
    for
      lines <- tryReadingLinesFrom(diagnosticsFileName)
      diagnostics <- tryParsingDiagnosticsIn(lines)
    yield diagnostics

  def tryAggregating(diagnostics: List[BitCounts]): Try[BitCounts] =
    Try {
      diagnostics.size match {
        case 0 => throw new IllegalArgumentException("expected: 1 or more diagnostic numbers; actual: none.")
        case 1 => diagnostics.head
        case other => diagnostics reduce { _ combine _ }
      }
    }

  def tryReadingLinesFrom(fileName: String): Try[List[String]] =
    Using( Source.fromFile(fileName) ) { bufferedSurce =>
      bufferedSurce.getLines.toList
    }

  def tryParsingDiagnosticsIn(lines: List[String]): Try[List[BitCounts]] =
    lines traverse( BitCounts(_) )

  val diagnosticsFileName = "day-3-input.txt"

  def handleFailure(throwable: Throwable): Unit =
    println(s"ERROR: Could not get diagnostics due to the following exception: ${throwable.getMessage}")

  def reportResult(powerConsumption: Int): Unit =
    println(s"The power consumption of the submarine is $powerConsumption.")
