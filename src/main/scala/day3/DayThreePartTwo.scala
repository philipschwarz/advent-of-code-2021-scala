package day3.part2

import day3.common.*
import day3.common.BitCounts._

import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

  @main def day3Part2: Unit = {
    for
      diagnostics <- getDiagnostics(diagnosticsFileName)
      aggregatedDiagnostics <- aggregate(diagnostics)
      oxygenGeneratorRating <- computeOxygenGeneratorRating(diagnostics, aggregatedDiagnostics)
      carbonDioxideScrubberRating <- computeCarbonDioxideScrubberRating(diagnostics, aggregatedDiagnostics)
      lifeSupportRating = oxygenGeneratorRating * carbonDioxideScrubberRating
    yield reportResult(lifeSupportRating )
  } recover( handleFailure(_) )

  def computeOxygenGeneratorRating(diagnostics: List[BitCounts], aggregatedDiagnostics: BitCounts, bitNumber: Int = 0): Try[Int] =
    if diagnostics.size == 1 then diagnostics.head.toInt
    else
      val filteredDiagnostics =
        if aggregatedDiagnostics.isMostlySet(bitNumber) || aggregatedDiagnostics.isEquallySetAndClear(bitNumber)
        then diagnostics.filter(_.isSet(bitNumber))
        else diagnostics.filter(_.isClear(bitNumber))
      aggregate(filteredDiagnostics).flatMap{ filteredAggregatedDiagnostics =>
        computeOxygenGeneratorRating(filteredDiagnostics, filteredAggregatedDiagnostics, bitNumber + 1) }

  def computeCarbonDioxideScrubberRating(diagnostics: List[BitCounts], aggregatedDiagnostics: BitCounts, bitNumber: Int = 0): Try[Int] =
    if diagnostics.size == 1 then diagnostics.head.toInt
    else
      val filteredDiagnostics =
        if aggregatedDiagnostics.isMostlySet(bitNumber) || aggregatedDiagnostics.isEquallySetAndClear(bitNumber)
        then diagnostics.filter(_.isClear(bitNumber))
        else diagnostics.filter(_.isSet(bitNumber))
      aggregate(filteredDiagnostics).flatMap{ filteredAggregatedDiagnostics =>
        computeCarbonDioxideScrubberRating(filteredDiagnostics, filteredAggregatedDiagnostics, bitNumber + 1) }

  def reportResult(lifeSupportRating: Int): Unit =
      println(s"The life support rating of the submarine is $lifeSupportRating.")
