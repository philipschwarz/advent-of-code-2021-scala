package day3.part2

import day3.common.*
import day3.common.BitCounts._

import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

  @main def day3Part2: Unit = {
    for
      diagnostics <- getDiagnostics(diagnosticsFileName)
      oxygenGeneratorRating <- computeOxygenGeneratorRating(diagnostics)
      carbonDioxideScrubberRating <- computeCarbonDioxideScrubberRating(diagnostics)
      lifeSupportRating = oxygenGeneratorRating * carbonDioxideScrubberRating
    yield reportResult(lifeSupportRating)
  } recover( handleFailure(_) )

  def computeOxygenGeneratorRating(diagnostics: List[BitCounts], bitNumber: Int = 0): Try[Int] =
    if diagnostics.size == 1 then diagnostics.head.toInt
    else aggregate(diagnostics).flatMap{ aggregatedDiagnostics =>
      val filteredDiagnostics =
        if aggregatedDiagnostics.isMostlySet(bitNumber) || aggregatedDiagnostics.isEquallySetAndClear(bitNumber)
        then diagnostics.filter(_.isSet(bitNumber))
        else diagnostics.filter(_.isClear(bitNumber))
      computeOxygenGeneratorRating(filteredDiagnostics, bitNumber + 1)
    }

  def computeCarbonDioxideScrubberRating(diagnostics: List[BitCounts], bitNumber: Int = 0): Try[Int] =
    if diagnostics.size == 1 then diagnostics.head.toInt
    else aggregate(diagnostics).flatMap { aggregatedDiagnostics =>
      val filteredDiagnostics =
        if aggregatedDiagnostics.isMostlySet(bitNumber) || aggregatedDiagnostics.isEquallySetAndClear(bitNumber)
        then diagnostics.filter(_.isClear(bitNumber))
        else diagnostics.filter(_.isSet(bitNumber))
      computeCarbonDioxideScrubberRating(filteredDiagnostics, bitNumber + 1)
    }

  def reportResult(lifeSupportRating: Int): Unit =
    println(s"The life support rating of the submarine is $lifeSupportRating.")
