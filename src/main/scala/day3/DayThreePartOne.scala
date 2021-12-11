package day3.part1

import day3.common._
import day3.common.BitCounts._

import scala.language.postfixOps

  @main def day3Part1: Unit = {
    for
      diagnostics <- getDiagnostics(diagnosticsFileName)
      aggregatedDiagnostics <- aggregate(diagnostics)
      gammaRate <- aggregatedDiagnostics.gammaRate
      epsilonRate <- aggregatedDiagnostics.epsilonRate
      powerConsumption = gammaRate * epsilonRate
    yield reportResult(powerConsumption)
  } recover( handleFailure(_) )

  def reportResult(powerConsumption: Int): Unit =
    println(s"The power consumption of the submarine is $powerConsumption.")
