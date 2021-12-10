package day3

  val diagnosticReport = List(
    "00100",
    "11110",
    "10110",
    "10111",
    "10101",
    "01111",
    "00111",
    "11100",
    "10000",
    "11001",
    "00010",
    "01010",
  )

  type BitCounts = Vector[(Int,Int)]

  extension (s: String)
    def toBitCounts: BitCounts = s.map {
      case '0' => (1,0)
      case '1' => (0,1)
      case other => (0,0)
    }.toVector

  extension (xs: BitCounts)

    def combine(ys: BitCounts): BitCounts =
      (xs zip ys) map { case ((x1, y1), (x2, y2)) => ((x1 + x2), (y1 + y2)) }

    def gammaRate: Int =
      val bits = xs.map { (zeroes,ones) =>
        if zeroes >= ones then '0' else '1'
      }.mkString
      Integer.parseInt(bits,2)

    def epsilonRate: Int =
      val bits = xs.map { (zeroes,ones) =>
        if zeroes < ones then '0' else '1'
      }.mkString
      Integer.parseInt(bits,2)

@main def main =

  val finalBitCounts = diagnosticReport.tail.foldLeft(diagnosticReport.head.toBitCounts){
    (bitCounts, number) =>
      bitCounts combine number.toBitCounts
  }

  println(finalBitCounts.gammaRate * finalBitCounts.epsilonRate)
