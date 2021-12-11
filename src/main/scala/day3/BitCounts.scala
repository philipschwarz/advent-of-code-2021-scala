  package day3.common

  import scala.language.postfixOps
  import scala.util.Try

  type BitCounts = List[(Int,Int)]

  object BitCounts:

    def apply(bits: String): Try[BitCounts] = {
      Try {
        bits map {
          case '0' => (1, 0)
          case '1' => (0, 1)
          case other => throw IllegalArgumentException(
            s"Expected: a sequence of zeroes and ones, e.g. 011010; Actual: $bits")
        } toList
      }
    }

    extension (xs: BitCounts)

      def combine(ys: BitCounts): BitCounts =
        (xs zip ys) map {
          case ((x1, y1), (x2, y2)) => ((x1 + x2), (y1 + y2))
        }

      def gammaRate: Try[Int] =
        val bits = xs.map { (zeroes,ones) =>
          if zeroes >= ones then '0' else '1'
        }.mkString
        parseInt(bits)

      def epsilonRate: Try[Int] =
        val bits = xs.map { (zeroes,ones) =>
          if zeroes < ones then '0' else '1'
        }.mkString
        parseInt(bits)

      def toInt: Try[Int] =
        val bits = xs.map {
          case (1, 0) => '0'
          case (0, 1) => '1'
        }.mkString
        parseInt(bits)

      def isSet(bitNumber: Int): Boolean =
        xs(bitNumber) == (0,1)

      def isClear(bitNumber: Int): Boolean =
        xs(bitNumber) == (1,0)

      def isMostlySet(bitNumber: Int): Boolean =
        xs(bitNumber) match { case (zeroes, ones) => zeroes < ones }

      def isMostlyClear(bitNumber: Int): Boolean =
        xs(bitNumber) match { case (zeroes, ones) => zeroes > ones }

      def isEquallySetAndClear(bitNumber: Int): Boolean =
        xs(bitNumber) match { case (zeroes, ones) => zeroes == ones }

    private def parseInt(bits: String): Try[Int] =
      Try {
        Integer.parseInt(bits, 2)
      } recoverWith { throwable =>
        throw new IllegalArgumentException(
          s"Could not parse '$bits' into an Int. Reason: $throwable.")
      }