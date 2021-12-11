  package day3

  import scala.language.postfixOps
  import scala.util.Try

  opaque type BitCounts = List[(Int,Int)]

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
        tryParsingInt(bits)

      def epsilonRate: Try[Int] =
        val bits = xs.map { (zeroes,ones) =>
          if zeroes < ones then '0' else '1'
        }.mkString
        tryParsingInt(bits)

    private def tryParsingInt(bits: String): Try[Int] =
      Try {
        Integer.parseInt(bits, 2)
      } recoverWith { throwable =>
        throw new IllegalArgumentException(
          s"Could not parse '$bits' into an Int. Reason: $throwable.")
      }