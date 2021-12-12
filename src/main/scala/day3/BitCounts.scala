  package day3.common

  import scala.language.postfixOps
  import scala.util.Try

  opaque type BitCounts = Vector[(Int,Int)]

  /**
   * A BitCounts keeps track, across one or more binary numbers, of the
   * count of zeroes and ones seen in each bit. In the case of a single
   * number, the count of zeroes and the count of ones for each bit is
   * either zero or one.
   *
   * When we combine the BitCounts instances for multiple binary numbers,
   * the counts get added up.
   *
   *                               --0-----1-----1-----0--
   * BitCounts("0110") ---> Vector((1,0),(0,1),(0,1),(1,0)
   *                                |       |     |   |
   *                                0 1   0 1   0 1   0 1
   *
   *                               --1-----0-----1-----0--
   * BitCounts("1010") ---> Vector((0,1),(1,0),(0,1),(1,0)
   *                                  |   |       |   |
   *                                0 1   0 1   0 1   0 1
   *
   * BitCounts("0110")
   *   \
   *    combine ----------> Vector((1,1),(1,1),(0,2),(2,0)
   *   /                              |     |     |   |
   * BitCounts("1010")              0 1   0 1   0 1   0 1
   *
   * scala> BitCounts("0110")
   * val res0: util.Try[day3.common.BitCounts] = Success(Vector((1,0), (0,1), (0,1), (1,0)))
   *
   * scala> BitCounts("1010")
   * val res1: util.Try[day3.common.BitCounts] = Success(Vector((0,1), (1,0), (0,1), (1,0)))
   *
   * scala> BitCounts("abcd")
   * val res2: util.Try[day3.common.BitCounts] = Failure(java.lang.IllegalArgumentException: Expected: a sequence of zeroes and ones, e.g. 011010; Actual: abcd)
   *
   * scala> for
   *          bc1 <- BitCounts("0110")
   *          bc2 <- BitCounts("1010")
   *        yield bc1 combine bc2
    val res3: scala.util.Try[day3.common.BitCounts] = Success(Vector((1,1), (1,1), (0,2), (2,0)))
   */
  object BitCounts:

    def apply(bits: String): Try[BitCounts] = {
      Try {
        bits map {
          case '0' => (1, 0)
          case '1' => (0, 1)
          case other => throw IllegalArgumentException(
            s"Expected: a sequence of zeroes and ones, e.g. 011010; Actual: $bits")
        } toVector
      }
    }

    extension (xs: BitCounts)

      /** The next two methods make sense for a BitCounts
        * instance representing a single diagnostic number */

      def isOne(bitNumber: Int): Boolean =
        xs(bitNumber) == (0,1)

      def isZero(bitNumber: Int): Boolean =
        xs(bitNumber) == (1,0)

      /** The following method makes sense for all BitCount
        * instances: it is used to aggregate instances */

      def combine(ys: BitCounts): BitCounts =
        (xs zip ys) map {
          case ((x1, y1), (x2, y2)) =>
            ((x1 + x2), (y1 + y2))
        }

      /** The following methods make sense for BitCounts instances
        * representing the aggregation of multiple diagnostic
        * numbers, i.e. ones created using the combine method */

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

      /** In the diagnostic numbers represented by this BitCounts
        * instance, the indicated bit is mostly set to one */
      def isMostlyOne(bitNumber: Int): Boolean =
        xs(bitNumber) match { case (zeroes, ones) => zeroes < ones }

      /** In the diagnostic numbers represented by this BitCounts
        * instance, the indicated bit is mostly set to zero */
      def isMostlyZero(bitNumber: Int): Boolean =
        xs(bitNumber) match { case (zeroes, ones) => zeroes > ones }

      /** In the diagnostic numbers represented by this BitCounts
        * instance, the indicated bit is both zero and one in
        * equal measure. */
      def isEquallyZeroAndOne(bitNumber: Int): Boolean =
        xs(bitNumber) match { case (zeroes, ones) => zeroes == ones }

    private def parseInt(bits: String): Try[Int] =
      Try {
        Integer.parseInt(bits, 2)
      } recoverWith { throwable =>
        throw new IllegalArgumentException(
          s"Could not parse '$bits' into an Int. Reason: $throwable.")
      }