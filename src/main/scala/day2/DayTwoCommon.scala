
  package day2

  import scala.io.Source
  import scala.util.{Try, Using}

  enum Command:
    case Forward(amount: Int)
    case Up(amount: Int)
    case Down(amount: Int)

  def tryToGetCommands: Try[List[Command]] =
    for
      lines <- readLines(readingsFileName)
      readings <- parseCommands(lines)
    yield readings

  def readLines(fileName: String): Try[List[String]] =
    Using( Source.fromFile(fileName) ) { bufferedSurce =>
      bufferedSurce.getLines.toList
    }

  // String interpolation in extractors, as seen in Jakub Kozłowski's
  // 'Coding at 6am isn't fun' https://www.youtube.com/watch?v=4nGSF_Ub7QQ
  def parseCommands(lines: List[String]): Try[List[Command]] =
    Try {
      lines map {
        case s"forward $amount" => Command.Forward(amount.toInt)
        case s"up $amount" => Command.Up(amount.toInt)
        case s"down $amount" => Command.Down(amount.toInt)
        case other => throw new IllegalArgumentException(s"expected: forward|up|down <amount>; actual: $other.")
      }
    }

  val readingsFileName = "day-2-input.txt"

  def handleErrorGettingCommands(throwable: Throwable): Unit =
    println(s"ERROR: could not get commands due to the following exception: ${throwable}")