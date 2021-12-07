
  package daytwocommon

  import scala.io.Source
  import scala.util.{Try, Using}

  enum Command:
    case Forward(amount: Int)
    case Up(amount: Int)
    case Down(amount: Int)

  import Command._

  def tryToGetCommands: Try[List[Command]] =
    for
      lines <- readLines(readingsFileName)
      readings <- parseCommands(lines)
    yield readings

  def readLines(fileName: String): Try[List[String]] =
    Using( Source.fromFile(fileName) ) { bufferedSurce =>
      bufferedSurce.getLines.toList
    }

  def parseCommands(lines: List[String]): Try[List[Command]] =
    Try {
      lines map { line =>
        line.split("\\s+") match {
          case Array(command, amount) =>
            command match {
              case "forward" => Forward(amount.toInt)
              case "up" => Up(amount.toInt)
              case "down" => Down(amount.toInt)
              case other => throw new IllegalArgumentException(s"expected: 'forward', 'up' or 'down'; actual: $command.")
            }
          case other => throw new IllegalArgumentException(s"expected: <command> <amount>; actual: $line.")
        }
      }
    }

  val readingsFileName = "day-2-input.txt"

  def handleErrorGettingCommands(throwable: Throwable): Unit =
    println(s"ERROR: could not get commands due to the following exception: ${throwable}")