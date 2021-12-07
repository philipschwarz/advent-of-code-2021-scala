  package daytwopart2

  import daytwocommon.Command.*
  import daytwocommon.{tryToGetCommands, handleErrorGettingCommands}

  @main def dayTwoPart2: Unit =
    tryToGetCommands map { commands =>
      val endPosition = commands.foldLeft(startPosition) {
        case (Position(aim, horizontalPosition, depth), command) =>
          command match {
            case Forward(amount) => Position(aim, horizontalPosition + amount, depth + aim * amount)
            case Up(amount) => Position(aim - amount, horizontalPosition, depth)
            case Down(amount) => Position(aim + amount, horizontalPosition, depth)
          }
      }
      reportPosition(endPosition)
    } recover { handleErrorGettingCommands(_) }

  case class Position(aim: Int, horizontalPosition: Int, depth: Int)
  val startPosition = Position(aim = 0,horizontalPosition = 0, depth = 0)

  def reportPosition(position: Position): Unit =
    println(s"horizontal position times depth is ${position.horizontalPosition * position.depth}")