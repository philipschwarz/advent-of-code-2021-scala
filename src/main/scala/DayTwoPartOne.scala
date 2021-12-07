  package daytwopart1

  import daytwocommon.Command.*
  import daytwocommon.{tryToGetCommands, handleErrorGettingCommands}

  @main def dayTwoPart1: Unit =
    tryToGetCommands map { commands =>
      val endPosition = commands.foldLeft(startPosition) {
        case (Position(horizontalPosition, depth), command) =>
          command match {
            case Forward(amount) => Position(horizontalPosition + amount, depth)
            case Up(amount) => Position(horizontalPosition, depth - amount)
            case Down(amount) => Position(horizontalPosition, depth + amount)
          }
      }
      reportPosition(endPosition)
    } recover { handleErrorGettingCommands(_) }

  case class Position(horizontalPosition: Int, depth: Int)
  val startPosition = Position(horizontalPosition = 0, depth = 0)

  def reportPosition(position: Position): Unit =
    println(s"horizontal position times depth is ${position.horizontalPosition * position.depth}")