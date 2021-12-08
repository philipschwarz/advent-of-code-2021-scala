  package day2.part1

  import day2.{Command, handleErrorGettingCommands, tryToGetCommands}

  @main def dayTwoPart1: Unit =
    tryToGetCommands map { commands =>
      val endPosition = commands.foldLeft(startPosition)(updatePosition)
      reportPosition(endPosition)
    } recover { handleErrorGettingCommands(_) }

  val updatePosition: (Position, Command) => Position = {
    case (Position(horizontalPosition, depth), command) =>
      command match {
        case Command.Forward(amount) => Position(horizontalPosition + amount, depth)
        case Command.Up(amount) => Position(horizontalPosition, depth - amount)
        case Command.Down(amount) => Position(horizontalPosition, depth + amount)
      }
  }

  case class Position(horizontalPosition: Int, depth: Int)
  val startPosition = Position(horizontalPosition = 0, depth = 0)

  def reportPosition(position: Position): Unit =
    println(s"horizontal position times depth is ${position.horizontalPosition * position.depth}.")