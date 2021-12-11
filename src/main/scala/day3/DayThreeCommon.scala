  package day3.common

  import cats.implicits.*
  import BitCounts.combine

  import scala.io.Source
  import scala.util.{Try, Using}

  def getDiagnostics(diagnosticsFileName: String): Try[List[BitCounts]] =
    for
      lines <- readLines(diagnosticsFileName)
      diagnostics <- parseDiagnostics(lines)
    yield diagnostics

  def aggregate(diagnostics: List[BitCounts]): Try[BitCounts] =
    Try {
      diagnostics.size match {
        case 0 => throw new IllegalArgumentException("expected: 1 or more diagnostic numbers; actual: none.")
        case 1 => diagnostics.head
        case other => diagnostics reduce { _ combine _ }
      }
    }

  def readLines(fileName: String): Try[List[String]] =
    Using( Source.fromFile(fileName) ) { bufferedSurce =>
      bufferedSurce.getLines.toList
    }

  def parseDiagnostics(lines: List[String]): Try[List[BitCounts]] =
    lines traverse( BitCounts(_) )

  val diagnosticsFileName = "day-3-input.txt"

  def handleFailure(throwable: Throwable): Unit =
    println(s"ERROR: Could not get diagnostics due to the following exception: ${throwable.getMessage}")