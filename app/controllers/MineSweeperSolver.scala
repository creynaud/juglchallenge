package controllers

import collection.mutable._

object MineSweeperSolver {
  def parse(text: String): MineSweeperSolver = {
    val minesListBuffer = ListBuffer[(Int, Int)]()
    var currentLine: Int = 0
    var lines = 0
    var columns = 0
    for (line <- text.split("[\\n-\\r]+")) {
      if (line.contains(".") || line.contains("*")) {
        var currentChar: Int = 0
        for (c <- line) {
          if (c == '*') {
            minesListBuffer += ((currentLine, currentChar))
          }
          currentChar = currentChar + 1
        }
        currentLine = currentLine + 1
      } else {
        val sizes = line.split("\\s+")
        for (sizeString <- sizes) {
          if (lines > 0) {
            columns = sizeString.toInt
          } else {
            lines = sizeString.toInt
          }
        }
      }
    }
    new MineSweeperSolver(lines, columns, minesListBuffer.toList)
  }
}

class MineSweeperSolver(lines: Int, columns: Int, mines: List[(Int, Int)]) {

  def solution(): String = {
    // TODO compute the number of mines by walking the mines and marking +1 in the surrounding spots
    val sb = new StringBuilder()
    for (line <- Range(0, lines)) {
      for (column <- Range(0, columns)) {
        if (mines.contains((line, column))) {
          sb.append("*")
        } else {
          var numberOfSurroundingMines = 0
          if (mines.contains((line, column - 1))) {
            numberOfSurroundingMines += 1
          }
          if (mines.contains((line, column + 1))) {
            numberOfSurroundingMines += 1
          }
          if (mines.contains((line - 1, column))) {
            numberOfSurroundingMines += 1
          }
          if (mines.contains((line + 1, column))) {
            numberOfSurroundingMines += 1
          }
          if (mines.contains((line - 1, column - 1))) {
            numberOfSurroundingMines += 1
          }
          if (mines.contains((line - 1, column + 1))) {
            numberOfSurroundingMines += 1
          }
          if (mines.contains((line + 1, column - 1))) {
            numberOfSurroundingMines += 1
          }
          if (mines.contains((line + 1, column + 1))) {
            numberOfSurroundingMines += 1
          }
          sb.append(numberOfSurroundingMines)
        }
        if ((column == columns - 1) && (line != lines - 1)) {
          sb.append("\n")
        }
      }
    }
    return sb.mkString
  }

  override def toString(): String = {
    val sb = new StringBuilder()
    sb.append(lines)
    sb.append(" ")
    sb.append(columns)
    sb.append("\n")
    for (line <- Range(0, lines)) {
      for (column <- Range(0, columns)) {
        if (mines.contains((line, column))) {
          sb.append("*")
        } else {
          sb.append(".")
        }
        if ((column == columns - 1) && (line != lines - 1)) {
          sb.append("\n")
        }
      }
    }
    return sb.mkString
  }
}
