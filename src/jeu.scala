import hevs.graphics.FunGraphics

import java.awt.{Color, Rectangle}
import scala.runtime.BooleanRef

object Display {
  val pixel_multiplicator: Int = 3
  def blit(grid: Array[Array[Int]]): Unit = {
    for ((x, xPos) <- grid.zipWithIndex;
         (y, yPos) <- x.zipWithIndex) {
      if (y == 1) { // 1 in the grid is a wall
        JEU.gameWindow.drawFillRect(xPos * pixel_multiplicator, yPos * pixel_multiplicator, pixel_multiplicator, pixel_multiplicator)
      }
      if (y == 4) { // 4 in the grid is the player
        JEU.gameWindow.drawFilledCircle(xPos * pixel_multiplicator, yPos * pixel_multiplicator, pixel_multiplicator)
      }
    }
  }
}
object Player {
  case class State(player: (Double, Double)) {
    def newState(dir: Int): State = {
      val (x, y) = player
      val (newx, newy) = dir match {
        case 1 => (x, y - 1) // up
        case 2 => (x, y + 1) // down
        case 3 => (x - 1, y) // left
        case 4 => (x + 1, y) // right
        case _ => (x, y)
      }
      val newplayer: (Double, Double) = (newx, newy)
      State (newplayer)
    }
  }
}


object Maze {
  def randomInValid(array : Array[Int]): Int = {
  var numOfValid: Int = 0
    for (i <- array){
      if (i == 1) numOfValid += 1
      }
    var ran: Int = (math.random() * numOfValid).toInt + 1
    var validcount: Int = 0
      for (i <- array.indices){
        if(array(i) == 1) validcount += 1
          if (validcount == ran) return i
      }
    15
  }
def generateMaze(width: Int, height: Int): Array[Array[Int]] = {
  var maze: Array[Array[Int]] = Array.ofDim[Int](width, height)
  var cells: Array[Array[Int]] = Array.ofDim[Int]((width/2-1),(height/2-1))
  /***
  * 0 -> unvisited
  * 1 -> wall
  * 2 -> visited
  * 3 -> visited and backtracked
  */
  var posX: Int = 0
  var posY: Int = 0
  cells(0)(0) = 2
  var visitedAll: Boolean = false
  do{
    var validArray: Array[Int] = new Array[Int](4) // Array that stores which values are valid
    //left-down-right-up
      if (posX -1 < 0) {
        validArray(0) = 0
      }else if(cells(posY)(posX -1) == 2 || cells(posY)(posX -1) == 3){
        validArray(0) = 0
      }else{
        validArray(0) = 1
      }

      if (posY -1 < 0) {
        validArray(1) = 0
      }else if(cells(posY-1)(posX) == 2 || cells(posY-1)(posX) == 3){
        validArray(1) = 0
      }else{
        validArray(1) = 1
      }

      if (posX +1 >= cells.length) {
        validArray(2) = 0
      }else if(cells(posX+1)(posY) == 2 || cells(posX+1)(posY) == 3){
        validArray(2) = 0
      }else{
        validArray(2) = 1
      }

      if (posY +1 >= cells.length) {
        validArray(3) = 0
      }else if(cells(posY+1)(posX) == 2 || cells(posY+1)(posX) == 3){
        validArray(3) = 0
      }else{
        validArray(3) = 1
      }
      println(validArray.mkString(" "))

      var direction: Int = randomInValid(validArray) // 0: left 1: down 2: right 3: up
      println(direction)
      direction match{
        case 0 => posX -= 1
        case 1 => posY -= 1
        case 2 => posX += 1
        case 3 => posY += 1
      }

      cells(posY)(posX) = 2
      println(posX+" "+posY)

      visitedAll = true
      for (x <- cells; y <- x){
        if (y != 2) visitedAll = false
      }
  }
  while (!visitedAll)
  maze
}
}

        object JEU extends App{
          var WIDTH: Int = 64
          var HEIGHT: Int = 64

          val gameWindow : FunGraphics = new FunGraphics(WIDTH*Display.pixel_multiplicator,HEIGHT*Display.pixel_multiplicator)

          var maze: Array[Array[Int]] = Maze.generateMaze(WIDTH,HEIGHT)

          Display.blit(maze)

        }
