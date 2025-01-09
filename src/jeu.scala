import hevs.graphics.FunGraphics

import java.awt.{Color, Rectangle}
import scala.runtime.BooleanRef

class Display {}
object Display {
  def blit(grid: Array[Array[Int]]): Unit = {
    for((x,xPos) <- grid.zipWithIndex;
        (y,yPos) <- x.zipWithIndex){
      if(y == 1){
        JEU.gameWindow.drawFillRect(xPos*10,yPos*10,10,10)
      }
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
    val posX: Int = 0
    val posY: Int = 0
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

      println(randomInValid(validArray))

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
  var WIDTH: Int = 15
  var HEIGHT: Int = 11

  val gameWindow : FunGraphics = new FunGraphics(WIDTH*10,HEIGHT*10)

  var maze: Array[Array[Int]] = Maze.generateMaze(WIDTH,HEIGHT)

  maze(2)(5) = 1
  Display.blit(maze)

}
