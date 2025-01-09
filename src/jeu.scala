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
  def generateMaze(width: Int, height: Int): Array[Array[Int]] = {
    var maze: Array[Array[Int]] = Array.ofDim[Int](width, height)
    var cells: Array[Array[Int]] = Array.ofDim[Int]((width/2-1),(height/2-1))
    /***
     * _ -> unvisited
     * 1 -> wall
     * 0 -> visited
     * 2 -> visited and backtracked
     */
    val posX: Int = 0
    val posY: Int = 0
    cells(0)(0) = 0
    var visitedAll: Boolean = false
    do{
      var validArray: Array[Int] = new Array[Int](4) // Array that stores which values are empty
      //left-down-right-up
      if ((posX -1 < 0)||)


      visitedAll = true
      for (x <- cells; y <- x){
        if (y != 0) visitedAll = false
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
