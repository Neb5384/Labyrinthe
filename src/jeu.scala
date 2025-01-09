import hevs.graphics.FunGraphics

import java.awt.{Color, Rectangle}
import scala.runtime.BooleanRef

class Display {}
object Display {
  val pixel_multiplicator:Int = 10
  def blit(grid: Array[Array[Int]]): Unit = {
    for((x,xPos) <- grid.zipWithIndex;
        (y,yPos) <- x.zipWithIndex){
      if(y == 1){
        JEU.gameWindow.drawFillRect(xPos*pixel_multiplicator,yPos*pixel_multiplicator,pixel_multiplicator,pixel_multiplicator)
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
      //if ((posX -1 < 0)||)


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
  var WIDTH: Int = 75
  var HEIGHT: Int = 50

  val gameWindow : FunGraphics = new FunGraphics(WIDTH*Display.pixel_multiplicator,HEIGHT*Display.pixel_multiplicator)

  var maze: Array[Array[Int]] = Maze.generateMaze(WIDTH,HEIGHT)

  maze(WIDTH/2)(HEIGHT/2) = 1
  maze(0)(0) = 1
  maze(WIDTH-1)(HEIGHT-1) = 1
  Display.blit(maze)

}
