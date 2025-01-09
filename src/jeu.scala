import hevs.graphics.FunGraphics
import java.awt.{Color,Rectangle}

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
    /***
     * 1 -> wall
     * 0 -> visited
     */
    var initialPosX = 0
    var initialPosy = 0

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
