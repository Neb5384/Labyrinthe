import hevs.graphics.FunGraphics
import java.awt.{Color,Rectangle}

class Display {}
object Display {
  def blit(grid: Array[Array[Int]]): Unit = {
    for((x,xPos) <- grid.zipWithIndex;
        (y,yPos) <- x.zipWithIndex){
      if(y == 1){
        JEU.gameWindow.drawFillRect(xPos,yPos,10,10)
      }
    }
  }
}

object Maze {
  def generateMaze(width: Int, height: Int): Array[Array[Int]] = {
    var maze: Array[Array[Int]] = Array.ofDim[Int](width, height)
    return maze
  }
}

object JEU extends App{
  var WIDTH: Int = 150
  var HEIGHT: Int = 100

  val gameWindow : FunGraphics = new FunGraphics(WIDTH,HEIGHT)

  var maze: Array[Array[Int]] = Maze.generateMaze(WIDTH,HEIGHT)

  maze(20)(50) = 1
  Display.blit(maze)

}
