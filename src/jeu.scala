import hevs.graphics.FunGraphics
import java.awt.event.{KeyAdapter, KeyEvent}
import java.awt.{Color, Rectangle}
import scala.runtime.BooleanRef

object Display {
  val pixel_value: Int = JEU.zoom

  def blit(grid: Array[Array[Int]]): Unit = {
    JEU.gameWindow.frontBuffer.synchronized{
      JEU.gameWindow.clear()
      for ((x, xPos) <- grid.zipWithIndex;
           (y, yPos) <- x.zipWithIndex) {
        if (y == 0) { // 0 in the grid is a pathway
          JEU.gameWindow.setColor(Color.WHITE)
          JEU.gameWindow.drawFillRect(xPos*pixel_value, yPos*pixel_value, pixel_value, pixel_value)
        }
        if (y == 1) { // 1 in the grid is a wall
          JEU.gameWindow.setColor(Color.BLACK)
          JEU.gameWindow.drawFillRect(xPos*pixel_value, yPos*pixel_value, pixel_value, pixel_value)
        }
        if (y == 4) { // 4 in the grid is the player
          JEU.gameWindow.setColor(Color.red)
          JEU.gameWindow.drawFilledCircle(xPos*pixel_value , yPos*pixel_value , pixel_value)
        }
        if (y == 5) { // 5 in the grid is the end
          JEU.gameWindow.setColor(Color.BLUE)
          JEU.gameWindow.drawFilledCircle(xPos*pixel_value , yPos*pixel_value , pixel_value)
        }
      }
    }
  }
}
object Player{
  var x: Int = 1
  var y: Int = 1

  def Nextpos(): Unit = {
    JEU.maze(Player.x)(Player.y) = 4
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
  def generateMaze(width: Int, height: Int ,visualize: Boolean = false): Array[Array[Int]] = {
    var maze: Array[Array[Int]] = Array.ofDim[Int](width,height)

    for ((y, yPos) <- maze.zipWithIndex;
         (x, xPos) <- y.zipWithIndex) {
      if((yPos%2) == 0 || (xPos%2) == 0){
        maze(yPos)(xPos) = 1
      }
    }

    var cells: Array[Array[Int]] = Array.ofDim[Int]((width/2),(height/2))


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

      if (posX +1 >= cells(0).length) {
        validArray(2) = 0
      }else if(cells(posY)(posX+1) == 2 || cells(posY)(posX+1) == 3){
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

      var direction: Int = randomInValid(validArray) // 0: left 1: down 2: right 3: up
      direction match{
        case 0 => maze(posY*2 +1)(posX*2 -1 +1) = 0; posX -= 1
        case 1 => maze(posY*2 -1 +1)(posX*2 +1) = 0; posY -= 1
        case 2 => maze(posY*2 +1)(posX*2 +1 +1) = 0; posX += 1
        case 3 => maze(posY*2+ 1 +1)(posX*2 +1) = 0;posY += 1
        case _ =>
      }

      cells(posY)(posX) = 2

      visitedAll = true
      for (x <- cells; y <- x){
        if (y != 2) visitedAll = false
      }

      var foundNewStart: Boolean = false
      if (direction == 15){
        while (!foundNewStart){
          posY = (math.random()*cells.length).toInt
          posX = (math.random()*cells(0).length).toInt
          if(cells(posY)(posX) == 2){
            foundNewStart = true
          }
        }
      }
      if (visualize && !foundNewStart){Display.blit(maze)}
    }
    while (!visitedAll)


    maze
  }
}

object JEU extends App{
  var zoom = Dialogs.getHiddenString("please enter the zoom value").toInt
  var largeur = Dialogs.getHiddenString("please enter the Width value").toInt
  var hauteur = Dialogs.getHiddenString("please enter the Height value").toInt
  var WIDTH: Int = largeur
  if (WIDTH%2==0){WIDTH+=1}
  var HEIGHT: Int = hauteur
  if (HEIGHT%2==0){HEIGHT+=1}
  var visualize: Boolean = true

  val gameWindow : FunGraphics = new FunGraphics(WIDTH*Display.pixel_value,HEIGHT*Display.pixel_value)
  gameWindow.setKeyManager(new KeyAdapter(){
    override def keyPressed(e: KeyEvent): Unit = {
      maze(Player.x)(Player.y) = 0
      if (e.getKeyCode == KeyEvent.VK_UP) {
        if(maze(Player.x)(Player.y-1) != 1) Player.y -= 1
      }
      if (e.getKeyCode == KeyEvent.VK_DOWN) {
        if(maze(Player.x)(Player.y+1) != 1) Player.y += 1
      }
      if (e.getKeyCode == KeyEvent.VK_LEFT) {
        if(maze(Player.x-1)(Player.y) != 1) Player.x -= 1
      }
      if (e.getKeyCode == KeyEvent.VK_RIGHT) {
        if(maze(Player.x+1)(Player.y) != 1) Player.x += 1
      }
    }
  })

  var maze: Array[Array[Int]] = Maze.generateMaze(WIDTH,HEIGHT,visualize)
  maze(WIDTH-2)(HEIGHT-2) = 5 //end of the maze

  while(maze(WIDTH-2)(HEIGHT-2) == 5){
    Player.Nextpos
    Display.blit(maze)

    JEU.gameWindow.syncGameLogic(60)
  }
}
