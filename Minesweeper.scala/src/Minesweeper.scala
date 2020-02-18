import java.util.Random

import scala.collection.mutable.ListBuffer

abstract class Cell(shownc: Boolean, flaggedc: Boolean) {
  val shown: Boolean = shownc
  val flagged: Boolean = flaggedc
}

case class Bomb(override val shown: Boolean, override val flagged: Boolean) extends Cell(shown, flagged)
case class Empty(override val shown: Boolean, override val flagged: Boolean) extends Cell(shown, flagged)
case class Hint(override val shown: Boolean, override val flagged: Boolean, val xc: Int) extends Cell(shown, flagged){
  val x = xc
}

class Game(rowsc: Int, columnsc: Int, bombsc: Int, boardc: List[List[Cell]] = Nil) {
  // Nil - empty list
  val rows = rowsc
  val columns = columnsc
  val bombs = bombsc
  var availableCells: ListBuffer[(Int,Int)] = ListBuffer()
  for (r <- 0 to (rows-1)){
    for (c <- 0 to (columns-1)){
      availableCells ++= Seq((r,c))
    }
  }
  val board = if (boardc != Nil) boardc else initBoard()

  private def initBoard(): List[List[Cell]] = {
    val board1 = List.tabulate(rows)(_ => List.tabulate(columns)(_ => new Empty(false, false)))
    //  @ up: creates a two-dim. list of empty fields: e.g. List(List((0,0), (0,1), (0,2)), List((1,0), (1,1), (1,2)),
    //  List((2,0), (2,1), (2,2)))
    val board2 = initBombs(bombs, board1)
    initHints(board2, rows, columns)
  }

  //bombs
  private def initBombs(quantity: Int, board: List[List[Cell]]): List[List[Cell]] = { // recursion
    val newboard = tryToPutBomb(board)
    if (quantity > 1) {
      initBombs(quantity - 1, newboard)
    } else {
      newboard
    }
  }

  private def putBomb(x: Int, y: Int, board: List[List[Cell]]): List[List[Cell]] = {
    board.updated(x, board(x).updated(y, Bomb(false, false)))
  }

  private def tryToPutBomb(board: List[List[Cell]]): List[List[Cell]] = {
    val rand = new Random().nextInt(availableCells.size-1)
    val x = availableCells(rand)._1
    val y = availableCells(rand)._2
    availableCells --= Seq((x,y))
    putBomb(x, y, board)
  }
  //end of bombs

  //hints
  private def initHints(board: List[List[Cell]], x: Int, y: Int): List[List[Cell]] = {
    List.tabulate(x, y)((i, j) => transformIntoHint(board, i, j))
  }

  private def transformIntoHint(boardWithBombs: List[List[Cell]], x: Int, y: Int): Cell = {
    val cell = boardWithBombs(x)(y)
    cell match {
      case Bomb(b,c) => Bomb(b,c)
      case _ => initHint(boardWithBombs, x, y)
    }
  }

  private def initHint(boardWithBombs: List[List[Cell]], x: Int, y: Int): Cell = {
    def countBomb(cell: Cell): Int = {
      cell match {
        case Bomb(_,_) => 1
        case _ => 0
      }
    }

    val neighborCells = getCellOnBoard(boardWithBombs, x - 1, y - 1) ::
                        getCellOnBoard(boardWithBombs, x, y - 1) ::
                        getCellOnBoard(boardWithBombs, x + 1, y - 1) ::
                        getCellOnBoard(boardWithBombs, x - 1, y) ::
                        getCellOnBoard(boardWithBombs, x + 1, y) ::
                        getCellOnBoard(boardWithBombs, x - 1, y + 1) ::
                        getCellOnBoard(boardWithBombs, x, y + 1) ::
                        getCellOnBoard(boardWithBombs, x + 1, y + 1) :: Nil
    val hintValue = (neighborCells.map(countBomb)).foldLeft(0)(_ + _) // is bomb
    hintValue match {
      case 0 => Empty(false, false)
      case _ => Hint(false, false,hintValue)
    }
  }
  //end of hints

  //game behavior
  def releaseFlag(tuple: (Int,Int)): Game = {
    val x = tuple._1 // first tuple elem.
    val y = tuple._2
    val cell = board(x)(y)
    cell match {
      case Empty(false, true) => {
        val newboard = board.updated(x, board(x).updated(y, Empty(false, false)))
        new Game(rows, columns, bombs, newboard)
      }
      case Bomb(false, true) => {
        val newboard = board.updated(x, board(x).updated(y, Bomb(false, false)))
        new Game(rows, columns, bombs, newboard)
      }
      case Hint(false, true, hint) => {
        val newboard = board.updated(x, board(x).updated(y, Hint(false, false, hint)))
        new Game(rows, columns, bombs, newboard) // ????
      }
      case _ => new Game(rows, columns, bombs, board)
    }
  }

  def putFlag(tuple: (Int,Int)): Game = {
    val x = tuple._1 // first tuple elem.
    val y = tuple._2
    val cell = board(x)(y)
    cell match {
      case Empty(false, false) => {
        val newboard = board.updated(x, board(x).updated(y, Empty(false, true)))
        new Game(rows, columns, bombs, newboard)
      }
      case Bomb(false, false) => {
        val newboard = board.updated(x, board(x).updated(y, Bomb(false, true)))
        new Game(rows, columns, bombs, newboard)
      }
      case Hint(false, false, hint) => {
        val newboard = board.updated(x, board(x).updated(y, Hint(false, true, hint)))
        new Game(rows, columns, bombs, newboard) // ????
      }
      case _ => new Game(rows, columns, bombs, board)
    }
  }

  def showCell(tuple: (Int, Int)): Game = {
    val x = tuple._1 // first tuple elem.
    val y = tuple._2
    val cell = board(x)(y)
    cell match {
      case Empty(false, false) => showNeighborCells(x, y)
      case Bomb(false, false) => {
        val newboard = board.updated(x, board(x).updated(y, Bomb(true, false)))
        new Game(rows, columns, bombs, newboard)
      }
      case Hint(false, false, hint) => {
        val newboard = board.updated(x, board(x).updated(y, Hint(true, false, hint)))
        new Game(rows, columns, bombs, newboard)
      }
      case _ => new Game(rows, columns, bombs, board)
    }
  }

  private def showNeighborCells(x: Int, y: Int): Game = {
    def getPosition(board: List[List[Cell]], x: Int, y: Int): List[(Int, Int)] = {
      if (contains(x, y)) {
        List(Tuple2(x, y)) // returns a list of one tuple
      } else {
        Nil
      }
    }

    val newboard = board.updated(x, board(x).updated(y, Empty(true, false)))
    val newGame = new Game(rows, columns, bombs, newboard) // # TO fix
    val neighborPositions: List[(Int, Int)] = getPosition(board, x - 1, y - 1) ++
                                              getPosition(board, x, y - 1) ++
                                              getPosition(board, x + 1, y - 1) ++
                                              getPosition(board, x - 1, y) ++
                                              getPosition(board, x + 1, y) ++
                                              getPosition(board, x - 1, y + 1) ++
                                              getPosition(board, x, y + 1) ++
                                              getPosition(board, x + 1, y + 1) ++ Nil
    neighborPositions.foldLeft(newGame)((game, tuple) => game.showCell(tuple))
  }

  def hasOnlyBombs(): Boolean = {
    !board.flatten.exists(cell => cell match { // flatten. next step is to check if won
      case Empty(false, _) => true
      case Hint(false, _, _) => true
      case _ => false
    })
  }

  def hasActiveBomb(): Boolean = { // to check if game is lost
    board.flatten.exists(cell => cell match {
      case Bomb(true, _) => true
      case _ => false
    })
  }

  def contains(x: Int, y: Int): Boolean = { // if grid contains (x,y) cell
    x >= 0 && x < rows && y >= 0 && y < columns
  }

  def getCell(x: Int, y: Int): Cell = {
    if (contains(x, y)) {
      board(x)(y)
    } else {
      null
    }
  }
  
  def getCellOnBoard(board: List[List[Cell]], x: Int, y: Int): Cell = {
    if (contains(x, y)) {
      board(x)(y)
    } else {
      null
    }
  }
}
