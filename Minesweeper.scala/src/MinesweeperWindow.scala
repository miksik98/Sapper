import java.awt.event.{MouseAdapter, MouseEvent}
import java.awt.{BorderLayout, Color, Dimension, GridLayout}

import javax.swing._

class MinesweeperWindow extends JFrame {
  import Modes._
  val rowsLvl1 = 9
  val columnsLvl1 = 9
  val bombsLvl1 = 9
  val rowsLvl2 = 12
  val columnsLvl2 = 12
  val bombsLvl2 = 30
  val rowsLvl3 = 16
  val columnsLvl3 = 16
  val bombsLvl3 = 70
  val intro = new IntroPanel()
  val body = new JPanel()
  val gamePanel = new JPanel()
  val toolBar = new JToolBar()
  val btnLevel1 = new JLabel()
  val btnLevel2 = new JLabel()
  val btnLevel3 = new JLabel()
  val newGame = new JLabel(new ImageIcon("img/nwGame.png")) // # TODO
  val squareIcon = new ImageIcon("img/square.png")
  val flagIcon = new ImageIcon("img/flag.png")
  val bombIcon = new ImageIcon("img/bomb.png")
  val emptyIcon = new ImageIcon("img/empty.png")
  val bckColor = new Color(169, 169, 169)
  var time = 0
  val timer = new JLabel()
  var gameIsRunning = false
  var modeButton = new JLabel(getCurrentImage())

  object Modes {
    object Mode extends Enumeration {
      type Mode = Value
      val NormalMode, FlagMode = Value
    }
    import Mode._
    var currentMode: Mode = NormalMode
    def changeMode() = {
      currentMode match {
        case NormalMode => currentMode = FlagMode
        case FlagMode => currentMode = NormalMode
      }
    }
    def setNormalMode(): Unit ={
      currentMode = NormalMode
    }

    def isNormalMode(): Boolean ={
      currentMode == NormalMode
    }
    def getCurrentImage(): ImageIcon ={
      currentMode match {
        case NormalMode => bombIcon
        case FlagMode => flagIcon
      }
    }
  }


  def defineGamePanel(game: Game) {
    val gridLayout = new GridLayout()
    gridLayout.setRows(game.rows)
    gridLayout.setColumns(game.columns)
    gamePanel.setLayout(gridLayout)
  }

  def initializeGamePanel(game: Game) {
    val gameButtons = List.tabulate(game.rows, game.columns)((_, _) => new JLabel(squareIcon))
    gamePanel.removeAll()
    gameButtons.flatten.map(button => gamePanel.add(button))
    validate()
    repaint()

    def updateGameState(game: Game) {
      gameButtons.flatten.map(button => {
        val panelX = (button.getX() / button.getSize().getWidth()).toInt
        val panelY = (button.getY() / button.getSize().getHeight()).toInt

        val cell = game.getCell(panelX, panelY)
        cell match {
          case Empty(false, true) | Bomb(false, true) | Hint(false, true, _)  => {
            button.setIcon(flagIcon)
            button.revalidate()
            button.setOpaque(true)
            button.setBackground(bckColor)
            button.setBorder(BorderFactory.createLineBorder(new Color(49, 133, 156)))
            button.setForeground(new Color(119, 147, 60))
          }
          case Empty(true, false) => {
            button.setIcon(null)
            button.revalidate()
            button.setOpaque(true)
            button.setBackground(bckColor)
            button.setBorder(BorderFactory.createLineBorder(new Color(49, 133, 156)))
            button.setForeground(new Color(119, 147, 60))
          }
          case Hint(true, false ,hint) => {
            button.setIcon(null)
            button.revalidate()
            button.setOpaque(true)
            button.setBackground(bckColor)
            button.setBorder(BorderFactory.createLineBorder(new Color(49, 133, 156)))
            button.setForeground(new Color(102, 0, 51))
            button.setText(hint.toString)
          }
          case Bomb(true, false) => {
            button.setIcon(bombIcon)
            button.revalidate()
            button.setOpaque(true)
            button.setBackground(bckColor)
            button.setBorder(BorderFactory.createLineBorder(new Color(49, 133, 156)))
            button.setForeground(new Color(119, 147, 60))
          }
          case _ => {}
        }

        button.addMouseListener(new MouseAdapter() {
          override def mouseReleased(e: MouseEvent) {
            if(isNormalMode()) showCell(button, panelX, panelY)
            else flagCell(button,panelX,panelY)
          }
        })
      })
    }

    updateGameState(game)
    validate()
    repaint()

    if (game.hasActiveBomb()) {
      gameIsRunning = false
      JOptionPane.showInternalMessageDialog(body, "Game Over", "Bomb exploded!",
        JOptionPane.ERROR_MESSAGE)
      backToIntro()

    } else if (game.hasOnlyBombs) {
      gameIsRunning = false
      JOptionPane.showInternalMessageDialog(body, "You Won", "Congratulations, you won!",
        JOptionPane.INFORMATION_MESSAGE)
      backToIntro()
    }

    def showCell(button: JLabel, panelX: Int, panelY: Int) {
      if (button.isEnabled()) {
        button.setEnabled(false)
        val gameUpdated = game.showCell(panelX, panelY)
        initializeGamePanel(gameUpdated)
      }
    }

    def flagCell(button: JLabel, panelX: Int, panelY: Int): Unit ={
      if (button.isEnabled()) {
        button.setEnabled(false)
        val cell = game.getCell(panelX, panelY)
        cell match {
          case Empty(false, false) | Bomb(false, false) | Hint(false, false, _) => {
            //button.setIcon(flagIcon)
            val gameUpdated = game.putFlag(panelX, panelY)
            initializeGamePanel(gameUpdated)
          }
          case Empty(false, true) | Bomb(false, true) | Hint(false, true, _) => {
            button.setIcon(null)
            val gameUpdated = game.releaseFlag(panelX, panelY)
            initializeGamePanel(gameUpdated)
          }
          case _ => ()
        }
      }
    }
  }

  def createTimerThread(): Unit ={
    gameIsRunning = true
    val thread = new Thread {
      override def run: Unit = {
        time = 0
        while(gameIsRunning){
          Thread.sleep(1000)
          time += 1
          timer.setText(intToTime(time))
        }
      }
    }
    thread.start()
  }

  def defineNewGame(game: Game) {
    newGame.addMouseListener(new MouseAdapter() {
      override def mouseReleased(e: MouseEvent) {
        setNormalMode()
        modeButton.setIcon(getCurrentImage())
        time = -1
        initializeGamePanel(new Game(game.rows, game.columns, game.bombs))
      }
    })
  }

  def defineModeButton(): Unit ={
    modeButton.addMouseListener(new MouseAdapter(){
      override def mouseReleased(e: MouseEvent) {
        changeMode()
        modeButton.setIcon(getCurrentImage())
      }
    })
  }

  def defineToolBar() {
    toolBar.setFloatable(false)
    toolBar.add(newGame)
    toolBar.addSeparator(new Dimension(50,0))
    toolBar.add(timer)
    toolBar.addSeparator(new Dimension(50,0))
    toolBar.add(modeButton)
  }

  def intToTime(time: Int): String ={

    val seconds = time % 60
    val minutes = time / 60

    var time_str = ""

    if (minutes < 10) time_str += "0"
    time_str += minutes.toString()
    time_str += ":"
    if (seconds < 10) time_str += "0"
    time_str += seconds.toString()

    time_str
  }

  def defineBody() {
    body.setLayout(new BorderLayout())
    body.add(gamePanel, BorderLayout.CENTER)
    body.add(toolBar, BorderLayout.NORTH)
  }

  def defineTimer(){
    timer.setSize(5,1)
    timer.setText("00:00")
  }

  def defineLevelButtons() {
    val icon1 = new ImageIcon("img/btn1.png")
    btnLevel1.setSize(icon1.getIconWidth, icon1.getIconHeight)
    btnLevel1.setIcon(icon1)
    btnLevel1.setLocation(100, 125)
    btnLevel1.addMouseListener(new MouseAdapter() {
      override def mouseReleased(e: MouseEvent) {
        val game = new Game(rowsLvl1, columnsLvl1, bombsLvl1)
        initializeLevel(game)
      }
    })
    val icon2 = new ImageIcon("img/btn2.png")
    btnLevel2.setSize(icon2.getIconWidth, icon2.getIconHeight)
    btnLevel2.setIcon(icon2)
    btnLevel2.setLocation(100, 183)
    btnLevel2.addMouseListener(new MouseAdapter() {
      override def mouseReleased(e: MouseEvent) {
        val game = new Game(rowsLvl2, columnsLvl2, bombsLvl2)
        initializeLevel(game)
      }
    })
    val icon3 = new ImageIcon("img/btn3.png")
    btnLevel3.setSize(icon3.getIconWidth, icon3.getIconHeight)
    btnLevel3.setIcon(icon3)
    btnLevel3.setLocation(100, 241)
    btnLevel3.addMouseListener(new MouseAdapter() {
      override def mouseReleased(e: MouseEvent) {
        val game = new Game(rowsLvl3, columnsLvl3, bombsLvl3)
        initializeLevel(game)
      }
    })
  }

  def initializeLevel(game: Game) {
    setNormalMode()
    modeButton.setIcon(getCurrentImage())
    setSize(game.rows * (squareIcon.getIconWidth - 1), (game.columns * squareIcon.getIconHeight))
    getContentPane.removeAll()
    setContentPane(body)
    validate()
    repaint()
    defineNewGame(game)
    defineGamePanel(game)
    initializeGamePanel(game)
    gameIsRunning = true
    createTimerThread()

  }

  def defineIntro() {
    intro.setLayout(null)
    intro.add(btnLevel1)
    intro.add(btnLevel2)
    intro.add(btnLevel3)
  }

  def backToIntro(){
    getContentPane.removeAll()
    validate();
    repaint();
    time = 0
    defineIntro()
    defineTimer()
    defineBody()

    setSize(intro.imgWidth, intro.imgHeight)
    setContentPane(intro)
    setTitle("SAPPER")

  }

  defineLevelButtons()
  defineIntro()
  defineTimer()
  defineToolBar()
  defineBody()
  defineModeButton()

  setSize(intro.imgWidth, intro.imgHeight)
  setContentPane(intro)
  setTitle("SAPPER")
  setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
}

object GameMain {
  def main(args: Array[String]): Unit = {
    val window = new MinesweeperWindow()
    window.setVisible(true)
  }
}
