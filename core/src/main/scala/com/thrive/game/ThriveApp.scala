package com.thrive.game

import com.badlogic.gdx._
import com.badlogic.gdx.graphics.Texture.TextureFilter
import com.badlogic.gdx.graphics.g2d.{BitmapFont, Sprite, SpriteBatch, TextureRegion}
import com.badlogic.gdx.graphics.{GL20, OrthographicCamera, Pixmap, Texture}
import com.badlogic.gdx.math.MathUtils
import com.badlogic.gdx.utils.Disposable
import com.thrive.simulation.clock.Clock
import com.thrive.simulation.world.World
import org.joda.time.DateTime
import org.joda.time.chrono.GJChronology

class ThriveApp extends Game {
  private val TAG = classOf[ThriveApp].getName

  override def create(): Unit = {
    Gdx.app.setLogLevel(Application.LOG_DEBUG)

    val chrono = GJChronology.getInstance

    val startingTime = new DateTime(1066, 10, 14, 10, 0, 0, 0, chrono)
    val world = World.randomWorld(startingTime)

    val worldController = WorldController(world = world, time = startingTime)
    setScreen(WorldRenderer(worldController))
  }
}


object WorldController {
  private val TAG = classOf[WorldController].getName
}

case class WorldController(var world: World, var time: DateTime) extends InputAdapter {
  private val TAG = classOf[WorldController].getName
  val testSprites: Vector[Sprite] = {

    // Create empty POT-sized Pixmap with 8 bit RGBA pixel data
    val width = 32
    val height = 32
    val pixmap: Pixmap = createProceduralPixmap(width, height)
    // Create a new texture from pixmap data
    val texture = new Texture(pixmap)
    // Create new sprites using the just created texture
    Vector.fill[Sprite](5) {
      val spr = new Sprite(texture)
      // Define sprite size to be 1m x 1m in game world
      spr.setSize(5, 5)
      // Set origin to sprite's center
      spr.setOrigin(spr.getWidth / 2.0f, spr.getHeight / 2.0f)
      // Calculate random position for sprite
      val randomX = MathUtils.random(0, Constants.VIEWPORT_WIDTH)
      val randomY = MathUtils.random(0, Constants.VIEWPORT_HEIGHT)
      spr.setPosition(randomX, randomY)
      spr
    }
  }

  var selectedSprite = 0
  Gdx.input.setInputProcessor(this)

  import com.badlogic.gdx.graphics.Pixmap
  import com.badlogic.gdx.graphics.Pixmap.Format

  private def createProceduralPixmap(width: Int, height: Int): Pixmap = {
    val pixmap = new Pixmap(width, height, Format.RGBA8888)
    // Fill square with red color at 50% opacity
    pixmap.setColor(1, 0, 0, 0.5f)
    pixmap.fill()
    // Draw a yellow-colored X shape on square
    pixmap.setColor(1, 1, 0, 1)
    pixmap.drawLine(0, 0, width, height)
    pixmap.drawLine(width, 0, 0, height)
    // Draw a cyan-colored border around square
    pixmap.setColor(0, 1, 1, 1)
    pixmap.drawRectangle(0, 0, width, height)

    pixmap
  }

  def update(deltaTime: Float): Unit = {
    updateTestObjects(deltaTime)
  }

  import com.badlogic.gdx.Gdx
  import com.badlogic.gdx.Input.Keys

  override def keyUp(keycode: Int): Boolean = { // Reset game world
    if (keycode == Keys.R) {
      Gdx.app.debug(TAG, "Game world resetted")
    }
    else { // Select next sprite
      if (keycode == Keys.SPACE) {
        selectedSprite = (selectedSprite + 1) % testSprites.length
        Gdx.app.debug(TAG, "Sprite #" + selectedSprite + " selected")
      }
    }
    false
  }

  private def updateTestObjects(deltaTime: Float): Unit = { // Get current rotation from selected sprite
    var rotation = testSprites(selectedSprite).getRotation
    // Rotate sprite by 90 degrees per second
    rotation += 90 * deltaTime
    // Wrap around at 360 degrees
    rotation %= 360
    // Set new rotation value to selected sprite
    testSprites(selectedSprite).setRotation(rotation)
  }

  import com.badlogic.gdx.Application.ApplicationType
  import com.badlogic.gdx.Gdx
  import com.badlogic.gdx.Input.Keys

  private def handleDebugInput(deltaTime: Float): Unit = {
    if (Gdx.app.getType ne ApplicationType.Desktop) return
    // Selected Sprite Controls
    val sprMoveSpeed = 20 * deltaTime
    if (Gdx.input.isKeyPressed(Keys.A)) moveSelectedSprite(-sprMoveSpeed, 0)
    if (Gdx.input.isKeyPressed(Keys.D)) moveSelectedSprite(sprMoveSpeed, 0)
    if (Gdx.input.isKeyPressed(Keys.W)) moveSelectedSprite(0, sprMoveSpeed)
    if (Gdx.input.isKeyPressed(Keys.S)) moveSelectedSprite(0, -sprMoveSpeed)
  }

  private def moveSelectedSprite(x: Float, y: Float): Unit = {
    testSprites(selectedSprite).translate(x, y)
  }
}

case class WorldRenderer(worldController: WorldController)
  extends ScreenAdapter with Disposable {

  private val TAG = classOf[WorldRenderer].getName

  private lazy val camera = new OrthographicCamera()
  val cameraGUI = new OrthographicCamera(Constants.VIEWPORT_GUI_WIDTH,
    Constants.VIEWPORT_GUI_HEIGHT)

  private lazy val batch: SpriteBatch = new SpriteBatch()
  private lazy val texture = new Texture("libgdxlogo.png")
  private lazy val region = new TextureRegion(texture, 0, 0,
    texture.getWidth, texture.getHeight)
  private lazy val sprite = new Sprite(region)

  private lazy val font = {
    val f = new BitmapFont()
    f.getData.setScale(2f)
    f
  }

  override def resize(width: Int, height: Int): Unit = {
    camera.viewportWidth = (Constants.VIEWPORT_HEIGHT / height) * width
    camera.update()

    cameraGUI.viewportHeight = Constants.VIEWPORT_GUI_HEIGHT
    cameraGUI.viewportWidth = (Constants.VIEWPORT_GUI_HEIGHT / height.asInstanceOf[Float]) * width.asInstanceOf[Float]
    cameraGUI.position.set(cameraGUI.viewportWidth / 2, cameraGUI.viewportHeight / 2, 0)
  }

  override def show(): Unit = {
    cameraGUI.position.set(0, 0, 0)
    cameraGUI.setToOrtho(true) // flip y-axis

    texture.setFilter(TextureFilter.Linear, TextureFilter.Linear)
    camera.position.set(0, 0, 0)
    camera.setToOrtho(false, Constants.VIEWPORT_WIDTH, Constants.VIEWPORT_HEIGHT)
    Gdx.gl.glClearColor(
      0.1f, 0.1f,
      0.25f, 0.1f
    )
  }

  override def render(delta: Float): Unit = {
    Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT)

    camera.update()

    worldController.update(delta)
    renderTestObjects()
    renderGui()

  }

  private def renderGui(): Unit = {
    batch.setProjectionMatrix(cameraGUI.combined)
    batch.begin()

    batch.end()
  }

  private def renderTestObjects(): Unit = {
    batch.setProjectionMatrix(camera.combined)
    batch.begin()
    for (sprite <- worldController.testSprites) {
      sprite.draw(batch)
    }
    batch.end()
  }
}

object Constants {
  val VIEWPORT_GUI_HEIGHT: Float = 640

  val VIEWPORT_GUI_WIDTH: Float = 800

  val VIEWPORT_HEIGHT: Float = 100

  val VIEWPORT_WIDTH: Float = 100

}
