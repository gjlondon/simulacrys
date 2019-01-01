package com.thrive.game

import com.badlogic.gdx.graphics.Texture.TextureFilter
import com.badlogic.gdx.graphics.g2d.{BitmapFont, Sprite, SpriteBatch, TextureRegion}
import com.badlogic.gdx.graphics.{GL20, OrthographicCamera, Pixmap, Texture}
import com.badlogic.gdx.math.MathUtils
import com.badlogic.gdx.utils.Disposable
import com.badlogic.gdx.{Application, Game, Gdx, ScreenAdapter}

class ThriveApp extends Game {
  private val TAG = classOf[ThriveApp].getName

  override def create(): Unit = {
    Gdx.app.setLogLevel(Application.LOG_DEBUG)
    val worldController = new WorldController()
    setScreen(new WorldRenderer(worldController))
  }
}


object WorldController {
  private val TAG = classOf[WorldController].getName
}

case class WorldController() {
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


  private def updateTestObjects(deltaTime: Float): Unit = { // Get current rotation from selected sprite
    var rotation = testSprites(selectedSprite).getRotation
    // Rotate sprite by 90 degrees per second
    rotation += 90 * deltaTime
    // Wrap around at 360 degrees
    rotation %= 360
    // Set new rotation value to selected sprite
    testSprites(selectedSprite).setRotation(rotation)
  }
}

case class WorldRenderer(worldController: WorldController)
  extends ScreenAdapter with Disposable {

  private val TAG = classOf[WorldRenderer].getName

  private lazy val camera = new OrthographicCamera()
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
  }

  override def show(): Unit = {
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
  val VIEWPORT_HEIGHT: Float = 100

  val VIEWPORT_WIDTH: Float = 100

}
