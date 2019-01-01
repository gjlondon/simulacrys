package com.thrive.game

import com.badlogic.gdx.graphics.Texture.TextureFilter
import com.badlogic.gdx.graphics.g2d.{BitmapFont, Sprite, SpriteBatch, TextureRegion}
import com.badlogic.gdx.graphics.{GL20, OrthographicCamera, Texture}
import com.badlogic.gdx.{Application, Game, Gdx, ScreenAdapter}
import com.thrive.simulation.person.Commoner
import org.joda.time.DateTime
import com.badlogic.gdx.graphics.OrthographicCamera
import com.badlogic.gdx.utils.Disposable

import scala.util.Random

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

  def update(deltaTime: Float): Unit = {
  }
}

case class WorldRenderer(worldController: WorldController)
  extends ScreenAdapter with Disposable {

  private val TAG = classOf[WorldRenderer].getName

  override def resize(width: Int, height: Int): Unit = {
  }

  override def pause(): Unit = {
  }

  override def resume(): Unit = {
  }

  override def dispose(): Unit = {
  }

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

  override def show(): Unit = {
    texture.setFilter(TextureFilter.Linear, TextureFilter.Linear)
    camera.setToOrtho(false, 800, 480)
    val aspectRatio = sprite.getHeight / sprite.getWidth
    sprite.setSize(190f, 190f * aspectRatio)
  }

  override def render(delta: Float): Unit = {
    Gdx.gl.glClearColor(0.1f, 1, 1, 0.1f)
    Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT)
    camera.update()
    batch.setProjectionMatrix(camera.combined)
    batch.begin()
    val fred = Commoner.randomCommoner(DateTime.now)

    val degreesPerSecond = 1000.0f
    val extraRotation = Gdx.graphics.getDeltaTime * degreesPerSecond
    val rot = (sprite.getRotation + extraRotation) % 360
    val shakeAmplitudeInDegrees = 7.0f
    val shake = MathUtils.sin(rot) * shakeAmplitudeInDegrees

    sprite.setRotation(shake)

    sprite.setCenter(200, 200)
    sprite.draw(batch)


    font.draw(
      batch, fred.name,
      Random.nextInt(600), font.getCapHeight + 50
    )
    batch.end()
  }

}