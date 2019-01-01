package com.thrive.game

import com.badlogic.gdx.graphics.Texture.TextureFilter
import com.badlogic.gdx.graphics.g2d.{BitmapFont, SpriteBatch}
import com.badlogic.gdx.graphics.{GL20, OrthographicCamera, Texture}
import com.badlogic.gdx.{Game, ScreenAdapter}
import com.thrive.simulation.person.Commoner
import org.joda.time.DateTime

import scala.util.Random

class ThriveApp extends Game {

  lazy val sprite = new Texture("libgdxlogo.png")
  lazy val batch = new SpriteBatch

  override def create(): Unit = {
    setScreen(new MainScreen)
  }
}

class MainScreen extends ScreenAdapter {

  import com.badlogic.gdx.Gdx
  import com.badlogic.gdx.graphics.Texture
  import com.badlogic.gdx.graphics.g2d.{Sprite, TextureRegion}
  import com.badlogic.gdx.math.MathUtils

  private lazy val camera = new OrthographicCamera()
  private lazy val batch: SpriteBatch = new SpriteBatch()
  private lazy val texture = new Texture("libgdxlogo.png")
  texture.setFilter(TextureFilter.Linear, TextureFilter.Linear)

  private lazy val region = new TextureRegion(texture, 0, 0,
    texture.getWidth, texture.getHeight)
  private lazy val sprite = new Sprite(region)

  private lazy val font = {
    val f = new BitmapFont()
    f.getData.setScale(2f)
    f
  }

  override def show(): Unit = {
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