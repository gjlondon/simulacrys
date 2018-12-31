package com.thrive.game

import com.badlogic.gdx.graphics.g2d.{BitmapFont, SpriteBatch}
import com.badlogic.gdx.graphics.{GL20, OrthographicCamera, Texture}
import com.badlogic.gdx.{Game, Gdx, ScreenAdapter}
import com.thrive.simulation.person.Commoner
import org.joda.time.DateTime

class ThriveApp extends Game {

    lazy val sprite = new Texture("libgdxlogo.png")
    lazy val batch = new SpriteBatch

    override def create(): Unit = {
        setScreen(new MainScreen)
    }

//    override def render(): Unit = {
//        Gdx.gl.glClearColor(0.4f + MathUtils.random()*0.2f,0.4f + MathUtils.random()*0.2f,0.4f + MathUtils.random()*0.2f,1f)
//        Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT)
//        batch.begin()
//        batch.draw(sprite, (Gdx.graphics.getWidth - sprite.getWidth) / 2f, (Gdx.graphics.getHeight - sprite.getHeight) / 2f)
//        batch.end()
//    }
}

class MainScreen extends ScreenAdapter {

    private lazy val camera = new OrthographicCamera()
    private val batch: SpriteBatch = new SpriteBatch()

    private lazy val font = {
        val f = new BitmapFont()
        f.getData.setScale(2f)
        f
    }

    override def show(): Unit = {
        camera.setToOrtho(false, 800, 480)
    }

    override def render(delta: Float): Unit = {
        Gdx.gl.glClearColor(0, 0, 0.5f, 1)
        Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT)
        camera.update()
        batch.setProjectionMatrix(camera.combined)
        batch.begin()
        val fred = Commoner.randomCommoner(DateTime.now)
        font.draw(batch, fred.name, 0, font.getCapHeight)
        batch.end()
    }

}