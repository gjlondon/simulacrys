import java.io.File

import sbt.Keys.libraryDependencies




val buildSettings = Defaults.coreDefaultSettings ++ Seq(
  javaOptions += "-Xmx20G",
)


sourceGenerators in Test += Def.task {
  val file = (sourceManaged in Test).value / "amm.scala"
  IO.write(file, """object amm extends App { ammonite.Main.main(args) }""")
  Seq(file)
}.taskValue

// Optional, required for the `source` command to work
(fullClasspath in Test) ++= {
  (updateClassifiers in Test).value
    .configurations
    .find(_.configuration == Test.name)
    .get
    .modules
    .flatMap(_.artifacts)
    .collect{case (a, f) if a.classifier == Some("sources") => f}
}


import java.io.File
import scala.collection.convert.wrapAll._

//lazy import com.google.common.base.Charsets
//lazy import com.google.common.io.Files
//lazy import org.apache.commons.io.FileUtils

val libgdxVersion = "1.9.9"

lazy val sharedSettings: Seq[Def.Setting[_]] = Seq(

  name := "simulacrys",
  version := "0.1",
  scalaVersion := "2.12.7",

  assetsDirectory := {
    val r = file("assets")
    IO.createDirectory(r)
    r
  },
  libraryDependencies ++= Seq(
    "com.badlogicgames.gdx" % "gdx" % libgdxVersion
  ),
  libraryDependencies ++= Seq("commons-io" % "commons-io" % "2.6", "com.google.guava" % "guava" % "23.0"),
  libraryDependencies += "org.typelevel"  %% "squants"  % "1.3.0",
  libraryDependencies += {
    val version = scalaBinaryVersion.value match {
      case "2.10" => "1.0.3"
      case _ ⇒ "1.3.3"
    }
    "com.lihaoyi" % "ammonite" % version % "test" cross CrossVersion.full
  },

  libraryDependencies += "com.github.nscala-time" %% "nscala-time" % "2.20.0",

  javacOptions ++= Seq(
    "-Xlint",
    "-encoding", "UTF-8",
    "-source", "1.6",
    "-target", "1.6"
  ),
  scalacOptions ++= Seq(
    "-Xlint",
    "-Ywarn-dead-code",
    "-Ywarn-value-discard",
    "-Ywarn-numeric-widen",
    "-Ywarn-unused",
    "-Ywarn-unused-import",
    "-unchecked",
    "-deprecation",
    "-feature",
    "-encoding", "UTF-8",
    "-target:jvm-1.6"
  ),
  cancelable := true,
  exportJars := true
)

lazy val core = project in file("core") settings (sharedSettings: _*)

lazy val desktop = project in file("desktop") settings (sharedSettings: _*) dependsOn core settings(
  name := (name in core).value + "-desktop",
  libraryDependencies ++= Seq(
    "net.sf.proguard" % "proguard-base" % "5.1" % "provided",
    "com.badlogicgames.gdx" % "gdx-backend-lwjgl" % libgdxVersion,
    "com.badlogicgames.gdx" % "gdx-platform" % libgdxVersion classifier "natives-desktop"
  ),
//  fork in Compile := true,
  baseDirectory in run := assetsDirectory.value,
  unmanagedResourceDirectories in Compile += assetsDirectory.value,
  assembly := {
    (fullClasspath in Runtime).value // Just to make sure that compile finished
    val log = streams.value.log
    val up = (update in Compile).value
    val provided = Set(up.select(configurationFilter("provided")):_*)
    val compile = Set(up.select(configurationFilter("compile")):_*)
    val runtime = Set(up.select(configurationFilter("runtime")):_*)
    val optional = Set(up.select(configurationFilter("optional")):_*)
    val onlyProvidedNames = provided -- compile -- runtime -- optional
    val (onlyProvided, withoutProvided) = (dependencyClasspath in Compile).value.partition(cpe => onlyProvidedNames contains cpe.data)
    val exclusions = Seq("!META-INF/MANIFEST.MF", "!library.properties").mkString(",")
    val inJars = withoutProvided.map("\""+_.data.absolutePath+"\"("+exclusions+")").mkString(File.pathSeparator)
    val libraryJars = onlyProvided.map("\""+_.data.absolutePath+"\"").mkString(File.pathSeparator)
    val outfile = "\""+(target.value/"%s-%s.jar".format((name in core).value, version.value)).absolutePath+"\""
    val classfiles = "\"" + (classDirectory in Compile).value.absolutePath + "\""
    val manifest = "\"" + file("desktop/manifest").absolutePath + "\""
//    val proguardOptions = Files.readLines(file("core/proguard-project.txt"), Charsets.UTF_8) ++
//      Files.readLines(file("desktop/proguard-project.txt"), Charsets.UTF_8)
//    val proguard = (javaOptions in Compile).value ++ Seq("-cp", Path.makeString((managedClasspath in Compile).value.files), "proguard.ProGuard") ++ proguardOptions ++ Seq(
//      "-injars", classfiles,
//      "-injars", inJars,
//      "-injars", manifest,
//      "-libraryjars", libraryJars,
//      "-outjars", outfile)
//    log.info("preparing proguarded assembly")
//    log.debug("Proguard command:")
//    log.debug("java "+proguard.mkString(" "))
//    val exitCode = Process("java", proguard) ! log
//    if (exitCode != 0) {
//      sys.error("Proguard failed with exit code [%s]" format exitCode)
//    } else {
//      log.info("Output file: "+outfile)
//    }
  }
)


lazy val assetsDirectory = settingKey[File]("Directory with game's assets")

lazy val nativesDirectory = settingKey[File]("Directory where android natives are extracted to")

lazy val extractNatives = taskKey[Unit]("Extracts natives to nativesDirectory")

lazy val assembly = TaskKey[Unit]("assembly", "Assembly desktop using Proguard")

lazy val all = project in file(".") aggregate(core, desktop)