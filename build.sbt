name := "simulacrys"

version := "0.1"

scalaVersion := "2.12.7"

libraryDependencies += "org.typelevel"  %% "squants"  % "1.3.0"

val buildSettings = Defaults.coreDefaultSettings ++ Seq(
  javaOptions += "-Xmx20G",
)

libraryDependencies += {
  val version = scalaBinaryVersion.value match {
    case "2.10" => "1.0.3"
    case _ ⇒ "1.3.3"
  }
  "com.lihaoyi" % "ammonite" % version % "test" cross CrossVersion.full
}

libraryDependencies += "com.github.nscala-time" %% "nscala-time" % "2.20.0"

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