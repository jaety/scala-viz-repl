name := "scala-viz-repl"

lazy val commonSettings = Seq(
  version := "1.0",
  scalaVersion := "2.11.8",
  scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")
)

run <<= run in Compile in core

runMain <<= runMain in Compile in core

initialCommands in (Test, console) := """ammonite.Main().run()"""

lazy val macros = (project in file("macro")).settings(
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
).settings(commonSettings: _*)

lazy val core = (project in file("core")).dependsOn(macros).settings(
  commonSettings: _*
).settings (
  libraryDependencies ++= {
    val akkaV = "2.3.9"
    val sprayV = "1.3.3"
    Seq(
      "io.spray"            %%  "spray-can"     % sprayV,
      "io.spray"            %%  "spray-routing" % sprayV,
      "io.spray"            %%  "spray-testkit" % sprayV  % "test",
      "com.typesafe.akka"   %%  "akka-actor"    % akkaV,
      "com.typesafe.akka"   %%  "akka-testkit"  % akkaV   % "test",
      "org.specs2"          %%  "specs2-core"   % "2.3.11" % "test",
      "net.liftweb" % "lift-json_2.11" % "3.0.1",
      "com.lihaoyi" %% "ammonite" % "0.8.1" % "test" cross CrossVersion.full,
      "com.github.tototoshi" %% "scala-csv" % "1.3.4",
      "org.plotly-scala" %% "plotly-render" % "0.3.0",
      "org.json4s" %% "json4s-native" % "3.5.0"
    )
  }
)

Revolver.settings

