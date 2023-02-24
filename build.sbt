import Dependencies._

lazy val root = project
  .in(file("."))
  .settings(
    name := "web-game",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := "3.2.2",
    libraryDependencies += scalaTest % Test
  )
