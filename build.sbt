val scala3Version = "3.0.0-M2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "aoc2020",
    version := "0.1.0",

    scalaVersion := scala3Version,

    libraryDependencies += "org.jsoup" % "jsoup" % "1.13.1",

    libraryDependencies += "org.scalameta" %% "munit" % "0.7.20" % Test,

    testFrameworks += new TestFramework("munit.Framework")
  )
