ThisBuild / scalaVersion := "2.13.6"

addCompilerPlugin("com.github.cb372" % "scala-typed-holes" % "0.1.9" cross CrossVersion.full)
scalacOptions += "-P:typed-holes:log-level:info"
lazy val root = (project in file("."))
  .aggregate(exercises, answers)
  .settings(
    name := "fpinscala"
  )

lazy val exercises = (project in file("exercises"))
  .settings(
    name := "exercises",
    addCompilerPlugin("com.github.cb372" % "scala-typed-holes" % "0.1.9" cross CrossVersion.full),
  )

lazy val answers = (project in file("answers"))
  .settings(
    name := "answers"
  )
