ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13"

lazy val root = (project in file("."))
  .settings(
    name := "leetcode-scala"
  )

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.3.7"