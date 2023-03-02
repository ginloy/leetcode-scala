ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "leetcode-scala",
    assembly / assemblyJarName := "ImmutableCollections.jar",
  )

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.3.7"