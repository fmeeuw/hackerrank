name := "HackerRank"

version := "1.0"

scalaVersion := "2.12.2"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.5"

scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation")