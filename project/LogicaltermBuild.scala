import sbt._
import sbt.Keys._

object LogicaltermBuild extends Build {

  lazy val logicalterm = Project(
    id = "logicalterm",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      name := "LogicalTerm",
      organization := "pl.luckboy.logicalterm",
      version := "0.1-SNAPSHOT",
      scalaVersion := "2.10.1",
      // add other settings here
      libraryDependencies += "org.scalaz" % "scalaz-core_2.10" % "7.0.0",
      libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test",
      libraryDependencies += "org.scala-lang" % "jline" % "2.10.1",
      scalacOptions ++= Seq("-feature", "-language:postfixOps", "-language:higherKinds", "-language:implicitConversions", "-unchecked")
    )
  )
}
