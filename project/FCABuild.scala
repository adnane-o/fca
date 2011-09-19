import sbt._

import Keys._

object BuildSettings {
  val buildOrganization = "rpr"
  val buildScalaVersion = "2.9.1"
  val buildVersion      = "0.0.1"

  val buildSettings = Defaults.defaultSettings ++ Seq (organization := buildOrganization,
						       //shellPrompt  := ShellPrompt.buildShellPrompt,
						       scalaVersion := buildScalaVersion,
						       version      := buildVersion)
}

object Dependencies {
  val scalatest		= "org.scalatest" % "scalatest_2.9.0" % "1.4.1" % "test"
  val apachecodec	= "commons-codec" % "commons-codec" % "1.4"
  val logging           = "rpr" % "scalog_2.9.1" % "1.0.1"
}

object FCABuild extends Build {
  import BuildSettings._
  import Dependencies._

  val fcaDeps = Seq (apachecodec, logging, scalatest)

  lazy val fca = Project ("fca", file ("."), 
                          settings = buildSettings ++ 
                          Seq (libraryDependencies := fcaDeps)) aggregate ()

}
