name := """cryptocurrbot"""
organization := "com.davaux"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala).disablePlugins(PlayFilters)

scalaVersion := "2.12.3"

libraryDependencies += guice
libraryDependencies += "org.scalatestplus.play" %% "scalatestplus-play" % "3.1.2" % Test
libraryDependencies += ws
libraryDependencies ++= Seq("com.roundeights" %% "hasher" % "1.2.0")
// Adds additional packages into Twirl
//TwirlKeys.templateImports += "com.davaux.controllers._"

// Adds additional packages into conf/routes
// play.sbt.routes.RoutesKeys.routesImport += "com.davaux.binders._"
