name := "church-and-state"

organization := "io.underscore"

version := "0.1-SNAPSHOT"

scalaVersion in ThisBuild := "2.12.1"

lazy val core = project

lazy val benchmarks = project.
  dependsOn(core % "compile->test").
  enablePlugins(JmhPlugin)
