scalaVersion := "2.11.8"

name := "eff-stoch"
organization := "uk.co.turingatemyhamster"
version := "0.1.0"

libraryDependencies += "org.atnos" %% "eff" % "4.0.2"
libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.2"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")

scalaOrganization in ThisBuild := "org.typelevel"

scalacOptions += "-Ypartial-unification"
scalacOptions += "-Ywarn-unused-import"

