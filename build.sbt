name := "scalaxiangqi"
organization := "com.github.katoue"
version := "0.1.0"

scalaVersion := "3.4.2"

libraryDependencies ++= Seq(
  "org.scalameta" %% "munit" % "1.0.0" % Test
)

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked"
)

testFrameworks += new TestFramework("munit.Framework")

publishMavenStyle := true
publishTo := Some(Resolver.defaultNonRootMavenResolver)
