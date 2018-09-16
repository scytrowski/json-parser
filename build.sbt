name := "json-parser"

version := "0.1"

scalaVersion := "2.12.6"

lazy val testDependencies = Seq(
  // https://mvnrepository.com/artifact/org.scalacheck/scalacheck
  "org.scalacheck" %% "scalacheck" % "1.14.0" % Test,
  // https://mvnrepository.com/artifact/org.scalatest/scalatest
  "org.scalatest" %% "scalatest" % "3.2.0-SNAP10" % Test
)

libraryDependencies ++= testDependencies