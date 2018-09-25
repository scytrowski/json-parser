name := "json-parser"

version := "0.1"

scalaVersion := "2.12.6"

lazy val testDevDependencies = Seq(
  // https://mvnrepository.com/artifact/org.scalacheck/scalacheck
  "org.scalacheck" %% "scalacheck" % "1.14.0",
  // https://mvnrepository.com/artifact/org.scalatest/scalatest
  "org.scalatest" %% "scalatest" % "3.2.0-SNAP10"
)

lazy val testDependencies = testDevDependencies.map(_ % Test)

libraryDependencies ++= testDependencies

lazy val test_utils = (project in file("test-utils"))
  .settings(libraryDependencies ++= testDevDependencies)

lazy val core = (project in file("core"))
  .settings(libraryDependencies ++= testDependencies)

lazy val tokenizer = (project in file("tokenizer"))
  .dependsOn(core, test_utils)
  .settings(libraryDependencies ++= testDependencies)

lazy val parser = (project in file("parser"))
  .dependsOn(core, test_utils)
  .settings(libraryDependencies ++= testDependencies)

lazy val formats = (project in file("formats"))
  .dependsOn(core, test_utils)
  .settings(libraryDependencies ++= testDependencies)

lazy val facade = (project in file("facade"))
  .dependsOn(core, tokenizer, parser)
  .settings(libraryDependencies ++= testDependencies)
