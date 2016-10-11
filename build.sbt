name := "scala-commonmark root project"

scalaVersion in ThisBuild := "2.11.8"

lazy val root = project.in(file(".")).
  aggregate(scalaCommonMarkJS, scalaCommonMarkJVM).
  settings(
    publish := {},
    publishLocal := {}
  )

lazy val scalaCommonMark = crossProject.in(file(".")).
  settings(
    scalaVersion := "2.11.8",
    name := "scala-commonmark",
    version := "0.1-SNAPSHOT",
    libraryDependencies += "org.parboiled" %%% "parboiled" % "2.1.3",
    libraryDependencies += "com.chuusai"   %%% "shapeless" % "2.3.2",
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.0" % "test"
  ).
  jvmSettings(
  ).
  jsSettings(
  )

lazy val scalaCommonMarkJVM = scalaCommonMark.jvm
lazy val scalaCommonMarkJS = scalaCommonMark.js