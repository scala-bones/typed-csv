val dottyVersion = "0.27.0-RC1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "typed-csv",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    libraryDependencies ++= Seq(
      "com.novocode" % "junit-interface" % "0.11" % Test,
      "org.scalacheck" % "scalacheck_2.13" % "1.14.1" % "test",
      "org.scalatest" % "scalatest-funsuite_2.13" % "3.2.0" % "test",
      "org.scalatest" % "scalatest-mustmatchers_2.13" % "3.2.0" % "test"
    )
  )
