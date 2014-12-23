name := "Reversi"

version := "1.0"

scalaVersion := "2.11.4"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.0"

libraryDependencies += "org.specs2" %% "specs2-scalacheck" % "2.4.13"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.6" % "test"

libraryDependencies += "org.scalaz" %% "scalaz-scalacheck-binding" % "7.1.0" % "test"

libraryDependencies += "org.typelevel" %% "scalaz-specs2" % "0.3.0" % "test"

libraryDependencies += "org.scalafx" %% "scalafx" % "8.0.20-R6"

// libraryDependencies += "org.reactfx" % "reactfx" % "1.4.1"

libraryDependencies += "org.fxmisc.easybind" % "easybind" % "1.0.3"

// libraryDependencies += "com.scalarx" %% "scalarx" % "0.2.6"

fork := true
