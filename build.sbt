name := "Reversi"

version := "1.0"

scalaVersion := "2.11.4"

// initialCommands in console := "import scalaz._, Scalaz._, nightra.reversi._, model._, ai._, util._"

resolvers += "bintray/non" at "http://dl.bintray.com/non/maven"

// Kind projector! Yay!
addCompilerPlugin("org.spire-math" % "kind-projector_2.11" % "0.5.2")

assemblyJarName in assembly := "Reversi.jar"

mainClass in assembly := Some("nightra.reversi.ui.UI")

proguardSettings

ProguardKeys.proguardVersion in Proguard := "5.0"

ProguardKeys.options in Proguard ++= Seq("-dontnote", "-dontwarn", "-ignorewarnings")

ProguardKeys.options in Proguard += ProguardOptions.keepMain("nightra.reversi.ui.UI")

net.virtualvoid.sbt.graph.Plugin.graphSettings

javaOptions in (Proguard, proguard) := Seq("-Xms2G","-Xmx2G")

libraryDependencies += ("org.scalaz" %% "scalaz-core" % "7.1.0"
                          exclude("org.scala-lang.modules", "scala-parser-combinators_2.11")
                          exclude("org.scala-lang.modules", "scala-xml_2.11"))

libraryDependencies += "org.scalaz" %% "scalaz-effect" % "7.1.0"

libraryDependencies += "org.scalaz" %% "scalaz-concurrent" % "7.1.0"

libraryDependencies += "org.specs2" %% "specs2-scalacheck" % "2.4.13" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.6" % "test"

libraryDependencies += "org.scalaz" %% "scalaz-scalacheck-binding" % "7.1.0" % "test"

libraryDependencies += "org.typelevel" %% "scalaz-specs2" % "0.3.0" % "test"

libraryDependencies += "org.scalafx" %% "scalafx" % "8.0.20-R6"

// libraryDependencies += "org.reactfx" % "reactfx" % "1.4.1"

// libraryDependencies += "org.fxmisc.easybind" % "easybind" % "1.0.3"

// libraryDependencies += "com.scalarx" %% "scalarx" % "0.2.6"

libraryDependencies += "org.abego.treelayout" % "org.abego.treelayout.core" % "1.0.1"

fork := true
