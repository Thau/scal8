name := "scal8"

version := "1.0"
scalaVersion := "2.12.8"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

// Add dependency on ScalaFX library
libraryDependencies += "org.scalafx" %% "scalafx" % "11-R16"

// Determine OS version of JavaFX binaries
lazy val osName = System.getProperty("os.name") match {
  case n if n.startsWith("Linux")   => "linux"
  case n if n.startsWith("Mac")     => "mac"
  case n if n.startsWith("Windows") => "win"
  case _ => throw new Exception("Unknown platform!")
}

libraryDependencies += "org.openjfx" % "javafx-base" % "11" classifier osName
libraryDependencies += "org.openjfx" % "javafx-controls" % "11" classifier osName
libraryDependencies += "org.openjfx" % "javafx-media" % "11" classifier osName
libraryDependencies += "org.openjfx" % "javafx-graphics" % "11" classifier osName

assemblyMergeStrategy in assembly := {
  case x if x.startsWith("META-INF") => MergeStrategy.discard
  case _ => MergeStrategy.first
}
