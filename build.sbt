ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.2"

libraryDependencies += "com.chuusai"    %% "shapeless"  % "2.3.9"
libraryDependencies += "org.typelevel"  %% "cats-core"       % "2.7.0"

scalacOptions ++= Seq(
  "-encoding", "utf8", // Option and arguments on same line
  "-Xfatal-warnings",  // New lines for each options
  "-deprecation",
  "-unchecked",
  "-language:implicitConversions",
  "-language:higherKinds",
  "-language:existentials",
  "-language:postfixOps",
  "-Xlog-implicits"
)

lazy val root = (project in file("."))
  .settings(
    name := "scala-shapeless-icecream"
  )
