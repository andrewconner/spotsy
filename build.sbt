name := """spot"""

version := "0.1"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  jdbc,
  cache,
  ws,
  specs2 % Test,
  "com.typesafe.slick" %% "slick" % "3.1.1",
  //"com.zaxxer" % "HikariCP" % "2.4.1",
  "com.typesafe.slick" % "slick-hikaricp_2.11" % "3.1.1",
  "com.typesafe.play" %% "play-slick" % "2.0.0",
  "com.h2database" % "h2" % "1.4.191",
  "com.softwaremill.macwire" %% "macros" % "2.2.2" % "provided",
  "com.softwaremill.macwire" %% "util" % "2.2.2",
  "com.softwaremill.macwire" %% "proxy" % "2.2.2",
  "org.typelevel" %% "cats" % "0.4.1",
  "com.amazonaws" % "aws-java-sdk" % "1.10.65",
  "org.scalaz" %% "scalaz-concurrent" % "7.2.1",
  "org.feijoas" %% "mango" % "0.12"
)

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"
resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

scalacOptions in ThisBuild ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",       // yes, this is 2 args
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",        // N.B. doesn't work well with the ??? hole
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Xfuture"
  //"-Yno-predef"   // no automatic import of Predef (removes irritating implicits)
)

routesGenerator := InjectedRoutesGenerator

import scalariform.formatter.preferences._

scalariformSettings
excludeFilter in scalariformFormat := (excludeFilter in scalariformFormat).value ||
  "Routes.scala" ||
  "ReverseRoutes.scala" ||
  "JavaScriptReverseRoutes.scala" ||
  "RoutesPrefix.scala"

/////////////////////////////////////////////////
// Things to enable later:

//wartremoverWarnings in (Compile, compile) ++= Warts.unsafe
wartremoverExcluded += sourceManaged.value

