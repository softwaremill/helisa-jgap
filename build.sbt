import sbt.url

organization := "com.softwaremill"
name := "helisa-jgap"

scalaVersion := "2.12.8"

lazy val repoUrl = "https://github.com/softwaremill/helisa-jgap"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.6")

scalacOptions += "-Ypartial-unification"

val jgapDeps = Seq(
  "org.jdesktop"             % "appframework"  % "1.0.3",
  "commons-lang"             % "commons-lang"  % "2.3",
  "log4j"                    % "log4j"         % "1.2.9" % "runtime",
  "commons-codec"            % "commons-codec" % "1.3",
  "org.apache.commons"       % "commons-math"  % "2.0",
  "net.sf.trove4j"           % "trove4j"       % "2.0.2",
  "xstream"                  % "xstream"       % "1.2.2",
  "xpp3"                     % "xpp3"          % "1.1.4c",
  "xpp3"                     % "xpp3"          % "1.1.3.4.O",
  "commons-cli"              % "commons-cli"   % "1.2",
  "tablelayout"              % "TableLayout"   % "20050920",
  "net.sf.jcgrid"            % "jcgrid"        % "0.05",
  "jfree"                    % "jfreechart"    % "1.0.12",
  "com.thoughtworks.xstream" % "xstream"       % "1.3.1"
)

val coreDeps = Seq("com.chuusai" %% "shapeless" % "2.3.2") ++
  Seq("cats-core", "alleycats-core").map("org.typelevel" %% _ % "1.1.0")

lazy val akkaVersion = "2.5.17"
val apiDeps = Seq("com.typesafe.akka" %% "akka-stream" % akkaVersion,
  "co.fs2" %% "fs2-core" % "1.0.0").map(_ % Optional)

val testDeps = Seq("org.scalatest" %% "scalatest" % "3.0.5",
  "org.scalacheck" %% "scalacheck" % "1.14.0",
  "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.6").map(_ % "test")

libraryDependencies ++= jgapDeps ++ coreDeps ++ apiDeps ++ testDeps


//Sonatype OSS stuff (based on https://github.com/xerial/sbt-sonatype )
publishTo := Some(
  if (isSnapshot.value)
    Opts.resolver.sonatypeSnapshots
  else
    Opts.resolver.sonatypeStaging
)

publishMavenStyle := true

licenses := Seq("LGPL" -> url("https://www.gnu.org/copyleft/lesser.html"))
homepage := Some(url(repoUrl))
scmInfo := Some(
  ScmInfo(
    url(repoUrl),
    "scm:git@github.com:softwaremill/helisa-jgap.git"
  )
)
developers := List(
  Developer(id = "mikolak-net", name = "Mikołaj Koziarkiewicz", email = "", url = url("https://softwaremill.com"))
)