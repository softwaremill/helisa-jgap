name := "sgap"
version := "0.1"

scalaVersion := "2.12.4"

addCompilerPlugin("io.tryp"        % "splain"          % "0.2.7" cross CrossVersion.patch)
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.6")

scalacOptions += "-P:splain:implicits:true"
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

val coreDeps = Seq("com.chuusai" %% "shapeless" % "2.3.2", "org.typelevel" %% "cats-core" % "1.1.0")

libraryDependencies ++= jgapDeps ++ coreDeps
