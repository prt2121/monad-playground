name          := "zzz"

organization  := "prt2121"

version       := "0.1.0"

scalaVersion  := "2.11.8"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-encoding",
  "utf8",
  "-language:_"
)

libraryDependencies ++= {
  val scalazV = "7.2.1"
  Seq(
    "org.scalaz"  %%  "scalaz-core"                 % scalazV,
    //"org.scalaz"  %%  "scalaz-effect"               % scalazV,
    //"org.scalaz"  %%  "scalaz-concurrent"           % scalazV,
    //"org.scalaz"  %%  "scalaz-stream"               % scalazV,
    "org.scalaz"  %%  "scalaz-scalacheck-binding"   % scalazV  % "test"
  )
}

libraryDependencies += "org.typelevel" %% "cats" % "0.4.1"
libraryDependencies += "com.github.mpilquist" %% "simulacrum" % "0.7.0"