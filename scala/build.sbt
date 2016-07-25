name := "scala-slackbots"

version := "1.0"

scalaVersion := "2.11.8"


libraryDependencies ++= Seq(
  "com.tumblr" %% "colossus" % "0.8.0",
  "net.databinder.dispatch" %% "dispatch-core" % "0.11.2",
  "com.typesafe.play" %% "play-json" % "2.5.4"
)


enablePlugins(JavaAppPackaging)
