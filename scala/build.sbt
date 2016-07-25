name := "scala-slackbots"

version := "1.0"

scalaVersion := "2.11.8"

resolvers ++= Seq(
  "Tatami Releases" at "https://raw.github.com/cchantep/tatami/master/releases/"
)

libraryDependencies ++= Seq(
  "com.tumblr" %% "colossus" % "0.8.0",
  "foorgol" %% "scala" % "1.0.5"
)


enablePlugins(JavaAppPackaging)
