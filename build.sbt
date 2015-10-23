
lazy val root = (project in file(".")).
  settings(
    name            := "scala-generative-systems",
    version         := "1.0",
    scalaVersion    := "2.11.7"
  )

lazy val bench = (project in file("bench"))
  .settings (
    scalaVersion := "2.11.7"
  )
  .enablePlugins(JmhPlugin)
  .dependsOn(root)