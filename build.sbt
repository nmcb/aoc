Global / scalaVersion := "3.8.3"
Global / version      := "0.1.0"

Global / libraryDependencies ++= Seq(
  "org.scalatest"  %% "scalatest"  % "3.2.20" % "test",

  // 2025
  "tools.aqua" % "z3-turnkey" % "4.14.1",

  // 2023
  ("org.scala-graph" %  "graph-core" % "2.0.3").cross(CrossVersion.for3Use2_13),
  ("org.scala-graph" %  "graph-dot"  % "2.0.0").cross(CrossVersion.for3Use2_13),

  // 2022
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.2.0"
)

Global / scalacOptions ++= Seq(
  "-encoding", "utf8",
  "-feature",
  "-language:implicitConversions",
  "-language:existentials",
  "-language:strictEquality",
  "-unchecked",
  "-Werror",
  "-deprecation"
)

Global / run / fork         := true
Global / run / javaOptions ++= Seq("-Xmx4G", "-Xss1G", "-XX:+UseG1GC")

Global / fork         := true
Global / javaOptions ++= Seq("-Xmx4G", "-Xss1G", "-XX:+UseG1GC")
Global / testOptions  += Tests.Argument("-oD")

Global / onChangedBuildSource   := ReloadOnSourceChanges

lazy val input    = project.in(file("input"))
lazy val nmcb     = project.in(file("nmcb"))
lazy val examples = project.in(file("examples"))
lazy val aoc2015  = project.in(file("2015")).aggregate(input, nmcb)
lazy val aoc2016  = project.in(file("2016")).dependsOn(input, nmcb)
lazy val aoc2017  = project.in(file("2017")).dependsOn(input, nmcb)
lazy val aoc2018  = project.in(file("2018")).dependsOn(input, nmcb)
lazy val aoc2019  = project.in(file("2019")).dependsOn(input, nmcb)
lazy val aoc2020  = project.in(file("2020")).dependsOn(input, nmcb)
lazy val aoc2021  = project.in(file("2021")).dependsOn(input, nmcb)
lazy val aoc2022  = project.in(file("2022")).dependsOn(input, nmcb)
lazy val aoc2023  = project.in(file("2023")).dependsOn(input, nmcb)
lazy val aoc2024  = project.in(file("2024")).dependsOn(input, nmcb)
lazy val aoc2025  = project.in(file("2025")).dependsOn(input, nmcb)