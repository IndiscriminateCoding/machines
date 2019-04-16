organization := "com.github.IndiscriminateCoding"
name := "machines"
crossScalaVersions := Seq("2.12.8")
scalacOptions ++= Seq(
  "-language:higherKinds",
  "-Ypartial-unification",
  "-P:acyclic:force"
)
version := "0.0.1"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.8")
addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.2.4")
addCompilerPlugin("com.lihaoyi" %% "acyclic" % "0.1.8")

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "1.5.0",

  "org.scalatest" %% "scalatest" % "3.0.5" % Test,
  "org.scalacheck" %% "scalacheck" % "1.14.0" % Test
)
