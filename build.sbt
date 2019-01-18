name := "herding-cats"

version := "0.1"

scalaVersion := "2.12.4"

scalacOptions += "-Ypartial-unification"

val circeVersion = "0.10.0"

libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.1"

libraryDependencies ++= Seq(
	"io.circe" %% "circe-core",
	"io.circe" %% "circe-generic",
	"io.circe" %% "circe-parser"
).map(_ % circeVersion)

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.7")


