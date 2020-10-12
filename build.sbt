import sbt.Resolver

organization  := "ch.unibas.cs.gravis"

name := "Shape-Modeling-FAUST"

version       := "0.1"

scalaVersion  := "2.12.8"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

resolvers ++= Seq(
  Resolver.bintrayRepo("unibas-gravis", "maven"),
  Resolver.bintrayRepo("cibotech", "public"),
  Opts.resolver.sonatypeSnapshots
)


libraryDependencies  ++= Seq(
  "ch.unibas.cs.gravis" % "scalismo-native-all" % "4.0.+",
  "ch.unibas.cs.gravis" %% "scalismo-ui" % "0.14-RC1",
  "com.cibo" %% "evilplot" % "0.6.3",
  "com.opencsv" % "opencsv" % "5.2"
)
