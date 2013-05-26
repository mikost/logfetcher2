organization := "name.mikkoostlund.utils"

name := "logfetcher2"

scalaVersion := "2.10.1"

resolvers += Resolver.sonatypeRepo("releases")

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies ++= Seq( "com.jsuereth" %% "scala-arm" % "1.3",
                             "org.scalatest" %% "scalatest" % "1.9.1" % "test", 
                             "org.scalamock" %% "scalamock-scalatest-support" % "3.1-SNAPSHOT" % "test"
                           )
