name := "Paermar"

version := "0.1"

scalaVersion := "2.10.3"

seq(Twirl.settings: _*)

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "Typesafe Snapshot Repository" at "http://repo.typesafe.com/typesafe/snapshots/"

resolvers += "spray" at "http://repo.spray.io/"

resolvers += Resolver.sonatypeRepo("snapshots")

resolvers += "spray repo" at "http://repo.spray.io"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.2"

libraryDependencies += "com.typesafe.slick" %%  "slick" % "2.0.1"

libraryDependencies += "mysql" % "mysql-connector-java" % "5.1.30"

libraryDependencies += "io.spray" % "spray-routing" % "1.3.1"

libraryDependencies += "io.spray" %% "spray-json" % "1.2.6"

libraryDependencies += "io.spray" % "spray-can" % "1.3.1"

libraryDependencies += "joda-time" % "joda-time" % "2.3"

libraryDependencies += "org.joda" % "joda-convert" % "1.5"

libraryDependencies += "com.github.tototoshi" %% "slick-joda-mapper" % "1.0.1"

// libraryDependencies += "org.scala-lang" %% "scala-pickling" % "0.8.0-SNAPSHOT"