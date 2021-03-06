name := "holidays"
 
version := "1.0" 
      
lazy val `holidays` = (project in file(".")).enablePlugins(PlayScala)

resolvers += "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases"
      
resolvers += "Akka Snapshot Repository" at "https://repo.akka.io/snapshots/"
      
scalaVersion := "2.12.2"

libraryDependencies ++= Seq( jdbc , ehcache , ws , specs2 % Test , guice, "org.jsoup" % "jsoup" % "1.13.1", "org.scalaj" %% "scalaj-http" % "2.4.2", "org.webjars" % "bootstrap" % "3.3.7"
)

unmanagedResourceDirectories in Test <+=  baseDirectory ( _ /"target/web/public/test" )


      