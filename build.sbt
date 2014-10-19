name := "mrswadesh"

version := "0.1"

organization := "edu.berkeley.linguistics"

scalaVersion := "2.11.2"

resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.ivy2/local"

resolvers += "Maven Repository" at "http://repo1.maven.org/maven2/"

resolvers += "Sonatype" at "https://oss.sonatype.org/content/groups/scala-tools/"

libraryDependencies += "org.scalanlp" %% "breeze" % "0.10"

libraryDependencies += "net.sf.opencsv" % "opencsv" % "2.0"
