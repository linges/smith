name := "smith"

version := "0.1"

scalaVersion := "2.10.0"

parallelExecution in Test := false

scalacOptions ++= Seq("-unchecked", "-feature", "-language:implicitConversions", 
"-language:postfixOps")

initialize ~= { _ => sys.props("scalac.patmat.analysisBudget") = "off" }

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % "1.9.1",
	"com.googlecode.kiama" %% "kiama" % "1.4.0",
    "org.scalaz" %% "scalaz-core" % "6.0.4",
    "net.sf.jgrapht" % "jgrapht" % "0.8.3"
)
