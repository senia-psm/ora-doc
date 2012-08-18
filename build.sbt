seq(ProguardPlugin.proguardSettings :_*)

name := "oraParser"

version := "0.0.1"

mainClass := Some("senia.oracle.autodoc.Parse")

proguardOptions += keepMain("senia.oracle.autodoc.Parse")

proguardOptions ++= Seq(
"-keep class sun.misc.Unsafe{  *;}",
"-keep public class akka.actor.LocalActorRefProvider {  public <init>(...);}",
"-keep public class akka.remote.RemoteActorRefProvider {  public <init>(...);}",
"-keep class akka.actor.SerializedActorRef {  *;}",
"-keep class akka.remote.netty.NettyRemoteTransport {  *;}",
"-keep class akka.serialization.JavaSerializer {  *;}",
"-keep class akka.serialization.ProtobufSerializer {  *;}",
"-keep class com.google.protobuf.GeneratedMessage {  *;}",
"-keep class akka.event.Logging*",
"-keep class akka.event.Logging$LogExt{  *;}")

proguardOptions ++= Seq("-keep class org.pegdown.** { *; }", "-keep class org.clapper.markwrap.** { *; }", "-keep class org.parboiled.** { *; }") 

libraryDependencies += "org.clapper" %% "markwrap" % "[0.5.4,)"

scalacOptions += "-deprecation"

resolvers += "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "6.0.4"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "com.typesafe.akka" % "akka-actor" % "2.0.3"

libraryDependencies += "org.apache.httpcomponents" % "httpclient" % "4.2.1"