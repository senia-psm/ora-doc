package senia.oracle.autodoc

import akka.actor.ActorSystem
import akka.dispatch.{Future, Await}
import akka.util.duration._
import scala.util.control.Exception.allCatch
import java.io.{File, PrintWriter, OutputStreamWriter, FileOutputStream}

object Parse extends App {
  def closing[T](s: io.BufferedSource)(f: io.BufferedSource => T) = allCatch.andFinally(s.close())(f(s))
  def readFile[T](f: File) = closing(io.Source.fromFile(f)(io.Codec.string2codec("cp1251"))){ in => in.mkString }
  def withActorSystem[T](name: String)(f: ActorSystem => T ) = {
    val system = ActorSystem(name)
    allCatch.andFinally(system.shutdown())(f(system))
  }
  def printToFile[T](f: File)(op: PrintWriter => T) = { 
    val p = new PrintWriter(new OutputStreamWriter(new FileOutputStream(f), "UTF-8"))
    p.print('\ufeff'); // BOM!!!
    allCatch.andFinally(p.close())(op(p))
  }
  
  def processFileAsync(file: File)( implicit s: ActorSystem ): Future[xml.Elem] = file match {
    case f if f.isDirectory() => Future.sequence(for {subF <- f.listFiles().toSeq} yield processFileAsync(subF)).map{ l => <directory name={ f.getName }>{ l }</directory> }
    case f => Future{ readFile(f) }.map{ new OracleParser().apply }.map{
        case parsed if parsed.successful => <file name={ f.getName }>{ parsed.get.toXml }</file>
        case _ => <file error="true" name={ f.getName }/>
      }
  }

  def processFile(file: File): xml.Elem = file match {
    case f if f.isDirectory() => <directory name={ f.getName }>{ for {subF <- f.listFiles().toSeq} yield processFile(subF) }</directory> 
    case f => (new OracleParser).apply(readFile(f)) match {
        case parsed if parsed.successful => <file name={ f.getName }>{ parsed.get.toXml }</file>
        case _ => <file error="true" name={ f.getName }/>
      }
  }

  if (args.length != 2) {
    Console.err.println("2 params requared")
    sys.exit(1)
  }
  
  if (! new File(args(1)).exists) {
    Console.err.println("source file/directory not exists")
    sys.exit(1)
  }

  val res =
    withActorSystem("OraParser") { implicit s =>
      <root>{ Await.result(processFileAsync(new File(args(1))), 20 seconds) }</root>
	}

//  val res = <root>{ processFile(new File(args(1))) }</root>
	  
  printToFile(new File(args(0))) { p =>
    p.println(res)
  }
}