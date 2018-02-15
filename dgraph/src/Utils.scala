package ajr.util

import java.nio.file.Files
import geny.Generator
import scala.collection.JavaConverters._

object FPath {
  def apply(file: java.io.File): FPath = FPath(file.toPath)
  def apply(file: ammonite.ops.Path): FPath = FPath(file.toNIO)
}

// Faster file stats - ammonite is super slow here (20x) but gets a lot more information
case class FPath(path: java.nio.file.Path) {
  def name = path.getFileName.toString
  lazy val mtime = Files.getLastModifiedTime(path)
  lazy val size = Files.size(path)
  lazy val isDir = Files.isDirectory(path)
  lazy val isSymLink = Files.isSymbolicLink(path)
  lazy val isFile = Files.isRegularFile(path)
  lazy val ext = {
    val idx = name.lastIndexOf('.')
    if (idx < 0) "" else name.substring(idx+1)
  }

  def files = {
    Generator.selfClosing{
      val dirStream = Files.newDirectoryStream(path)
      (dirStream.iterator().asScala.map(FPath(_)), () => dirStream.close())
    }
  }
}



object Stopwatch {
  val start = new Stopwatch
}

class Stopwatch {
  private var startTime = 0L
  private var pauseTime = 0L

  resetAndStart()

  private def systemTime = System.nanoTime()
  //private def systemTime = System.currentTimeMillis()*1000000

  def resetAndStart(): Unit = {
    startTime = systemTime
    pauseTime = 0L
  }

  def stop(): Unit = {
    pauseTime = systemTime
  }

  def stopped = pauseTime != 0L

  def start(): Unit = {
    if (stopped) {
      startTime = systemTime - current
      pauseTime = 0L
    }

  }

  private def current = {
    if (pauseTime == 0L)
      systemTime - startTime
    else
      pauseTime - startTime
  }

  def seconds = current / 1e9
  def millis = current / 1e6
  def micros = current / 1e3
  def nanos = current

  def elapsed = Utils.nanosToString(current)

  override def toString = elapsed
}

object Utils {
  def nanosToString(nanos: Long): String = {
    nanos match {
      //case t if t < 1e3 => "%.2f ns".format(t / 1e0) // pointless as min measurement is about 2ms
      case t if t < 1e6 => "%.2f us".format(t / 1e3)
      case t if t < 1e9 => "%.2f ms".format(t / 1e6)
      case t => "%.2f s ".format(t / 1e9)
    }
  }

  def millisToString(millis: Long): String = nanosToString(millis * 1000000)

  def dataSize(length: Long): String = {
    val kb = 1L << 10
    val mb = 1L << 20
    val gb = 1L << 30
    val tb = 1L << 40
    length.toDouble match {
      case l if l < 10 * kb => "%.0f bytes".format(l)
      case t if t < 100 * kb => "%.1f kb".format(t / kb)
      case t if t < mb => "%.0f kb".format(t / kb)
      case t if t < 100 * mb => "%.1f Mb".format(t / mb)
      case t if t < gb => "%.0f mb".format(t / mb)
      case t if t < 100 * gb => "%.1f Gb".format(t / gb)
      case t if t < tb => "%.0f gb".format(t / gb)
      case t if t < 100 * tb => "%.1f Tb".format(t / tb)
      case t => "%.0f Tb".format(t / tb)
    }
  }

  def simpleClassName(obj: AnyRef): String = obj.getClass.getName.split('$').last

  val numFormatter = java.text.NumberFormat.getIntegerInstance

  def intComma(value: Int): String = numFormatter.format(value)

  def openInOS(path: ammonite.ops.Path) = {
    val startCmd = System.getProperty("os.name") match {
      case "Linux" => Some("xdg-open")
      case "Windows" => Some("start")
      case "Mac OS X" => Some("open")
      case _ => None
    }
    import ammonite.ops._
    import sys.process._
    startCmd match {
      case Some(cmd) =>
        val res = Seq(cmd, path.toString).!
        if (res != 0)
          System.out.println(s"Failed to open: $path")

      case None =>
        System.out.println(s"Don't know how to open: $path")
    }
  }
}

object CollectionExtensions {
  implicit class SeqExt[T](s: Seq[T]) {
    def pl = s.foreach(println)
  }

  implicit class AggExt[T](s: mill.util.Loose.Agg[T]) {
    def pl = s.foreach(println)
  }

  implicit class ListEnhanced[T](s: Seq[T]) {
    import scala.collection.mutable
    def distinctBy[V](fn: T => V): Seq[T] = {
      var b = Seq.empty[T]
      val seen = mutable.HashSet[V]()
      for (x <- s) {
        val key = fn(x)
        if (!seen(key)) {
          b :+= x
          seen += key
        }
      }
      b
    }
  }
}