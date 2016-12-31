package jae

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Paths, Files}
import java.security.MessageDigest
import scala.tools.nsc.io
import scala.io.Source

/**
  * Created by jaety on 12/30/16.
  */
package object data {
  val cachedir = {
    // TODO better system for specifying directory. System property, command line flag, etc.
    val path = new File(System.getProperty("user.home"), ".jae-data")
    if (!path.exists()) {
      path.mkdir()
    }
    path
  }

  def md5(s: String) = {
    MessageDigest.getInstance("MD5").digest(s.getBytes).map("%02x".format(_)).mkString
  }

  def getWithCaching(source: String) = {
    val fn = md5(source)
    val cacheFile = new File(cachedir, fn)
    val manifest = new File(cachedir, "manifest.txt")
    val content = (if (cacheFile.exists()) Source.fromFile(cacheFile) else Source.fromURL(source)).mkString
    if (!cacheFile.exists()) {
      Files.write(Paths.get(cacheFile.toURI), content.getBytes(StandardCharsets.UTF_8))
      io.File(manifest).appendAll(s"${source},${fn}\n")
    }
    content
  }
}
