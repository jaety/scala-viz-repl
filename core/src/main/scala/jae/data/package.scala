package jae

import java.io.File

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
}
