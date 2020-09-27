import mill._, scalalib._
import $ivy.`com.lihaoyi::mill-contrib-bloop:0.6.1`

trait UselessModule extends ScalaModule {
  def scalaVersion = "2.13.1"
  def sources = T.sources { millSourcePath }
}

object `01-burrows-wheeler-transform` extends UselessModule
object `02-avl-tree` extends UselessModule
object `03-wavelet-tree` extends UselessModule
object `04-b-tree` extends UselessModule
object `05-finger-tree` extends UselessModule
