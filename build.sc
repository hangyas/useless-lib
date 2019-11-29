import mill._, scalalib._

trait UselessModule extends ScalaModule {
  def scalaVersion = "2.13.1"
  def sources = T.sources { millSourcePath }
}

object `01-burrows-wheeler-transform` extends UselessModule 
