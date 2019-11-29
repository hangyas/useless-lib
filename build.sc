import mill._, scalalib._

trait UselessModule extends ScalaModule {
  def scalaVersion = "2.12.4"
  def sources = T.sources { os.pwd / folder }

  def folder: String
}

object `01-burrows-wheeler-transform` extends UselessModule {
  def folder = "01-burrows-wheeler-transform"
}
