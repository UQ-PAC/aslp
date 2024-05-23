import mill._, scalalib._

object lift extends RootModule with ScalaModule {
  def scalaVersion = "3.3.1"

  def ivyDeps = Agg(
    ivy"com.lihaoyi::mainargs:0.6.2",
    ivy"com.lihaoyi::sourcecode:0.3.0"
  )

}
