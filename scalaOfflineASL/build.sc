import mill._, scalalib._




object lifter extends ScalaModule {
  def scalaVersion = "3.3.1"
}

object main extends ScalaModule {
    def scalaVersion = "3.3.1"

    def moduleDeps = Seq(lifter)

    def ivyDeps = Agg(
      ivy"com.lihaoyi::mainargs:0.6.2",
      ivy"com.lihaoyi::sourcecode:0.3.0"
    )

    def mainClass = Some("main.Main")

  }

