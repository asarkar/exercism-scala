import mill._, scalalib._, scalafmt._
import $ivy.`com.lihaoyi::mill-contrib-jmh:`
import contrib.jmh.JmhModule

def isSbtProject(p: os.Path) = os.exists(p / "build.sbt")
def moduleNames = interp.watchValue(
    os.walk(millSourcePath, !isSbtProject(_), maxDepth = 1)
      .map(_.last)
)

object modules extends Cross[ExercismModule](moduleNames)

trait ExercismModule extends SbtModule with Cross.Module[String] with ScalafmtModule {
  outer =>

  val scalaVersion = "3.4.2"

  // Ends with 'modules' that need to be removed
  def millSourcePath = super.millSourcePath / os.up / crossValue

  def scalacOptions: T[Seq[String]] = Seq(
    "-encoding", "UTF-8",
    "-feature",
    "-Werror",
    "-explain",
    "-deprecation",
    "-unchecked",
    "-Wunused:all",
    "-rewrite",
    "-indent",
    "-source", "future",
  )

  object test extends SbtTests with TestModule.ScalaTest {
   val scalatestVersion = "3.2.19"
   val scalacheckVersion = "3.2.19.0"
   def scalacOptions: T[Seq[String]] = Seq("-encoding", "UTF-8")

   def ivyDeps = Agg(
      ivy"org.scalactic::scalactic:$scalatestVersion",
      ivy"org.scalatest::scalatest:$scalatestVersion",
      ivy"org.scalatestplus::scalacheck-1-18:$scalacheckVersion"
    )
  }

  // if (os.exists(millSourcePath / "src" / "jmh")) {
    object jmh extends ScalaModule with JmhModule {
      val scalaVersion = outer.scalaVersion
      def scalacOptions = outer.scalacOptions
      def jmhCoreVersion = "1.37"
      def moduleDeps = test.moduleDeps
    }
  // }
}
