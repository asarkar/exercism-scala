import mill._, scalalib._, scalafmt._

trait ExercismModule extends SbtModule with ScalafmtModule {
  def scalaVersion = "3.4.2"

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

  trait ExercismTestModule extends SbtTests with TestModule.ScalaTest {
   def scalatestVersion = "3.2.19"

   def scalacOptions: T[Seq[String]] = Seq("-encoding", "UTF-8")

   def ivyDeps = Agg(
      ivy"org.scalactic::scalactic:$scalatestVersion",
      ivy"org.scalatest::scalatest:$scalatestVersion",
    )
  }
}

object `hello-world` extends ExercismModule {
  object test extends ExercismTestModule
}

object `two-fer` extends ExercismModule {
  object test extends ExercismTestModule
}

object leap extends ExercismModule {
  object test extends ExercismTestModule
}

object `space-age` extends ExercismModule {
  object test extends ExercismTestModule
}

object `grade-school` extends ExercismModule {
  object test extends ExercismTestModule
}

object bob extends ExercismModule {
  object test extends ExercismTestModule
}

object hamming extends ExercismModule {
  object test extends ExercismTestModule
}

object etl extends ExercismModule {
  object test extends ExercismTestModule
}

object `reverse-string` extends ExercismModule {
  object test extends ExercismTestModule
}

object `robot-simulator` extends ExercismModule {
  object test extends ExercismTestModule
}