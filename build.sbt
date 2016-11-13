name := "FoldMapReduce"

version := "1.0"

scalaVersion := "2.11.8"


libraryDependencies ++= {
  val scalaTestV  = "3.0.0"
  val catsV = "0.8.1"
  val shapelessV = "2.3.2"
  Seq(
    "org.scalatest"             %% "scalatest"                          % scalaTestV % "test",
    "org.typelevel"             %% "cats"                               % catsV,
    "com.chuusai"               %% "shapeless"                          % shapelessV,
    "org.typelevel"             %% "cats-laws"          % catsV,
    "org.scalacheck"            %% "scalacheck"         % "1.13.2",
    "org.typelevel"             %% "catalysts-platform" % "0.0.4"    % "test",
    "org.typelevel"             %% "discipline"         % "0.7.1"    % "test"
  )
}