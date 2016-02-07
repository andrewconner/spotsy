// The Play plugin
addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.5.1")

// web plugins

//addSbtPlugin("com.typesafe.sbt" % "sbt-coffeescript" % "1.0.0")

//addSbtPlugin("com.typesafe.sbt" % "sbt-less" % "1.0.6")

//addSbtPlugin("com.typesafe.sbt" % "sbt-jshint" % "1.0.3")

//addSbtPlugin("com.typesafe.sbt" % "sbt-rjs" % "1.0.7")

//addSbtPlugin("com.typesafe.sbt" % "sbt-digest" % "1.1.0")

//addSbtPlugin("com.typesafe.sbt" % "sbt-mocha" % "1.1.0")

// linting / code quality

addSbtPlugin("org.brianmckenna" % "sbt-wartremover" % "0.14")

addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "0.7.0")

resolvers += "sonatype-releases" at "https://oss.sonatype.org/content/repositories/releases/"

addSbtPlugin("org.scalariform" % "sbt-scalariform" % "1.6.0")

addSbtPlugin("com.github.alexarchambault" % "coursier-sbt-plugin" % "1.0.0-M10")

