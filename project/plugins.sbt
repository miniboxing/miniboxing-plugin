resolvers += Classpaths.sbtPluginReleases

resolvers += Resolver.url("artifactory", url("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases"))(Resolver.ivyStylePatterns)

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.10.1")

addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.4.0")

addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "0.2.1")

addSbtPlugin("com.typesafe.sbt" % "sbt-pgp" % "0.8.3")

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.0.4")

addSbtPlugin("com.codacy" % "sbt-codacy-coverage" % "1.0.3") 

addSbtPlugin("com.github.alexarchambault" % "coursier-sbt-plugin" % "1.0.0-M7")
