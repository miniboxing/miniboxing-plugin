#Miniboxing Plugin for the Scala programming language#

Miniboxing is a lightweight approach to optimizing generic classes in the Scala compiler. It revisits the current [specialization](http://infoscience.epfl.ch/record/150134/files/p42-dragos.pdf) transformation, but makes different choices aimed at reducing the bytecode without sacrificing execution speed. If that's enough to get you interested, have a look at the `docs` directory for more technical descriptions.

##Repository organization##
 - wip is the working branch, most current version of the plugin
 - master is usually a bit behind wip, but should be stable (let's make it clear: alpha-stable, nowhere near production-ready)
 - sbt is used for compilation and testing (version 0.12.1)
 - the repository contains several sbt projects:
   - `components/plugin`      - the actual Scala compiler plugin
   - `components/runtime`     - the runtime support for the transformed code
   - `components/classloader` - the classloader used for runtime class specialization
   - `tests/benchmarks`       - the benchmarks for the project
   - `tests/correctness`      - the tests for the plugin transformation
   - `docs`                   - documents released as the development goes on

##Questions?##
If you have any question, you can contact me at vlad dot ureche at epfl dot ch.
