#Miniboxing Plugin for the Scala programming language#

Miniboxing is a lightweight approach to optimizing generic classes in the Scala compiler. It takes its roots from [specialization](http://infoscience.epfl.ch/record/150134/files/p42-dragos.pdf) but makes different choices aimed at reducing the created bytecode as much as possible without sacrificing execution speed. Have a look at the `docs` folder for a more technical description.

##Repository organization##
 - wip is the working branch, most current version of the plugin
 - sbt is used for compilation and testing
 - the repository contains several sbt projects:
   - `components/plugin` - the actual Scala compiler plugin
   - `components/runtime` - the runtime support for the transformed code
   - `components/classloader` - the classloader used for runtime class specialization
   - `components/benchmarks` - the benchmarks for the project
   - `tests` - the tests for the plugin transformation
   - `docs` - documents released as the development goes on

##Questions?##
If you have any question, you can contact me at vlad dot ureche at epfl dot ch.
