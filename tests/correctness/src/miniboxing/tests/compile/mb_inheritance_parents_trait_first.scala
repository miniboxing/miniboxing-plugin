/** Trait inheritance is a bit tricky because of the
 *  specialized type tags, which need to transform into
 *  getters and need to be overridden. */
package miniboxing.tests.compile.inheritance.traits


trait C[@miniboxed T]
