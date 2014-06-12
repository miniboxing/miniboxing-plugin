package miniboxing.tests.correctness.erasure.torture9

/** This fails in the transformation if
 *  the wildcard type is not considered */
class C[@miniboxed T](var x: T)
