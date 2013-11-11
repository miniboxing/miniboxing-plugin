/*
 * This is a very simple test for rewiring parents.
 */
package miniboxing.tests.compile.inheritance


class CCCC[T]

class DDDD[@miniboxed U] extends CCCC[U]
