/*
 * This is a very simple test for rewiring parents.
 */
package miniboxing.tests.compile.inheritance
import miniboxing.plugin.minispec

class CCCC[T]

class DDDD[@minispec U] extends CCCC[U]
