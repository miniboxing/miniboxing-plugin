/*
 * This is a very simple test for rewiring parents.
 * There are two things to care about here:
 *  - finding the most specific parent
 *  - rewiring the constructor with the correct type tags
 */
package miniboxing.tests.compile.inheritance
import miniboxing.plugin.minispec

class CCC[@minispec T]

class DDD[@minispec U] extends CCC[U]
