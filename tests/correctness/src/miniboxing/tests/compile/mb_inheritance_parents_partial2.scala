/*
 * This is a more complex test on parent rewiring, checking
 * partial specialization and specialization.
 *
 * There are two things to care about here:
 *  - finding the most specific parent
 *  - rewiring the constructor with the correct type tags
 */
package miniboxing.tests.compile.inheritance
import miniboxing.plugin.minispec

class FF1[@minispec T, @minispec R]

class PPredicate[@minispec T] extends FF1[T, Boolean]
