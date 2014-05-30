/*
 * This is a more complex test on parent rewiring, checking
 * partial specialization and specialization.
 *
 * There are two things to care about here:
 *  - finding the most specific parent
 *  - rewiring the constructor with the correct type tags
 */
package miniboxing.tests.compile.inheritance


class FF1[@miniboxed T, @miniboxed R]

class PPredicate[@miniboxed T] extends FF1[T, Int]
