/*
 * This is a more complex test on parent rewiring, checking
 * partial specialization.
 *
 * There are two things to care about here:
 *  - finding the most specific parent
 *  - rewiring the constructor with the correct type tags
 */
package miniboxing.tests.compile.inheritance


class F1[@miniboxed T, @miniboxed R]

class Predicate[T] extends F1[T, Int]
