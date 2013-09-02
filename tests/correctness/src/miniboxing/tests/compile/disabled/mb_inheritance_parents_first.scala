/*
 * This is a very simple test for rewiring parents.
 * There are two things to care about here:
 *  - finding the most specific parent
 *  - rewiring the constructor with the correct type tags
 */
package miniboxing.tests.compile.inheritance


class C[@miniboxed T]

class D extends C[Int]
