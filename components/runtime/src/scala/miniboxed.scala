/**
 * An annotation to signal miniboxing-based specialization. Notice that the
 * miniboxing compiler plugin needs to be available to scalac, otherwise your
 * code won't be transformed
 */
package scala

class miniboxed extends annotation.StaticAnnotation

