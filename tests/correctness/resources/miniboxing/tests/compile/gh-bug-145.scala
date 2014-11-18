package miniboxing.tests.compile.bug145
package com.clarifi.miniboxing.test

import collection.GenTraversable
import collection.generic.GenericCompanion

abstract class Projected[+CC[X] <: GenTraversable[X]]
    extends GenericCompanion[CC] {
  def project = new GenericCompanion[CC] {
    def newBuilder[A] = Projected.this.newBuilder
  }
}
