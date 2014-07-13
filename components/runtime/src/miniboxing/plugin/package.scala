//
//     _____   .__         .__ ___.                    .__ scala-miniboxing.org
//    /     \  |__|  ____  |__|\_ |__    ____  ___  ___|__|  ____     ____
//   /  \ /  \ |  | /    \ |  | | __ \  /  _ \ \  \/  /|  | /    \   / ___\
//  /    Y    \|  ||   |  \|  | | \_\ \(  <_> ) >    < |  ||   |  \ / /_/  >
//  \____|__  /|__||___|  /|__| |___  / \____/ /__/\_ \|__||___|  / \___  /
//          \/          \/          \/               \/         \/ /_____/
// Copyright (c) 2012-2014 Scala Team, École polytechnique fédérale de Lausanne
//
// Authors:
//    * Cristian Talau
//    * Vlad Ureche
//
package miniboxing

import scala.annotation._
package object plugin {
  /**
   * A bridge from the old miniboxing annotation to the new one: `scala.miniboxed`.
   * @see [[scala.miniboxed]]
   */
  @deprecated(message = "Please use `scala.miniboxed` instead of `miniboxing.plugin.minispec`.", "0.1-SNAPSHOT")
  type minispec = scala.miniboxed
}

/**
 * Internal, don't use!
 */
class mbFunction extends StaticAnnotation with TypeConstraint
