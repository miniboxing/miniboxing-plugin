package miniboxing

package object plugin {
  /**
   * A bridge from the old miniboxing annotation to the new one: `scala.miniboxed`.
   * @see [[scala.miniboxed]]
   */
  @deprecated(message = "Please use `scala.miniboxed` instead of `miniboxing.plugin.minispec`.")
  type minispec = scala.miniboxed
}

package plugin {
  /**
   * This class should only appear in the tree during the `minibox` phase
   * and should be cleaned up afterwards, during the `minibox-cleanup` phase.
   */
  class storage extends annotation.StaticAnnotation
}
