package miniboxing.runtime

object MiniboxTypes {
  // Conclusions after digging deeper into the tableswitch problem:
  //  - Byte works as well as Int for the tag type
  //  - having a type alias as the Tag will prevent tableswitch generation (https://issues.scala-lang.org/browse/SI-6955)
  //  - looks like tableswitch is being generated instead of lookupswitch whenever possible, which is what we want
  //
  // So, until we get a fix for SI-6955, we need to keep the tag as Byte directly to benefit from fast tableswitches.
  type Tag = Byte
  type Minibox = Long
}
