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
//    * Vlad Ureche
//
package miniboxing.runtime;

public interface MiniboxConstants {
  public static final byte UNIT = 0;
  public static final byte BOOLEAN = 1;
  public static final byte BYTE = 2;
  public static final byte SHORT = 3;
  public static final byte CHAR = 4;
  public static final byte INT = 5;
  public static final byte LONG = 6;
  public static final byte FLOAT = 7;
  public static final byte DOUBLE = 8;
  public static final byte REFERENCE = 9;
}
