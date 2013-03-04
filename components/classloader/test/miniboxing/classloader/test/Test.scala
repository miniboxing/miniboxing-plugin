package miniboxing.classloader.test

import org.junit.Test

class ClassloaderTest extends Base {

  @Test def testFactoryDirect(): Unit =
    assertEqualString(collectSystemOut(TargetFactory.newTarget_J[Int](5, 1).print), "print(5, 1) by miniboxing.classloader.test.Target_1\n")

  @Test def testFactoryClassLoader(): Unit =
    assertEqualString(collectSystemOut(new Target_J[Int](5,1).print), "print(5, 1) by miniboxing.classloader.test.Target_J\n")
}
