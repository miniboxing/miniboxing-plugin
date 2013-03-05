package miniboxing.classloader.test

import org.junit.Test

class ClassloaderTest extends Base {

  @Test def testTargetDirect(): Unit =
    assertEqualString(collectSystemOut(new Target_J[Int](5,1).print), "print(5, 1) by miniboxing.classloader.test.Target_J\n")

  @Test def testTargetFactoryClassLoader(): Unit =
    assertEqualString(collectSystemOut(TargetFactory.newTarget_J[Int](5, 1).print), "print(5, 1) by miniboxing.classloader.test.Target_1\n")

  @Test def testTargetDispatcherDirect(): Unit =
    assertEqualString(collectSystemOut(new TargetDispatcher_J[Int](5, IntDispatcher).print), "print(5, IntDispatcher) by miniboxing.classloader.test.TargetDispatcher_J\n")

  @Test def testTargetDispatcherFactoryClassLoaderInt(): Unit =
    assertEqualString(collectSystemOut(TargetDispatcherFactory.newTargetDispatcher_J[Int](5, IntDispatcher).print), "print(5, IntDispatcher) by miniboxing.classloader.test.TargetDispatcher_0\n")

  @Test def testTargetDispatcherFactoryClassLoaderLong(): Unit =
    assertEqualString(collectSystemOut(TargetDispatcherFactory.newTargetDispatcher_J[Int](5, LongDispatcher).print), "print(5, LongDispatcher) by miniboxing.classloader.test.TargetDispatcher_1\n")
}
