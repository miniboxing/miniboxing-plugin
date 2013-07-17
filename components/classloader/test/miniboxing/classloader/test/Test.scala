package miniboxing.classloader.test

import org.junit.Test

class ClassloaderTest extends Base {

//////////////////////////////////////////////////  TYPETAG TESTING  \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

  @Test def testTargetDirect(): Unit =
    assertEqualString(collectSystemOut(new Target_class_J[Int](5,1).print), "print(5, 1) by miniboxing.classloader.test.Target_class_J\n")

  @Test def testTargetSuper(): Unit =
    assertEqualString(collectSystemOut(new Target_class_J[Int](5,1).printTrait), "printTrait: miniboxing.classloader.test.TargetTrait_class_J$class\n")

  @Test def testTargetTrait(): Unit =
    assertEqualString(collectSystemOut(new Target_class_J[Int](5,1).printSuper), "printSuper: miniboxing.classloader.test.TargetSuper_class_J\n")

  @Test def testTargetInnerClass(): Unit =
    assertEqualString(collectSystemOut(new Target_class_J[Int](5,1).printInnerClass), "printInnerClass: miniboxing.classloader.test.Target_class_J$$anon$1\n")

  @Test def testTargetFactoryClassLoaderTarget(): Unit = try {
    assertEqualString(collectSystemOut(TargetFactory.newTarget_J[Int](5, 1).print), "print(5, 1) by miniboxing.classloader.test.Target_class_1\n")
  } catch {
    case th: Throwable =>
      th.printStackTrace(System.err)
      throw th
  }

  @Test def testTargetFactoryClassLoaderSuper(): Unit =
    assertEqualString(collectSystemOut(TargetFactory.newTarget_J[Int](5, 1).printSuper), "printSuper: miniboxing.classloader.test.TargetSuper_class_1\n")

  @Test def testTargetFactoryClassLoaderTrait(): Unit =
    assertEqualString(collectSystemOut(TargetFactory.newTarget_J[Int](5, 1).printTrait), "printTrait: miniboxing.classloader.test.TargetTrait_class_1$class\n")

  @Test def testTargetFactoryClassLoaderInnerClass(): Unit =
    assertEqualString(collectSystemOut(TargetFactory.newTarget_J[Int](5, 1).printInnerClass), "printInnerClass: miniboxing.classloader.test.Target_class_1$$anon$1\n")

/////////////////////////////////////////////////  DISPATCHER TESTING \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
  @Test def testTargetDispatcherDirect(): Unit =
    assertEqualString(collectSystemOut(new TargetDispatcher_class_J[Int](5, IntDispatcher).print), "print(5, IntDispatcher) by miniboxing.classloader.test.TargetDispatcher_class_J\n")

  @Test def testTargetDispatcherFactoryClassLoaderInt(): Unit =
    assertEqualString(collectSystemOut(TargetDispatcherFactory.newTargetDispatcher_J[Int](5, IntDispatcher).print), "print(5, IntDispatcher) by miniboxing.classloader.test.TargetDispatcher_class_0\n")

  @Test def testTargetDispatcherFactoryClassLoaderLong(): Unit =
    assertEqualString(collectSystemOut(TargetDispatcherFactory.newTargetDispatcher_J[Int](5, LongDispatcher).print), "print(5, LongDispatcher) by miniboxing.classloader.test.TargetDispatcher_class_1\n")
}
