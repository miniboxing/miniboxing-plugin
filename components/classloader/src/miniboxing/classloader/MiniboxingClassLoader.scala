//
//     _____   .__         .__ ___.                    .__ scala-miniboxing.org
//    /     \  |__|  ____  |__|\_ |__    ____  ___  ___|__|  ____     ____
//   /  \ /  \ |  | /    \ |  | | __ \  /  _ \ \  \/  /|  | /    \   / ___\
//  /    Y    \|  ||   |  \|  | | \_\ \(  <_> ) >    < |  ||   |  \ / /_/  >
//  \____|__  /|__||___|  /|__| |___  / \____/ /__/\_ \|__||___|  / \___  /
//          \/          \/          \/               \/         \/ /_____/
// Copyright (c) 2011-2015 Scala Team, École polytechnique fédérale de Lausanne
//
// Authors:
//    * Cristian Talau
//    * Miguel Alfredo Garcia Guiterrez
//    * Vlad Ureche
//
package miniboxing.classloader

import java.net._
import java.io._
import miniboxing.tools.asm._
import miniboxing.tools.asm.tree._
import java.lang.{ClassLoader => JClassLoader}
import java.util.{List => JList}
import scala.collection.JavaConverters.asScalaBufferConverter
import java.util.ListIterator
import miniboxing.tools.asm.util._
import miniboxing.tools.asm.tree.analysis._
import scala.collection.mutable.Map
import miniboxing.tools.asm.optimiz._

/** Taken from http://stackoverflow.com/questions/6366288/how-to-change-default-class-loader-in-java */
class MiniboxingClassLoader(parent: ClassLoader, verbose: Boolean = false) extends ClassLoader(parent) {

  val classes = Map.empty[String, Class[_]]
  lazy val constantFolder = new ConstantFolder()
  lazy val unreachableCode = new UnreachableCode()
  lazy val jumpReducer = new JumpReducer(null)
  lazy val jumpChainsColl = new JumpChainsCollapser(jumpReducer)


  def needsInstantiation(name: String): Boolean =
    // TODO: Extend to more parameters
    name.matches(".*_class_[0-9].*")

  def needsUpdate(name: String): Boolean =
    // TODO: Extend to more parameters
    name.contains("_class_J")

  def updateClassName(name: String, tag: Int) =
    if (needsUpdate(name)) name.replaceAll("_class_J", "_class_" + tag) else name

  def modifyClass(in: InputStream, oldname: String, newname: String): Array[Byte] = {
    val tparam = newname(newname.indexOf("_class_") + 7) - '0'.toInt
//    System.err.println("TPARAM EXTRACTION: " + newname + " ==> " + tparam)
    // forcefully patch the class in the inputstream phase
    class PatchInline(in: InputStream) extends InputStream {
      var state = 0
      // TODO: I'm 100% sure this can be better coded with some bit shifting
      def read(): Int = in.read() match {
        case '_' if state == 0 => state = 1; '_'
        case 'c' if state == 1 => state = 2; 'c'
        case 'l' if state == 2 => state = 3; 'l'
        case 'a' if state == 3 => state = 4; 'a'
        case 's' if state == 4 => state = 5; 's'
        case 's' if state == 5 => state = 6; 's'
        case '_' if state == 6 => state = 7; '_'
        case 'J' if state == 7 => state = 0; tparam + '0'
        case other => state = 0; other
      }
    }
    val cr = new ClassReader(new PatchInline(in))
    val classNode = new ClassNode()
    cr.accept(classNode, 0)

    classNode.name = newname
    classNode.superName = updateClassName(classNode.superName, tparam)



    // Make the type tags static final
    val fieldNodes = classNode.fields.iterator()
    while(fieldNodes.hasNext()) {
      val fieldNode = fieldNodes.next()
      if (fieldNode.name.endsWith("_TypeTag")) {
        fieldNode.access |= Opcodes.ACC_FINAL;
        // TODO: Extend to more parameters, this only supports one
        fieldNode.value = new Integer(tparam)
      }
    }

    // Patch all the methods
    val methodNodes = classNode.methods.iterator()
    while(methodNodes.hasNext()) {
      val methodNode = methodNodes.next()
      val insnNodes = methodNode.instructions.iterator().asInstanceOf[ListIterator[AbstractInsnNode]]
      while (insnNodes.hasNext()) {
        insnNodes.next() match {
          case finst: FieldInsnNode =>
            // update owner to the new class
            finst.owner = finst.owner.replace(oldname, newname) // update names everywhere
            // and patch the code for the static final value
            if (finst.name.endsWith("_TypeTag")) {
              finst.getOpcode match {
                case Opcodes.GETFIELD =>
                  insnNodes.set(new InsnNode(Opcodes.POP));
                  val replNode = tparam match {
                    case 0 | 1 | 2 | 3 | 4 => new InsnNode(Opcodes.ICONST_0 + tparam)
                    case _ => new IntInsnNode(Opcodes.BIPUSH, tparam)
                  }
                  insnNodes.add(replNode) // Full expansion
                case Opcodes.PUTFIELD =>
                  insnNodes.set(new InsnNode(Opcodes.POP2));
              }
            }
//          case tinst: TypeInsnNode if tinst.getOpcode() == Opcodes.NEW =>
//            // patch up NEW calls
//            if (needsUpdate(tinst.desc))
//              insnNodes.set(new TypeInsnNode(Opcodes.NEW, updateClassName(tinst.desc, tparam)))
//          case minst: MethodInsnNode =>
//            // update owner to the new class
//            minst.owner = minst.owner.replaceAll(oldname, newname) // update names everywhere
//            if (minst.getOpcode() == Opcodes.INVOKESTATIC) {
//              minst.owner = updateClassName(minst.owner, tparam)
//              //System.err.println("INVOKESTATIC on " + minst.owner)
//            }
//            // patch up constructor call
//            if (minst.name == "<init>")
//              if (minst.owner.endsWith("_J"))
//                // TODO: In-place replace this
//                minst.owner = minst.owner.replaceAll("_class_J", "_class_" + tparam)
          case _ =>
        }
      }
    }

//    // Debugging:
//    System.err.println("================================BEFORE================================")
//    var printWriter = new PrintWriter(System.err);
//    var traceClassVisitor = new TraceClassVisitor(printWriter);
//    classNode.accept(traceClassVisitor);

    // Optimizing the hell out of that class:
//    val iter = classNode.methods.iterator()
//    while(iter.hasNext) {
//      val mnode = iter.next()
//      if (Util.hasBytecodeInstructions(mnode)) {
//         Util.computeMaxLocalsMaxStack(mnode)
//         jumpChainsColl.transform(mnode)
//         constantFolder.transform(newname, mnode)
//         unreachableCode.transform(newname, mnode)
//         jumpChainsColl.transform(mnode)
//      }
//    }

//    System.err.println("================================AFTER================================")
//    val printWriter = new PrintWriter(System.err);
//    val traceClassVisitor = new TraceClassVisitor(printWriter);
//    classNode.accept(traceClassVisitor);
//
//    val analyzer = new Analyzer(new BasicVerifier)
//    val methodNodes2 = classNode.methods.iterator()
//    while (methodNodes.hasNext()) {
//      val methodNode = methodNodes2.next()
//      analyzer.analyze(newname, methodNode)
//    }

    val cw = new ClassWriter(0/* ClassWriter.COMPUTE_MAXS | ClassWriter.COMPUTE_FRAMES */);
    classNode.accept(cw);
    var classBytes = cw.toByteArray

    // DUMP CLASS
    if (sys.props.get("classloader.dump").isDefined) {
      val outputFile = new File("/tmp/class-dump/" + newname + ".class")
      val outputDir = outputFile.getParentFile()
      outputDir.mkdirs()
      val output = new FileOutputStream(outputFile)
      output.write(classBytes)
    }

    classBytes
  }

  override def findClass(decodedName: String): Class[_] = classes get decodedName match {
    case Some(clazz) => clazz
    case None =>
//      System.err.println("CLASS: " + decodedName + "  " + needsInstantiation(decodedName))
      if (needsInstantiation(decodedName)) {
        try {
          if (verbose) System.err.println("UPDATING: " + decodedName)
          val encodedName = decodedName.replace('.', '/')
          // TODO: Extend to more parameters
          val encodedTplName = encodedName.replaceAll("_class_[0-9]", "_class_J")
//          System.err.println("BASE: " + encodedTplName)
          val templateBytes = super.getResourceAsStream(encodedTplName + ".class");
          if (templateBytes == null) {
            throw new ClassNotFoundException("Class " + encodedTplName + " not found. Sorry.");
          }
          val array = modifyClass(templateBytes, encodedTplName, encodedName);

          // store it
          val clazz = defineClass(decodedName, array, 0, array.length);
          classes += decodedName -> clazz
          if (verbose) System.err.println("DONE WITH " + decodedName)
          clazz
        } catch {
          case io: IOException =>
            throw new ClassNotFoundException(io.toString);
        }
      } else {
        super.findClass(decodedName);
      }
    }
}

object MiniboxingClassLoader {
  // TODO: Make this weak
  var classloaders = collection.mutable.Map.empty[ClassLoader, MiniboxingClassLoader]
  def classloader(ths: Any, verbose: Boolean = false): MiniboxingClassLoader = {
    val crt_classloader = ths.getClass().getClassLoader()
    classloaders get (crt_classloader) match {
      case Some(classloader) => classloader
      case None =>
        val classloader = new MiniboxingClassLoader(crt_classloader, verbose)
        classloaders += crt_classloader -> classloader
        classloader
    }
  }
}
