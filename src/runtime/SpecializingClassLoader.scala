package runtime

import java.io.ByteArrayOutputStream
import java.io.InputStream
import java.lang.{ClassLoader => JClassLoader}
import java.util.{List => JList}
import java.util.ListIterator
import org.objectweb.asm.tree.AbstractInsnNode
import org.objectweb.asm.tree.ClassNode
import org.objectweb.asm.tree.FieldInsnNode
import org.objectweb.asm.tree.FieldNode
import org.objectweb.asm.tree.InsnNode
import org.objectweb.asm.tree.MethodNode
import org.objectweb.asm.ClassReader
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes
import scala.collection.JavaConverters.asScalaBufferConverter

/**
 * This ClassLoader receives the value of the type tag encoded in the class name.
 * It uses this value to make the field static final initialized to that value.
 */
class SpecializingClassLoader(parent: JClassLoader) extends JClassLoader(parent) {
  def this() = { this(null) }

  import SpecializingClassLoader._

  private final val SPEC_MARKER = "_J"
  val specFor = 5 // 5 - int / 8 - double

  override def loadClass(name: String): Class[_] = loadClass(name, false)
  private val definedClasses = Map[String, Class[_]]()

  
  /*
   * For a class named `package.Foo`, this classloader will receive `package.Foo__2` 
   * whenever it needs to specialize it for the type tag equals 2.
   */
  override def loadClass(name: String, resolve: Boolean): Class[_] = {
    if (name.startsWith("java") || name.startsWith("scala"))
      return super.loadClass(name, resolve)
//    println("Loading " + name)

    val cr = new ClassReader(classAsStream(name));
    val classNode = new ClassNode();
    cr.accept(classNode, 0);

    if (name.contains(SPEC_MARKER)) {
      // Make tag field 'static final'
      val fieldNodes = classNode.fields.asInstanceOf[JList[FieldNode]].asScala
      for (fieldNode <- fieldNodes if fieldNode.name.endsWith("_TypeTag")) {
        fieldNode.access |= Opcodes.ACC_STATIC;
        fieldNode.value = new Integer(specFor)
      }

      // Patch all the methods
      val methodNodes = classNode.methods.asInstanceOf[JList[MethodNode]].asScala
      for (methodNode <- methodNodes) {
        val insnNodes = methodNode.instructions.iterator().asInstanceOf[ListIterator[AbstractInsnNode]]
        while (insnNodes.hasNext) {
          insnNodes.next match {
            case finsn: FieldInsnNode if (finsn.name.endsWith("_TypeTag")) =>
              finsn.getOpcode match {
                case Opcodes.GETFIELD =>
                  insnNodes.set(new InsnNode(Opcodes.POP));
                  insnNodes.add(new FieldInsnNode(Opcodes.GETSTATIC, finsn.owner, finsn.name, finsn.desc))
                case Opcodes.PUTFIELD =>
                  insnNodes.set(new InsnNode(Opcodes.POP2));
              }
            case _ =>
          }
        }
      }
    }

    // Convert the class to byte array again
    val cw = new ClassWriter(ClassWriter.COMPUTE_MAXS | ClassWriter.COMPUTE_FRAMES);
    classNode.accept(cw);
    var classBytes = cw.toByteArray

    // Load the class into the JVM
    val c = defineClass(name, classBytes, 0, classBytes.length)
    if (c == null)
      throw new ClassNotFoundException(name)
    if (resolve)
      resolveClass(c)

    c
  }

  private def classAsStream(className: String) =
    getResourceAsStream(className.replaceAll("""\.""", "/") + ".class")

  private def readBytes(in: InputStream): Array[Byte] = {
    if (in == null)
      throw new ClassNotFoundException()
    val bos = new ByteArrayOutputStream();
    var next = in.read;
    while (next > -1) {
      bos.write(next);
      next = in.read();
    }
    bos.flush();
    bos.toByteArray();
  }

}

object SpecializingClassLoader {

  def setContext(cl: JClassLoader) =
    Thread.currentThread.setContextClassLoader(cl)

  final val TAG_FIELD_NAME_PREFIX = "_tag_"
  final val SPEC_CLASS_NAME_TEMPLATE = ".*Wmc.*Wsp"
  //  final val TAG_FIELD_NAME_PREFIX = "$tag$"
  //  final val SPEC_CLASS_NAME_TEMPLATE = "*$mc*$sp"
  private val descriptorToTag = Map(
    'U' -> 0,
    'Z' -> 1,
    'B' -> 2,
    'C' -> 3,
    'S' -> 4,
    'I' -> 5,
    'J' -> 6,
    'D' -> 7,
    'F' -> 8)

  private def getTypeTags(name: String) = {
    //    val typeParamDescriptors = name.substring(name.lastIndexOf('c') + 1, name.lastIndexOf('$')) 
    val typeParamDescriptors = name.substring(name.lastIndexOf('c') + 1, name.lastIndexOf('W'))
    typeParamDescriptors.map(descriptorToTag)
  }

  private def getTemplateName(name: String) = {
    name.substring(0, name.length - 3)
  }
}
