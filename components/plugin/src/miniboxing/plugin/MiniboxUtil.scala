//
//     _____   .__         .__ ___.                    .__ scala-miniboxing.org
//    /     \  |__|  ____  |__|\_ |__    ____  ___  ___|__|  ____     ____
//   /  \ /  \ |  | /    \ |  | | __ \  /  _ \ \  \/  /|  | /    \   / ___\
//  /    Y    \|  ||   |  \|  | | \_\ \(  <_> ) >    < |  ||   |  \ / /_/  >
//  \____|__  /|__||___|  /|__| |___  / \____/ /__/\_ \|__||___|  / \___  /
//          \/          \/          \/               \/         \/ /_____/
// Copyright (c) 2011-2015 Scala Team, École polytechnique fédérale de Lausanne
//
package miniboxing.plugin

import scala.tools.nsc.Global
import scala.tools.nsc.Phase
import scala.tools.nsc.plugins._
import scala.tools.nsc.transform._
import metadata._
import transform._
import interop.inject._
import interop.bridge._
import interop.coerce._
import interop.commit._
import minibox.inject._
import minibox.bridge._
import minibox.coerce._
import minibox.commit._
import hijack._
import prepare._
import infrastructure._
import tweakerasure._
import scala.tools.nsc.settings.ScalaVersion



/** Specialization hijacking component `@specialized T` -> `@miniboxed T` */
trait HijackComponent extends
    PluginComponent
    with MiniboxInfoHijack
    with MiniboxDefinitions
    with ScalacCrossCompilingLayer {

  val common: CommonDefinitions { val global: HijackComponent.this.global.type }
  def flags = common /* common contains the flags as well */
}

/** Glue transformation to bridge Function and MiniboxedFunction */
trait InteropInjectComponent extends
    PluginComponent
    with InteropDefinitions
    with InteropMetadata
    with InteropInjectInfoTransformer
    with InteropInjectTreeTransformer
    with ScalacCrossCompilingLayer {

  def interopInjectPhase: StdPhase

  def afterInteropInject[T](op: => T): T = global.afterPhase(interopInjectPhase)(op)
  def beforeInteropInject[T](op: => T): T = global.beforePhase(interopInjectPhase)(op)

  val common: CommonDefinitions { val global: InteropInjectComponent.this.global.type }
  def flags = common /* common contains the flags as well */
}

/** Tree preparer before retyping the tree */
trait PrepareComponent extends
    PluginComponent
    with PrepareTreeTransformer
    with ScalacCrossCompilingLayer {

  val interop: InteropInjectComponent { val global: PrepareComponent.this.global.type }

  def preparePhase: StdPhase

  def afterPrepare[T](op: => T): T = global.afterPhase(preparePhase)(op)
  def beforePrepare[T](op: => T): T = global.beforePhase(preparePhase)(op)

  val common: CommonDefinitions { val global: PrepareComponent.this.global.type }
  def flags = common /* common contains the flags as well */
}

/** Introduces explicit bridge methods to respect the object model
 *  in the presence of data representation transformations. */
trait InteropBridgeComponent extends
    PluginComponent
    with InteropBridgeTreeTransformer
    with ScalacCrossCompilingLayer {

  val interop: InteropInjectComponent { val global: InteropBridgeComponent.this.global.type }

  def interopBridgePhase: StdPhase

  def afterInteropBridge[T](op: => T): T = global.afterPhase(interopBridgePhase)(op)
  def beforeInteropBridge[T](op: => T): T = global.beforePhase(interopBridgePhase)(op)
  def afterInteropBridgeNext[T](op: => T): T = global.afterPhase(interopBridgePhase.next)(op)
  def beforeInteropBridgeNext[T](op: => T): T = global.beforePhase(interopBridgePhase.next)(op)

  val common: CommonDefinitions { val global: InteropBridgeComponent.this.global.type }
  def flags = common /* common contains the flags as well */
}

/** Glue transformation to bridge Function and MiniboxedFunction */
trait InteropCoerceComponent extends
    PluginComponent
    with InteropCoerceTreeTransformer
    with InteropAnnotationCheckers
    with ScalacCrossCompilingLayer {

  val interop: InteropInjectComponent { val global: InteropCoerceComponent.this.global.type }

  def interopCoercePhase: StdPhase

  def afterInteropCoerce[T](op: => T): T = global.afterPhase(interopCoercePhase)(op)
  def beforeInteropCoerce[T](op: => T): T = global.beforePhase(interopCoercePhase)(op)

  val common: CommonDefinitions { val global: InteropCoerceComponent.this.global.type }
  def flags = common /* common contains the flags as well */
}

/** Glue transformation to bridge Function and MiniboxedFunction */
trait InteropCommitComponent extends
    PluginComponent
    with InteropCommitInfoTransformer
    with InteropCommitTreeTransformer
    with ScalacCrossCompilingLayer {

  def minibox: MiniboxInjectComponent
  val interop: InteropInjectComponent { val global: InteropCommitComponent.this.global.type }

  def interopCommitPhase: StdPhase

  def afterInteropCommit[T](op: => T): T = global.afterPhase(interopCommitPhase)(op)
  def beforeInteropCommit[T](op: => T): T = global.beforePhase(interopCommitPhase)(op)

  val common: CommonDefinitions { val global: InteropCommitComponent.this.global.type }
  def flags = common /* common contains the flags as well */
}

/** Injecticator component `def t -> def t_L, def t_J` */
trait MiniboxInjectComponent extends
    PluginComponent
    with MiniboxLogging
    with MiniboxDefinitions
    with MbArrayDefinitions
    with MbReflectionDefinitions
    with MiniboxNameUtils
    with MiniboxMetadata
    with MiniboxMetadataUtils
    with MiniboxMetadataAddons
    with MiniboxMethodInfo
    with MiniboxFlagVersioning
    with MiniboxInjectInfoTransformation
    with MiniboxInjectTreeTransformation
    with TreeRewriters
    with ScalacCrossCompilingLayer {

  def mboxInjectPhase: StdPhase

  def afterMiniboxInject[T](op: => T): T = global.afterPhase(mboxInjectPhase)(op)
  def beforeMiniboxInject[T](op: => T): T = global.beforePhase(mboxInjectPhase)(op)

  val common: CommonDefinitions { val global: MiniboxInjectComponent.this.global.type }
  def flags = common /* common contains the flags as well */
}

/** Introduces explicit bridge methods to respect the object model
 *  in the presence of data representation transformations. */
trait MiniboxBridgeComponent extends
    PluginComponent
    with MiniboxBridgeTreeTransformer
    with ScalacCrossCompilingLayer {

  val minibox: MiniboxInjectComponent { val global: MiniboxBridgeComponent.this.global.type }

  def mboxBridgePhase: StdPhase

  def afterMiniboxBridge[T](op: => T): T = global.afterPhase(mboxBridgePhase)(op)
  def beforeMiniboxBridge[T](op: => T): T = global.beforePhase(mboxBridgePhase)(op)
  def afterMiniboxBridgeNext[T](op: => T): T = global.afterPhase(mboxBridgePhase.next)(op)
  def beforeMiniboxBridgeNext[T](op: => T): T = global.beforePhase(mboxBridgePhase.next)(op)

  val common: CommonDefinitions { val global: MiniboxBridgeComponent.this.global.type }
  def flags = common /* common contains the flags as well */
}


/** Introduces explicit Coerceations from `T` to `@storage T` and back */
trait MiniboxCoerceComponent extends
    PluginComponent
    with MiniboxCoerceTreeTransformer
    with MiniboxAnnotationCheckers
    with MbArrayDefinitions
    with ScalacCrossCompilingLayer {

  val minibox: MiniboxInjectComponent { val global: MiniboxCoerceComponent.this.global.type }

  def mboxCoercePhase: StdPhase

  def afterMiniboxCoerce[T](op: => T): T = global.afterPhase(mboxCoercePhase)(op)
  def beforeMiniboxCoerce[T](op: => T): T = global.beforePhase(mboxCoercePhase)(op)

  val common: CommonDefinitions { val global: MiniboxCoerceComponent.this.global.type }
  def flags = common /* common contains the flags as well */
}


/** Specializer component `T @storage -> Long` */
trait MiniboxCommitComponent extends
    PluginComponent
    with MiniboxCommitInfoTransformer
    with MiniboxCommitTreeTransformer
    with MbArrayDefinitions
    with ScalacCrossCompilingLayer {

  val minibox: MiniboxInjectComponent { val global: MiniboxCommitComponent.this.global.type }
  val interop: InteropInjectComponent { val global: MiniboxCommitComponent.this.global.type }

  def mboxCommitPhase: StdPhase

  def afterMiniboxCommit[T](op: => T): T = global.afterPhase(mboxCommitPhase)(op)
  def beforeMiniboxCommit[T](op: => T): T = global.beforePhase(mboxCommitPhase)(op)

  val common: CommonDefinitions { val global: MiniboxCommitComponent.this.global.type }
  def flags = common /* common contains the flags as well */
}

trait PreTyperComponent extends
  PluginComponent
  with TypingTransformers
  with ScalacCrossCompilingLayer {

  val minibox: MiniboxInjectComponent { val global: PreTyperComponent.this.global.type }
  val common: CommonDefinitions { val global: PreTyperComponent.this.global.type }
  def flags = common /* common contains the flags as well */
}

trait PostTyperComponent extends
  PluginComponent
  with TypingTransformers
  with ScalacCrossCompilingLayer {

  import global._
  import global.Flag._
  val minibox: MiniboxInjectComponent { val global: PostTyperComponent.this.global.type }

  val common: CommonDefinitions { val global: PostTyperComponent.this.global.type }
  def flags = common /* common contains the flags as well */
}

/** Tree preparer before retyping the tree */
trait TweakErasureComponent extends
    PluginComponent
    with TweakErasureTreeTransformer
    with ScalacCrossCompilingLayer {

  val interop: InteropInjectComponent { val global: TweakErasureComponent.this.global.type }

  def tweakErasurePhase: Phase

  def afterTweakErasure[T](op: => T): T = global.afterPhase(tweakErasurePhase)(op)
  def beforeTweakErasure[T](op: => T): T = global.beforePhase(tweakErasurePhase)(op)

  val common: CommonDefinitions { val global: TweakErasureComponent.this.global.type }
  def flags = common /* common contains the flags as well */
}


object VersionChecker {

  def versionMessage(): Option[String] = {
    try {
      val supportedVersion = new ScalacVersion {}
      val supportedMajor = supportedVersion.scalaVersionMajor
      val supportedMinor = supportedVersion.scalaVersionMinor
      val supportedRevision = supportedVersion.scalaVersionRevision

      val actualVersion = scala.util.Properties.versionString.replaceAll("version ", "")
      val versionComponents = actualVersion.split("\\.")

      val actualMajor = versionComponents(0).toInt
      val actualMinor = versionComponents(1).toInt
      val actualRevision = versionComponents(2).replaceAll("-.*", "").toInt

      val tag = versionComponents(2).replaceAll("^[0-9]*-","")

      if (actualMajor != supportedMajor)
        Some("The miniboxing plugin does not support this version of Scala. There is no way it will work correctly! " +
             "The supported version is " + supportedVersion.scalaVersion +
             ", while the current Scala compiler version is " + actualVersion + ".")
      else if (actualMinor != supportedMinor)
        Some("The miniboxing plugin does not match the Scala minor version. Please update the miniboxing plugin version " +
             "you are using to compile the current project. The supported version is " + supportedVersion.scalaVersion +
             ", while the current Scala compiler version is " + actualVersion + ".")
      else if (actualRevision != supportedRevision)
        Some("The miniboxing plugin does not match the Scala revision. If you encounter errors, please use the " +
             "Scala compiler version " + supportedVersion.scalaVersion + " or switch to a version of the miniboxing " +
             "plugin that supports Scala " + actualVersion + " (which you are using now).")
      else
        None
    } catch {
      case _: Throwable =>
        Some("The miniboxing plugin could not check Scala version. " +
             "Please make sure the miniboxing plugin version matches the Scala compiler.")
    }
  }
}