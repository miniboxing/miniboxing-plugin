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
//    * Vlad Ureche
//
package miniboxing.plugin
package metadata

import scala.tools.nsc.Global

trait CommonDefinitions extends Flags {

  val global: Global
  import global._

  def isCompiledInCurrentRuns(sym: Symbol): Boolean =
    sym.sourceFile != null
}

trait Flags {
  def flag_log: Boolean
  def flag_debug: Boolean
  def flag_stats: Boolean
  def flag_hijack_spec: Boolean
  def flag_spec_no_opt: Boolean
  def flag_loader_friendly: Boolean
  def flag_two_way: Boolean
  def flag_rewire_functionX_values: Boolean
  def flag_rewire_functionX_repres: Boolean
  def flag_rewire_functionX_bridges: Boolean
  def flag_mark_all: Boolean
  def flag_strict_typechecking: Boolean
  def flag_strip_miniboxed: Boolean
  def flag_create_local_specs: Boolean
  def flag_strict_warnings: Boolean
  def flag_strict_warnings_outside: Boolean
  def flag_warn_mbarrays: Boolean
  def flag_rewire_functionX_application: Boolean
  def flag_rewire_mbarray: Boolean
  def flag_rewire_tuples: Boolean
  def flag_constructor_spec: Boolean
}
