package miniboxing.tests.compile.erasure10

// There is a problem with this test, when typing x1.toString
// since x1 will have type x1.type (which expands to @storage T)
// and pt will be @storage T. But x1.type is not a subtype of
// @storage T, as assesed by isSubType:
// isSubType: DDDDD.this.x1.type vs Tsp @storage
//    scala.reflect.internal.Types$class.isSubType2(Types.scala:6036)
//    scala.reflect.internal.Types$class.isSubType(Types.scala:5831)
//    scala.reflect.internal.SymbolTable.isSubType(SymbolTable.scala:13)
//    scala.reflect.internal.Types$Type.$less$colon$less(Types.scala:872)
//    scala.tools.nsc.typechecker.Typers$Typer.fallBack$1(Typers.scala:1171)
//    scala.tools.nsc.typechecker.Typers$Typer.adapt(Typers.scala:1283)
//    scala.tools.nsc.typechecker.Typers$Typer.typed(Typers.scala:5637)
//    miniboxing.plugin.MiniboxAdaptTreeTransformer$TreeAdapters$TreeAdapter.typed(MiniboxAdaptTreeTransformer.scala:136)
//    miniboxing.plugin.MiniboxAdaptTreeTransformer$TreeAdapters$TreeAdapter$$anonfun$1.apply(MiniboxAdaptTreeTransformer.scala:94)
//    miniboxing.plugin.MiniboxAdaptTreeTransformer$TreeAdapters$TreeAdapter$$anonfun$1.apply(MiniboxAdaptTreeTransformer.scala:94)
// isSubType: DDDDD.this.x1.type vs Tsp
//    scala.reflect.internal.Types$class.isSubType2(Types.scala:6036)
//    scala.reflect.internal.Types$class.isSubType(Types.scala:5831)
//    scala.reflect.internal.SymbolTable.isSubType(SymbolTable.scala:13)
//    scala.reflect.internal.Types$class.firstTry$1(Types.scala:6069)
//    scala.reflect.internal.Types$class.isSubType2(Types.scala:6224)
//    scala.reflect.internal.Types$class.isSubType(Types.scala:5831)
//    scala.reflect.internal.SymbolTable.isSubType(SymbolTable.scala:13)
//    scala.reflect.internal.Types$Type.$less$colon$less(Types.scala:872)
//    scala.tools.nsc.typechecker.Typers$Typer.fallBack$1(Typers.scala:1171)
//    scala.tools.nsc.typechecker.Typers$Typer.adapt(Typers.scala:1283)
// isSubType: DDDDD.this.x1.type vs Nothing
//    scala.reflect.internal.Types$class.isSubType2(Types.scala:6036)
//    scala.reflect.internal.Types$class.isSubType(Types.scala:5831)
//    scala.reflect.internal.SymbolTable.isSubType(SymbolTable.scala:13)
//    scala.reflect.internal.Types$class.thirdTryRef$1(Types.scala:6124)
//    scala.reflect.internal.Types$class.thirdTry$1(Types.scala:6141)
//    scala.reflect.internal.Types$class.secondTry$1(Types.scala:6105)
//    scala.reflect.internal.Types$class.firstTry$1(Types.scala:6066)
//    scala.reflect.internal.Types$class.isSubType2(Types.scala:6224)
//    scala.reflect.internal.Types$class.isSubType(Types.scala:5831)
//    scala.reflect.internal.SymbolTable.isSubType(SymbolTable.scala:13)
// isSubType: Tsp @storage vs Nothing
//    scala.reflect.internal.Types$class.isSubType2(Types.scala:6036)
//    scala.reflect.internal.Types$class.isSubType(Types.scala:5831)
//    scala.reflect.internal.SymbolTable.isSubType(SymbolTable.scala:13)
//    scala.reflect.internal.Types$class.fourthTry$1(Types.scala:6219)
//    scala.reflect.internal.Types$class.thirdTryRef$1(Types.scala:6119)
//    scala.reflect.internal.Types$class.thirdTry$1(Types.scala:6141)
//    scala.reflect.internal.Types$class.secondTry$1(Types.scala:6105)
//    scala.reflect.internal.Types$class.firstTry$1(Types.scala:6066)
//    scala.reflect.internal.Types$class.isSubType2(Types.scala:6224)
//    scala.reflect.internal.Types$class.isSubType(Types.scala:5831)
// isSubType: Tsp vs Nothing
//    scala.reflect.internal.Types$class.isSubType2(Types.scala:6036)
//    scala.reflect.internal.Types$class.isSubType(Types.scala:5831)
//    scala.reflect.internal.SymbolTable.isSubType(SymbolTable.scala:13)
//    scala.reflect.internal.Types$class.secondTry$1(Types.scala:6091)
//    scala.reflect.internal.Types$class.firstTry$1(Types.scala:6066)
//    scala.reflect.internal.Types$class.isSubType2(Types.scala:6224)
//    scala.reflect.internal.Types$class.isSubType(Types.scala:5831)
//    scala.reflect.internal.SymbolTable.isSubType(SymbolTable.scala:13)
//    scala.reflect.internal.Types$class.fourthTry$1(Types.scala:6219)
//    scala.reflect.internal.Types$class.thirdTryRef$1(Types.scala:6119)
// isSubType: Any vs Nothing
//    scala.reflect.internal.Types$class.isSubType2(Types.scala:6036)
//    scala.reflect.internal.Types$class.isSubType(Types.scala:5831)
//    scala.reflect.internal.SymbolTable.isSubType(SymbolTable.scala:13)
//    scala.reflect.internal.Types$class.fourthTry$1(Types.scala:6209)
//    scala.reflect.internal.Types$class.thirdTryRef$1(Types.scala:6119)
//    scala.reflect.internal.Types$class.firstTry$1(Types.scala:6064)
//    scala.reflect.internal.Types$class.isSubType2(Types.scala:6224)
//    scala.reflect.internal.Types$class.isSubType(Types.scala:5831)
//    scala.reflect.internal.SymbolTable.isSubType(SymbolTable.scala:13)
//    scala.reflect.internal.Types$class.secondTry$1(Types.scala:6091)
// isSubType: Tsp @storage vs Tsp
//    scala.reflect.internal.Types$class.isSubType2(Types.scala:6036)
//    scala.reflect.internal.Types$class.isSubType(Types.scala:5831)
//    scala.reflect.internal.SymbolTable.isSubType(SymbolTable.scala:13)
//    scala.reflect.internal.Types$class.fourthTry$1(Types.scala:6219)
//    scala.reflect.internal.Types$class.thirdTryRef$1(Types.scala:6125)
//    scala.reflect.internal.Types$class.thirdTry$1(Types.scala:6141)
//    scala.reflect.internal.Types$class.secondTry$1(Types.scala:6105)
//    scala.reflect.internal.Types$class.firstTry$1(Types.scala:6066)
//    scala.reflect.internal.Types$class.isSubType2(Types.scala:6224)
//    scala.reflect.internal.Types$class.isSubType(Types.scala:5831)
//
// annotationsConform: Tsp @storage vs Tsp: false
//
// To fix the problem, we take just set the tree's type to the correct value,
// but this prohibits us from checking miniboxing-adapt, as the phase generates
// seemingly incorrect code :(

class CCCCC[@miniboxed T](c: T) {
  def test() = {  // necessary for miniboxing to duplicate body in CCCC_J
    class DDDDD { // necessary to trigger isSubType issue
      val x1: T = c
      println(x1.toString)
    }
  }
}
