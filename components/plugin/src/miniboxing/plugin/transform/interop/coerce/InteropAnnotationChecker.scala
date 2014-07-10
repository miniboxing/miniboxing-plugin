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
package miniboxing
package plugin
package transform
package interop
package coerce

/*
                                           .. ,:~~:,.
                                  .7MMMMMMMD~,~=+$$$$$DMMMM+
      MMZIII$MMM:           .MMM$=====?IIIIIIIII~I$$$$$$$$$ZMMM
    MMNM=IIII$$$$NM$   .NMMM+,,=IIIIIIIII::::IIIIII$+++?$$$ZO88MMO
   M7MNM$+IIII$$$$88MMM$$$$=:~IIIIIIIIIII?=IIII::=I$MMMMMMMMMMMMN8MMM
  $M7.:DM=IIII$$$$888MM$$IIIIII::II7$$$$$ZZZ$$$III7Z$$7$+?+7$$$I8MMMMMM
  MNNNMZM=IIII$$$8MIIIIIIIIIIIIII7$$ZMM+==,,,ZDIIIIIIIIIIII77??7$$+$Z8MMM7.
  MD.?NMM=IIIIZM$IIIIIIIIIIIIIII$$M8==IIIIIIIIIIIIIIIII:II::::+??$$$$$Z888MMN
  MMMMMMMIII7MIIIIIIIIIIIIIIIII$M$==II$MM==?$MMZIIIII~IIIII?==?I$$$$$$++$?Z88MMM
  MMMMMMMIIIIIIIIIIIIIIIIIIIII$M=?II7MI=?$$MMOMZM7IIIIIIIIIIIIIII+?IIIII7$$$7Z88MM=
   MMMMM8IIIIIIIIIIIIIIIIIIIIII+IIIMMZMMMM8M...M?MOIIIIIIIIIII?:IIIIIII+?IIIII$$?I8M?
   .MM$MIIIIIIIIIIIIIIIIIIIIIIIIIIDMNNNNNNNMMMM?ZOMIIIIIIIIIII=:IIIIIIII:::?I:II?+?OMM
     . MNIIIIIIIIIIIIIIIIIIIIIIIIIMNNMM..8NNM=?$OOOMIIIIIIIIIIIIIIIIIIII=::IIIIIIII$$$MZ
       M7IIIIIIIIIIIIIIIIIIIIIIII7MMMMMMDMNNM7?$OOOMIII77IIIIIIII::::IIIIIIIIII:=IIII$$MM
       MIIIIIIIIIIIIIIIIIIIIIIIII7MMMMMMMMNNMN?$OOOM$MMMM7IIIIIII::::IIIIIIIIIIIIIIII:$$M8
       MII=M$IIIIIIIII7MM=IIIIIIIIMMMMMMMMNNMIIZOOMMIMM=::::~::::::::::+IIIIIII::?IIII$$$M       .
       M+I+=M$IIIIII7MM==IIIIIIIIIIMMMMMMNNMD?$ZMMZMM++:::===:::::::::====::+I~::+$$NI$$$M7MMMMMM..
       7M?II=IIIIII7N=+IIIIIIIIIIIIIIDMMMMM?7MMMMM+++~:::===========:===::::===?IIM$II$$MM+++7$$$M~
        MD=IIIIIIIIIIIIIIIIIIIIIIIIIIIIIII7IOMM7++++::::::++++++I?I8888MM::==?IIMM+II78M$:::?I$$I?M
         $M=IIIIIIIIIIIIIIIIIIIIIIIIIIIIDMM7+++++$?::::::::::~+II+7$$IZZM+IIIIIM?IIIIMDIII~~IIII$Z8M
           NM8=IIIIIIIIIIIIIIIIIII7MMMMI++++++8MO+:::::::::==:==II$$$?ZZM+IIIDM=?::IMOIIIIIIIIII$$8M
             .8MMMZ====+++++IMMMMN?++++++++ZMM+++::::::::,,:,:=?II7$$7?MMII$MM==:::MNIIIIIII?:II7$8D=
                ?MMD7??+?++++++++++++INMMMOZ++++:::::::=+++IMM:II:~$$$$MOOOMM==IIIIMIII~?IIIIIII??88M
                    .+M$ZONMMMMMMM=,MMOZ$+++++++::::::::::+++?M?II+$?+8MOODM===IIIIIIIIIII=IIOI7$$88M
           .8MI    .MM????????778M     +MMMMI+++:::::::::::++M?+III$?$MMMMM=:,IIIII:?::+IIIIMZI$$$8M~
          $M=~~$MMMMI??????????77M            IMMMI=~~~=~+8MMM==II7$$8MO8M8===?~?III:::IIIIM8I7$$88M
          M???7$MMMMM??????????77M                          +M=III??$M88ZM===:::IIII::+IIIMO7I$$88M
          NMMMM.    .M??????????7OM   ..                    M==III$$MM$77M===:::IIIIIII?=MD$I$$O8M,
                   . M=???NMMMMMI?MMMMMMM.                 .M=III$$DM$MMDM===IIIIIIII==+MZ7I$$8MM
              8MMMMMMM77ZM     .M=?????77M                 M==III$ZM    .MM====III+===MM$II$$8M~
             +M~=~~??777M       .MI~~~+?7M,               :M=III$$M.      MM========MM7III$$MM
             ZN~?????77M.         ,MMMMMMO       .      . M~=II7$M=        .MMMMMMMN7?II7$$M
              M~????77MD.          .         .M?7M.      IM~III$MM    .MMMMMMMMM777???I7$88
              ,M?7777M:                      :M=+NM     .M~~II$8M... .M???I7$7???????I$$M,
                ,MMM,                          MM7MD    DO~?I7$M.M$~DMMMMMM$?????????$$M.
                                    MM+$MMM.    M7IZMMMMM~?II$M: M????MO7?II????????$OM.
                                   M~~+~~M?~~~?????????M+??I$MN  DZMMMMMMMM8???????$M7
                                  MZ????7Z8777?????????????7ZM   ,M~?~?M87???77???7M,.
                                  .MZ77MM, :MI????????????I$M.  .M??????OMMNZM??I$M
                                    .     ,M$?????????????$M~    M?????M$~M+~=?7M?
                                    8MMI~NMI77NMMMMMM????$M.      MMDMM?~????7NM
                                    ?D~~???7DM+      ,M??7M~          .MZ??77MM..
                                   M=???7OM~.      .MN??M=              .7D,.
                                   =8?I78M,        :M~IIM.
                                    =MM8 .         M????7M=
                                                   M~~???7M
                                                   M~=??I7M
                                                   .NMDOMM~

  Frog clipart credits: www.frog-life-cycle.com
  ASCII transformation: www.glassgiant.com/ascii/
*/
trait InteropAnnotationCheckers {
  this: InteropCoerceComponent =>

  import global._
  import interop._

  object mbFunctionAnnotationChecker extends AnnotationChecker{

    /**
     *  Check the annotations on two types conform.
     *  Q: Does T <: @storage T?
     *  A: It depends. It conforms before the adaptation phase. If doesn't after :)
     *
     *  LDL FTW -- Boil frog, boil!
     */
    override def annotationsConform(tpe1: Type, tpe2: Type): Boolean = {
      if (interopCoercePhase != null && global.phase.id > interopCoercePhase.id) {
        val res1 = tpe1.isMbFunction ^ tpe2.isMbFunction
        val res2 = tpe2.isWildcard
//        if (tpe1.isMbFunction || tpe2.isMbFunction)
//          println(tpe1 + " <: " + tpe2 + "  ==> " + res1 + "  " + res2 + " ==> " + (!res1 || res2))
        !res1 || res2
      } else {
        true
      }
    }

    /** Refine the computed least upper bound of a list of types.
     *  All this should do is add annotations. */
    override def annotationsLub(tp: Type, ts: List[Type]): Type = {
      val res =
        if (ts.forall(_.isMbFunction)) // note: forall!
          if (tp.isMbFunction)
            tp
          else {
            tp.withMbFunction
          }
        else
          if (tp.isMbFunction)
            tp.withoutMbFunction
          else
            tp
      res
    }

    /** Refine the computed greatest lower bound of a list of types.
     *  All this should do is add annotations. */
    override def annotationsGlb(tp: Type, ts: List[Type]): Type =
      annotationsLub(tp, ts)
  }
}
