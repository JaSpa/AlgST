{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module AlgST.Builtins.Names where

import AlgST.Builtins.TH2
import AlgST.Rename.Modules (ModuleMap)
import AlgST.Syntax.Name
import AlgST.Syntax.Operators
import AlgST.Util

$( runDefines (ModuleName "Builtin") "builtinsPartialModuleMap" "builtinOperators" do
     let prefix p s = p ++ capitalise s
     let parens x = Unqualified $ "(" ++ x ++ ")"
     let typ x = defineType (prefix "type" x) (Unqualified x)
     let con x = defineValue (prefix "con" x) (Unqualified x)
     let val x = defineValue (prefix "val" x) (Unqualified x)
     let op n = defineOperator (Just (prefix "op" n)) . parens
     let op_ = defineOperator Nothing . parens

     defineValue "conPair" $ Unqualified "(,)"

     typ "Int"
     typ "Char"
     typ "String"

     typ "Bool"
     con "True"
     con "False"

     val "sendLin"
     val "receive"

     val "wait"
     val "terminate"

     val "trace"
     val "traceMsg"
     val "usleep"

{- ORMOLU_DISABLE -}
     op_           "||"  L  POr
     op_           "&&"  L  PAnd
     op_           "<"   NA PCmp
     op_           ">"   NA PCmp
     op "LEQ"      "<="  NA PCmp
     op_           ">="  NA PCmp
     op_           "=="  NA PCmp
     op_           "/="  NA PCmp
     op "add"      "+"   L  PAddSub
     op "sub"      "-"   L  PAddSub
     op "mul"      "*"   L  PMulDiv
     op "div"      "/"   L  PMulDiv
     op "mod"      "%"   L  PMulDiv
     op "pipeFwd"  "|>"  L  PForward
     op "pipeBwd"  "<|"  R  PBackward
     op "mapAfter" "<&>" NA PBackward
     op "appComb"  "<*>" NA PBackward
{- ORMOLU_ENABLE -}
 )

builtinOperators :: OperatorTable
builtinsPartialModuleMap :: ModuleMap

pattern BuiltinsModule :: ModuleName
pattern BuiltinsModule = ModuleName "Builtin"
