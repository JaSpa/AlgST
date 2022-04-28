{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module AlgST.Builtins where

import AlgST.Builtins.TH
import AlgST.Parse.Phase
import AlgST.Syntax.Decl
import AlgST.Syntax.Program
import Prelude hiding (all)

builtins :: PProgram
builtins =
  $$( let sigs =
            [ -- Session operations.
              (,) "send" "∀(a : ML). ∀(s : SL). a -> !a.s -o s",
              (,) "receive" "∀(a : ML). ∀(s : SL). ?a.s -> (a, s)",
              -- Arithmetic operations.
              (,) "(+)" "Int -> Int -> Int",
              (,) "(-)" "Int -> Int -> Int",
              (,) "(*)" "Int -> Int -> Int",
              (,) "(/)" "Int -> Int -> Int",
              (,) "(%)" "Int -> Int -> Int",
              -- Base comparison function. All other comparison functions are
              -- defined through this function.
              (,) "(<=)" "Int -> Int -> Bool"
            ]
          defs =
            [ "data String : MU",
              "data Char : MU",
              "data Int : MU",
              "data Bool = True | False",
              --
              "negate : Int -> Int",
              "negate n = 0 - n",
              --
              "(|>) : ∀(a:TL). ∀(b:TL). a -> (a -o b) -o b",
              "(|>) x f = f x",
              --
              "(<|) : ∀(a:TL). ∀(b:TL). (a -o b) -> a -o b",
              "(<|) f x = f x",
              --
              "not : Bool -> Bool",
              "not b = if b then False else True",
              --
              "(&&) : Bool -> Bool -> Bool",
              "(&&) a b = if a then b else False",
              --
              "(||) : Bool -> Bool -> Bool",
              "(||) a b = if a then True else b",
              --
              "(==) : Int -> Int -> Bool",
              "(==) a b = a <= b && b <= a",
              --
              "(/=) : Int -> Int -> Bool",
              "(/=) a b = not (a == b)",
              --
              "(<) : Int -> Int -> Bool",
              "(<) a b = a <= b && a /= b",
              --
              "(>) : Int -> Int -> Bool",
              "(>) a b = a >= b && a /= b",
              --
              "(>=) : Int -> Int -> Bool",
              "(>=) a b = b <= a",
              --
              "fst : ∀(a:TL). ∀(b:TU). (a, b) -> a",
              "fst ab = let (a, _) = ab in a",
              --
              "snd : ∀(a:TU). ∀(b:TL). (a, b) -> b",
              "snd ab = let (_, b) = ab in b"
            ]
       in [||
          let p = $$(parseStatic sigs defs)
              markBuiltin :: TypeDecl Parse -> TypeDecl Parse
              markBuiltin = \case
                DataDecl _ decl -> DataDecl OriginBuiltin decl
                decl -> decl
           in p {programTypes = markBuiltin <$> programTypes p}
          ||]
    )
