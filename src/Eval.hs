{-# LANGUAGE RankNTypes #-}

module Eval where

import Data.Sequence hiding (reverse)

data Molecule = A | C | G | T | U
    deriving (Show, Eq)

instance Read Molecule where
    readsPrec _ value =
        case value of
            "A" -> [(A, "")]
            "a" -> [(A, "")]
            "C" -> [(C, "")]
            "c" -> [(C, "")]
            "G" -> [(G, "")]
            "g" -> [(G, "")]
            "T" -> [(T, "")]
            "t" -> [(T, "")]
            "U" -> [(U, "")]
            "u" -> [(U, "")]
            _ -> []

data Expr
    = MolE Molecule
    | SeqE [Molecule]
    | --    | NumE (forall a. (Num a) => a)
      Fun String Expr

--    | NumV (forall a. (Num a) => a)

eval :: Expr -> Seq Molecule
eval (MolE m) = singleton m
eval (SeqE ms) = fromList ms
eval (Fun "transcribe" e) =
    case e of
        (MolE m) -> singleton $ transcribe m
        (SeqE ms) -> fromList $ map transcribe ms
        _ -> error "not passing functions yet"
eval (Fun "complement" e) =
    case e of
        (MolE m) -> singleton $ complement m
        (SeqE ms) -> fromList $ map complement ms
        _ -> error "not passing functions yet"
eval (Fun "reverse_complement" e) =
    case e of
        (SeqE ms) -> fromList $ reverseComplement ms
        _ -> error "can only apply to sequences"
eval (Fun s _) = error $ "function: " ++ s ++ " not defined"

transcribe :: Molecule -> Molecule
transcribe T = U
transcribe x = x

complement :: Molecule -> Molecule
complement A = T
complement T = A
complement C = G
complement G = C
complement U = U

reverseComplement :: [Molecule] -> [Molecule]
reverseComplement = reverse . map complement

{-
data BinOp
    = Add
    | Sub
    | Mult
    | Div

binaryDecision :: BinOp -> (Value -> Value -> Value)
binaryDecision Add = add'
binaryDecision Sub = sub'
binaryDecision Mult = mult'
binaryDecision Div = div'

add' :: Value -> Value -> Value
add' (NumV x) (NumV y) = NumV (x + y)
add' _ _ = error "+ should only match on Number"

sub' :: Value -> Value -> Value
sub' (NumV x) (NumV y) = NumV (x - y)
sub' _ _ = error "- should only match on Number"

mult' :: Value -> Value -> Value
mult' (NumV x) (NumV y) = NumV (x * y)
mult' _ _ = error "* should only match on Number"

div' :: Value -> Value -> Value
div' (NumV x) (NumV y) = NumV (x `div` y)
div' _ _ = error "/ should only match on Number"
-}
