module Interp where

import Grammars
import Data.List (nub)

-- RETO 3: sustitucion nominal que evita captura
freeVars :: ASA -> [String]
freeVars = nub . go
    where
        go (Id x) = [x]
        go (Let bs body) =


names :: ASA -> [String]
names e = nub $ case e of
    Id x -> [x]
    Let bs body -> map fst bs ++ concatMap (names . snd) bs ++ names body
    LetStar bs body -> map fst bs ++ concatMap (names . snd) bs ++ names body
    

freshName :: [String] -> String

sust :: ASA -> String -> ASA -> ASA

sustMany :: ASA -> [Binding] -> ASA

-- RETO 4: semantica operacional de paso grande
-- let es simultaneo; let* se evalua directamente, asociacion por asociacion.
bigStep :: ASA -> Maybe ASA
