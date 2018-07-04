{-# LANGUAGE FlexibleInstances #-}

module Lib
    ( parseAlg
    , m
    , e
    , s
    , x
    , y
    , z
    , algToFacelets
    , initFaceletMap
    )
where

import           Math.Projects.Rubik
import           Math.Algebra.Group.PermutationGroup
import           Math.Core.Utils
import           Data.List
import           Data.Maybe
import           Data.Tuple

-- http://hackage.haskell.org/package/HaskellForMaths-0.4.8/docs/src/Math-Projects-Rubik.html
-- http://rubiks.wikia.com/wiki/Notation
m, e, s :: Permutation Integer
m = p [[12, 2, 32, 58], [18, 8, 38, 52], [15, 5, 35, 55]]
e = p [[24, 4, 44, 54], [25, 5, 45, 55], [26, 6, 46, 56]]
s = p [[16, 22, 34, 48], [15, 25, 35, 45], [14, 28, 36, 42]]

x, y, z :: Permutation Integer
x = r * m ^- 1 * l ^- 1
y = u * e ^- 1 * d ^- 1
z = f * s ^- 1 * b ^- 1

--           11 12 13
--           14  U 16
--           17 18 19
-- 21 22 23   1  2  3  41 42 43  51 52 53
-- 24  L 26   4  F  6  44  R 46  54  B 56
-- 27 28 29   7  8  9  47 48 49  57 58 59
--           31 32 33
--           34  D 36
--           37 38 39

parseAlg :: String -> Algorithm
parseAlg s = parseAlg' s []
    where
        parseAlg' "" a = reverse a
        parseAlg' [x] a = reverse $ (permFromString [x]):a
        parseAlg' (x:y:xs) a  | isNothing $ [x] `lookup` permNotations = parseAlg' (y:xs) a
                             | y == '2' || y == '\'' = parseAlg' xs $ (permFromString [x, y]):a
                             | otherwise = parseAlg' (y:xs) $ (permFromString [x]):a

instance {-# Overlapping #-} (Show Algorithm) where
    show a = concat $ intersperse " " $ map permToString a

permNotations :: [(String, Permutation Integer)]
permNotations =
    let base =
            [ ("F", f)
            , ("U", u)
            , ("D", d)
            , ("R", r)
            , ("L", l)
            , ("B", b)
            , ("M", m)
            , ("S", s)
            , ("E", e)
            , ("x", x)
            , ("y", y)
            , ("z", z)
            ]
    in  base
        ++ map (\(n, p) -> (n ++ "'", p ^- 1)) base
        ++ map (\(n, p) -> (n ++ "2", p ^ 2))  base

permFromString :: String -> Permutation Integer
permFromString s = fromJust $ lookup s permNotations

permToString :: Permutation Integer -> String
permToString s = fromJust $ lookup s $ map swap permNotations

type Algorithm = [Permutation Integer]

-- http://cube.crider.co.uk/visualcube.php

algToFacelets :: Algorithm -> String
algToFacelets alg = mapMaybe (\(n, _) -> n `lookup` faceletMap) initFaceletMap
  where
    perm       = product alg
    faceletMap = map (\(n, f) -> (n .^ perm, f)) initFaceletMap

initFaceletMap :: [(Integer, Char)]
initFaceletMap =
    concatMap (\(nn, f) -> nn `zip` repeat f) $ indices `zip` facelets
  where
    indices =
        [ [11 .. 19] -- U
        , [41 .. 49] -- R
        , [01 .. 09] -- F
        , [31 .. 39] -- D
        , [21 .. 29] -- L
        , [51 .. 59] -- B
        ]
    facelets = "urfdlb"
