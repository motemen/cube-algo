module Algorithms where

import           Lib                            ( parseAlg )

-- PLL
-- https://www.speedsolving.com/wiki/index.php/PLL

algoNames = [
    ( pllH, "H" ),
    ( pllUa, "Ua" ),
    ( pllUb, "Ub" ),
    ( pllZ, "Z" ),
    ( pllAa, "Aa" ),
    ( pllAb, "Ab" ),
    ( pllE, "E" ),
    ( pllF, "F" ),
    ( pllGa, "Ga" ),
    ( pllGb, "Gb" ),
    ( pllGc, "Gc" ),
    ( pllGd, "Gd" ),
    ( pllJa, "Ja" ),
    ( pllJb, "Jb" ),
    ( pllNa, "Na" ),
    ( pllNb, "Nb" ),
    ( pllRa, "Ra" ),
    ( pllRb, "Rb" ),
    ( pllT, "T" ),
    ( pllV, "V" ),
    ( pllY, "Y" ),
    ( oll27, "OLL 27" ),
    ( oll26, "OLL 26" ),
    ( oll21, "OLL 21" ),
    ( oll22, "OLL 22" ),
    ( oll23, "OLL 23" ),
    ( oll24, "OLL 24" ),
    ( oll25, "OLL 25" ),
    ( oll1, "OLL 1" ),
    ( oll2, "OLL 2" ),
    ( oll3, "OLL 3" ),
    ( oll4, "OLL 4" ),
    ( oll17, "OLL 17" ),
    ( oll18, "OLL 18" ),
    ( oll19, "OLL 19" ),
    ( oll31, "OLL 31" ),
    ( oll32, "OLL 32" ),
    ( oll43, "OLL 43" ),
    ( oll44, "OLL 44" ),
    ( oll36, "OLL 36" ),
    ( oll38, "OLL 38" ),
    ( oll48, "OLL 48" ),
    ( oll47, "OLL 47" ),
    ( oll53, "OLL 53" ),
    ( oll54, "OLL 54" ),
    ( oll49, "OLL 49" ),
    ( oll50, "OLL 50" ),
    ( oll34, "OLL 34" ),
    ( oll46, "OLL 46" ),
    ( oll33, "OLL 33" ),
    ( oll45, "OLL 45" ),
    ( oll55, "OLL 55" ),
    ( oll56, "OLL 56" ),
    ( oll51, "OLL 51" ),
    ( oll52, "OLL 52" ),
    ( oll5, "OLL 5" ),
    ( oll6, "OLL 6" ),
    ( oll39, "OLL 39" ),
    ( oll40, "OLL 40" ),
    ( oll7, "OLL 7" ),
    ( oll8, "OLL 8" ),
    ( oll11, "OLL 11" ),
    ( oll12, "OLL 12" ),
    ( oll9, "OLL 9" ),
    ( oll10, "OLL 10" ),
    ( oll35, "OLL 35" ),
    ( oll37, "OLL 37" ),
    ( oll13, "OLL 13" ),
    ( oll14, "OLL 14" ),
    ( oll15, "OLL 15" ),
    ( oll16, "OLL 16" ),
    ( oll29, "OLL 29" ),
    ( oll30, "OLL 30" ),
    ( oll41, "OLL 41" ),
    ( oll42, "OLL 42" ),
    ( oll28, "OLL 28" ),
    ( oll57, "OLL 57" ),
    ( oll20, "OLL 20" )
    ]

plls =
    [ pllH
    , pllUa
    , pllUb
    , pllZ
    , pllAa
    , pllAb
    , pllE
    , pllF
    , pllGa
    , pllGb
    , pllGc
    , pllGd
    , pllJa
    , pllJb
    , pllNa
    , pllNb
    , pllRa
    , pllRb
    , pllT
    , pllV
    , pllY
    ]

pllH = parseAlg "M2 D S2 D2 S2 D M2"
pllUa = parseAlg "B2 U' M U2 M' U' B2"
pllUb = parseAlg "B2 U M U2 M' U B2"
pllZ = parseAlg "(M2' U)2 M' (U2 M2' U2) M' U2"

pllAa = parseAlg "R' F R' B2 R F' R' B2 R2"
pllAb = parseAlg "R B' R F2 R' B R F2 R2"
pllE = parseAlg "(y x') (R U' R' D) (R U R' D') (R U R' D) (R U' R' D') (x)"
pllF = parseAlg "(R' U R U') R2 (F' U' F U) (R F R' F') R2 U'"

pllGa = parseAlg "(y) R2 U (R' U R' U') R U' R2 (D U' R' U) R D'"
pllGb = parseAlg "R' U' R (y) R2 u (R' U R U' R) u' R2"
pllGc = parseAlg "(y) R2' u' (R U' R U R') u R2 (y) R U' R'"
pllGd = parseAlg "(y2) R U R' (y') R2 u' (R U' R' U R') u R2"

pllJa = parseAlg "(B' U F') U2 (B U' B') U2 (F B U')"
pllJb = parseAlg "(B U' F) U2 (B' U B) U2 (F' B' U)"

pllNa = parseAlg "(L U' R U2 L' U R')2 U'"
pllNb = parseAlg "(R' U L' U2 R U' L)2 U"

pllRa = parseAlg "R U2 R' U2 R B' R' U' R U R B R2 U"
pllRb = parseAlg "R' U2 R U2 R' F (R U R' U') R' F' R2' U'"

pllT = parseAlg "R U R' U' R' F R2 U' R' U' R U R' F'"
pllV = parseAlg "R' U R' U' B' R' B2 U' B' U B' R B R"
pllY = parseAlg "F R U' R' U' R U R' F' R U R' U' R' F R F'"

-- OLL
-- https://www.speedsolving.com/wiki/index.php/OLL

colls = [oll27, oll26, oll21, oll22, oll23, oll24, oll25]

olls =
    colls
        ++ [ oll1
           , oll2
           , oll3
           , oll4
           , oll17
           , oll18
           , oll19
           , oll31
           , oll32
           , oll43
           , oll44
           , oll36
           , oll38
           , oll48
           , oll47
           , oll53
           , oll54
           , oll49
           , oll50
           , oll34
           , oll46
           , oll33
           , oll45
           , oll55
           , oll56
           , oll51
           , oll52
           , oll5
           , oll6
           , oll39
           , oll40
           , oll7
           , oll8
           , oll11
           , oll12
           , oll9
           , oll10
           , oll35
           , oll37
           , oll13
           , oll14
           , oll15
           , oll16
           , oll29
           , oll30
           , oll41
           , oll42
           , oll28
           , oll57
           , oll20
           ]

oll27 = parseAlg "(U L U' R') (U L' U' R)"
oll27' = parseAlg "R U R' U R U2 R'"
oll26 = parseAlg "(R' U L U') (R U L')"
oll21 = parseAlg "R U R' U R U' R' U R U2 R'"
oll22 = parseAlg "R U2 R2' U' R2 U' R2' U2 R"
oll23 = parseAlg "R2' D' R U2 R' D R U2 R"
oll24 = parseAlg "L F R' F' L' F R F'"
oll25 = parseAlg "R U2 R' U' (R U R' U') (R U R' U') R U' R'"

oll1 = parseAlg "(R U2) (R2' F R F' U2') (R' F R F')"
oll2 = parseAlg "F (R U R' U') F' f (R U R' U') f'"
oll3 = parseAlg "(y) f (R U R' U') f' U' F (R U R' U') F'"
oll4 = parseAlg "f (R U R' U') f' U F (R U R' U') F'"
oll17 = parseAlg "R U R' U (R' F R F') U2 (R' F R F')"
oll18 = parseAlg "(y') r U R' U R U2 r2' U' R U' R' U2 r"
oll19 = parseAlg "r' R U R U R' U' r R2' F R F'"
oll31 = parseAlg "(y2) R' U' F U R U' R' F' R"
oll32 = parseAlg "(y2) x' R U R' D R U' R U' R' U R' D' x"
oll43 = parseAlg "(y) R' U' F' U F R"
oll44 = parseAlg "f (R U R' U') f'"
oll36 = parseAlg "R' U' R U' R' U R U R y R' F' R"
oll38 = parseAlg "(y2) (R U R' U) (R U' R' U') (R' F R F')"
oll48 = parseAlg "F (R U R' U') (R U R' U') F'"
oll47 = parseAlg "F' (L' U' L U) (L' U' L U) F"
oll53 = parseAlg "l' U' L U' L' U L U' L' U2 l"
oll54 = parseAlg "r U R' U R U' R' U R U2 r'"
oll49 = parseAlg "R B' R2 F R2 B R2 F' R"
oll50 = parseAlg "(y2) R' F R2 B' R2 F' R2 B R'"
oll34 = parseAlg "(R U R' U') x D' R' U R E' z'"
oll46 = parseAlg "R' U' R' F R F' U R"
oll33 = parseAlg "(R U R' U') (R' F R F')"
oll45 = parseAlg "F (R U R' U') F'"
oll55 = parseAlg "(y) R' F (R U R U') R2 F' R2 U' R' (U R U R')"
oll56 = parseAlg "r U r' U R U' R' U R U' R' r U' r'"
oll51 = parseAlg "F U R U' R' U R U' R' F'"
oll52 = parseAlg "R' U' R U' R' d R' U R B"
oll5 = parseAlg "(r' U2) (R U R' U r)"
oll6 = parseAlg "(y2) r U2 R' U' R U' r'"
oll39 = parseAlg "L F' (L' U' L U) F U' L'"
oll40 = parseAlg "R' F (R U R' U') F' U R"
oll7 = parseAlg "r U R' U R U2 r'"
oll8 = parseAlg "l' U' L U' L' U2 l"
oll11 = parseAlg " F' (L' U' L U) F y F (R U R' U') F'"
oll12 = parseAlg "F (R U R' U') F' U F (R U R' U') F'"
oll9 = parseAlg "(y') (R U R' U') R' F R2 U R' U' F'"
oll10 = parseAlg "(y') R U R' y R' F R U' R' F' R"
oll35 = parseAlg "R U2 R2' F R F' R U2 R'"
oll37 = parseAlg "(y) F R U' R' U' R U R' F'"
oll13 = parseAlg "F U R U' R2 F' R U R U' R'"
oll14 = parseAlg "R' F R U R' F' R y' R U' R'"
oll15 = parseAlg "(y2) l' U' l  (L' U' L U) l' U l"
oll16 = parseAlg "(y2) r U r' (R U R' U') r U' r'"
oll29 = parseAlg "M U (R U R' U') (R' F R F') M'"
oll30 = parseAlg "r' U2 R U R' U r R U2 R' U' R U' R'"
oll41 = parseAlg "R U' R' U2 R U y R U' R' y' U' R'"
oll42 = parseAlg "r' R2 y R U R' U' y' R' U M'"
oll28 = parseAlg "r U R' U' r' R U R U' R'"
oll57 = parseAlg "R U R' U' M' U R U' r' "
oll20 = parseAlg "r' R U R U R' U' r2 R2' U R U' r'"
