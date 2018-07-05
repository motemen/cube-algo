module Algorithms
where

import           Lib                            ( parseAlg )

-- PLL
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

oll27 = parseAlg "(U L U' R') (U L' U' R)"
oll26 = parseAlg "(R' U L U') (R U L')"
oll21 = parseAlg "R U R' U R U' R' U R U2 R'"