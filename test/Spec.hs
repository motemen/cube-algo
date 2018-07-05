import           Test.Hspec
import           Lib
import           Math.Projects.Rubik
import           Math.Core.Utils
import           Math.Algebra.Group.PermutationGroup
import           Control.Monad

main :: IO ()
main =
  hspec
    $ describe "parseAlg parses"
    $ do
        let tests =
              [ ("F"      , [f])
              , ("F'"     , [f ^- 1])
              , ("F2"     , [f ^ 2])
              , ("F2U2"   , [f ^ 2, u ^ 2])
              , ("U'2"    , [(u ^- 1) ^ 2])
              , ("RUR'"   , [r, u, r ^- 1])
              , ("R U R'" , [r, u, r ^- 1])
              , ("(RU)2"  , [r, u, r, u])
              , ("(RU)2R" , [r, u, r, u, r])
              , ("(RU)'RU", [u ^- 1, r ^- 1, r, u]) -- NOTE: in reality such algo would not exist
              ]

        forM_ tests
          $ \(s, p) ->
              it ("parses \"" ++ s ++ "\" -> " ++ show p)
                $          parseAlg s
                `shouldBe` p
