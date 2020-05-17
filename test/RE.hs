{-# LANGUAGE TemplateHaskell #-}

module RE where

import Data.List
import System.IO.Unsafe
import Dragen
import Test.QuickCheck

data Html
  = Text String
  | Join Html Html
  | Tag  String Html
  | Sing String
  deriving Show

dragenArbitrary ''Html 10 (weighted [('Join, 2), ('Text, 2)])

megadeth = sized go_awdn
  where
    go_awdn n_awdo = if (n_awdo == 0)
        then frequency 
          [ (1, Text <$> arbitrary)
          , (1, Sing <$> arbitrary)
          ]
        else frequency
          [ (1, Text <$> arbitrary)
          , (1, Join <$> go_awdn ((max 0) (n_awdo - 1))
                     <*> go_awdn ((max 0) (n_awdo - 1)))
          , (1, Tag <$> (resize ((max 0) (n_awdo - 1))) arbitrary
                    <*> go_awdn ((max 0) (n_awdo - 1)))
          , (1, Sing <$> arbitrary)
          ]


--- simpl :: Html -> Html
--- simpl (Join (Text t1) (Text t2))
---   = Text (t1 ++ t2)
--- simpl (Join (Join (Text t1) x) y)
---   = simpl (Join (simpl (Join (Text t1) x)) y)
--- simpl (Join x y) = Join (simpl x) (simpl y)
--- simpl (Tag tag inner) = Tag tag (simpl inner) 
--- simpl x = x

simpl :: ([Int], Html) -> ([Int], Html) 
simpl (ms, Text t1 `Join` Text t2)
  = (1:ms, Text (t1 ++ t2))
simpl (ms, Text t1 `Join` x `Join` y)
  = let (ms', res) = simpl (ms, x `Join` y) 
    in simpl (2:ms', Text t1 `Join` res)
simpl (ms, Join x y) 
  = let (ms', x')  = simpl (ms, x)
        (ms'', y') = simpl (ms', y)
    in (3:ms, Join x' y')
simpl (ms, Tag tag inner) 
  = let (ms', res) = simpl (ms, inner) 
    in  (4:ms', Tag tag res) 
simpl (ms, x) = (5:ms, x)

simpl' x = simpl ([], x)

simpl_test :: Gen Html -> IO [Int]
simpl_test g = do
  let gen = resize 10 g
  vals <- sequence (replicate 100000 (generate gen))
  return (concatMap fst (simpl' <$> vals))


run_test_megadeth = map length . group . sort <$> simpl_test megadeth
run_test_dragen = map length . group . sort <$> simpl_test arbitrary

norm :: [Int] -> [Double]
norm xs = map (\x ->  x/s * 100) (fromIntegral <$> xs)
  where s = fromIntegral (sum xs)

head_ :: Html -> Html
head_ inner = Tag "head" inner

body_ :: Html -> Html
body_ inner = Tag "body" inner

hr :: Html
hr = Sing "hr"

-- text :: String -> Html
-- text str = Text (addEscapes str)

(<+>) = Join

render :: Html -> String
render (Text str) = str
render (Join x y) = render x ++ render y
render (Sing tag) = "<" ++ tag ++ ">" 
render (Tag tag inner) = "<"  ++ tag ++ ">"
                       ++ render inner 
                       ++ "</" ++ tag ++ ">"







