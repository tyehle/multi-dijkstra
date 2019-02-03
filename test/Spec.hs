import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import MultiDijkstra


main :: IO ()
main = do
  putStrLn ""
  print $ fmap (inOrder . snd) (test 0) == Just "abab"
  print $ fmap (inOrder . snd) (test 2) == Just "aaaab"
  putStrLn "0 Cost Paths"
  maybe (return ()) (putStrLn . prettyTree . snd) $ test 0
  putStrLn "2 Cost Paths"
  maybe (return ()) (putStrLn . prettyTree . snd) $ test 2


inOrder :: Tree a -> [a]
inOrder (Leaf (Term a)) = [a]
inOrder (Leaf (NonTerm _)) = undefined
inOrder (Node _ children) = concatMap inOrder children


testGrammar :: Grammar Char
testGrammar = Grammar $ Map.fromList
    [ ("S", Set.singleton [NonTerm "A", NonTerm "B"])
    , ("A", Set.fromList [ [Term 'a', NonTerm "B", Term 'a']
                         , replicate 4 (Term 'a')
                         ])
    , ("B", Set.singleton [Term 'b'])
    ]


test :: Int -> Maybe (Int, Tree Char)
test w = dijkstra testGrammar (const w) start end
  where
    start = [(1, Term 'a'), (1, Term 'b')]
    end = Set.singleton $ NonTerm "S"
