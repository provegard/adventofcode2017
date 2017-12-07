module Towers where
import qualified Data.Map as Map
import Data.Maybe
import Data.List
import Data.List.Unique
import Debug.Trace

data InputLine = InputLine { name :: String
                           , weight :: Int
                           , children :: [String]
                           } deriving (Eq, Show) 

data TreeNode = TreeNode { t_name :: String
                         , t_weight :: Int
                         , t_nodes :: [TreeNode]
                         } deriving (Eq, Show)

to_int x = read x :: Int

strip_trailing_comma x = rstrip x
  where
    lstrip = dropWhile (`elem` ",")
    rstrip = reverse . lstrip . reverse

-- Parses: <name> (<weight>) [-> <children>]
parse_line :: String -> InputLine
parse_line line = parse' $ words line
  where
    parse' (n:w:_:rest) = InputLine n (to_int w) (map strip_trailing_comma rest)
    parse' (n:w:_)      = InputLine n (to_int w) []
    parse' x            = error "malformed input"

lookup_by_name the_map name = fromMaybe (error ("unknown name: " ++ name)) $ Map.lookup name the_map

-- Finds the root of the tree, the nodes of which are scattered in a Map.
-- Not very efficient but only called once.
get_root :: (Map.Map String TreeNode) -> TreeNode
get_root node_map = do
  let all_names = Map.keys node_map
  let child_names = foldl (\ names node -> names ++ (map t_name (t_nodes node))) [] node_map
  let result = all_names \\ child_names
  lookup_by_name node_map (head result)

to_node_map :: [InputLine] -> (Map.Map String InputLine) -> (Map.Map String TreeNode) -> (Map.Map String TreeNode)

insert_node :: InputLine -> (Map.Map String InputLine) -> (Map.Map String TreeNode) -> (Map.Map String TreeNode)
insert_node line line_map node_map = do
  case Map.lookup (name line) node_map of
      Just n  -> node_map
      Nothing -> do
        let child_lines = map (\ l -> lookup_by_name line_map l) (children line)
        let new_map = to_node_map child_lines line_map node_map
        let child_nodes = map (\ l -> lookup_by_name new_map l) (children line)
        Map.insert (name line) (TreeNode (name line) (weight line) child_nodes) new_map

to_node_map [] line_map node_map = node_map
to_node_map lines line_map node_map = to_node_map (tail lines) line_map map_with_inserted_node
  where
    map_with_inserted_node = insert_node (head lines) line_map node_map

make_tree :: [InputLine] -> TreeNode
make_tree lines = get_root $ to_node_map lines lines_by_name Map.empty
  where
    name_line line = (name line, line)
    lines_by_name = Map.fromList $ map name_line lines

weight_of_tree :: TreeNode -> Int
weight_of_tree tree = (t_weight tree) + (sum $ map weight_of_tree (t_nodes tree))

find_changed_weight :: TreeNode -> Int -> (Maybe Int)
find_changed_weight tree target_weight = do
  let weight_tuples = map (\ n -> (weight_of_tree n, n)) (t_nodes tree)
  case (find_unique_tup weight_tuples) of
      Just tup -> do
        -- unbalanced sub tree, so balance by adjusting the sub tree
        -- if the sub tree is ok, adjust our own weight
        let other = find_other_weight weight_tuples (fst tup)
        find_changed_weight (snd tup) (fst other)

      Nothing -> do
        let total_weight = weight_of_tree tree
        let diff = total_weight - target_weight
        Just ((t_weight tree) - diff)
  where
    find_unique_tup :: [(Int, TreeNode)] -> (Maybe (Int, TreeNode))
    find_unique_tup tuples = do
      let weights = map fst tuples
      let has_unique_weight tup = True == (fromMaybe False (isUnique (fst tup) weights))
      find has_unique_weight tuples
    find_other_weight :: [(Int, TreeNode)] -> Int -> (Int, TreeNode)
    find_other_weight tuples w = do
      case (find (\ t -> w /= (fst t)) tuples) of
          Just t  -> t
          Nothing -> error "unexpected"
