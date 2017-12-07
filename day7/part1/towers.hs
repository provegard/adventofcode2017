module Towers where
import qualified Data.Map as Map
import Data.Maybe
import Data.List

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

parse_line :: String -> InputLine
parse_line line = do
  let parts = words line
  -- first: name
  -- second: weight in parentheses
  -- third (opt): arrow
  -- rest: sub-towers with trailing comma
  parse' parts
  where
    parse' (n:w:_:rest) = InputLine n (to_int w) (map strip_trailing_comma rest)
    parse' (n:w:_)      = InputLine n (to_int w) []
    parse' x            = error "malformed input"

line_by_name line_map name = fromMaybe (error ("unknown line: " ++ name)) $ Map.lookup name line_map
node_by_name node_map name = fromMaybe (error ("unknown node: " ++ name)) $ Map.lookup name node_map


get_root :: (Map.Map String TreeNode) -> TreeNode
get_root node_map = do
  let all_names = Map.keys node_map
  let child_names = foldl (\ names node -> names ++ (map t_name (t_nodes node))) [] node_map
  let result = all_names \\ child_names
  node_by_name node_map (head result)

to_node_map :: [InputLine] -> (Map.Map String InputLine) -> (Map.Map String TreeNode) -> (Map.Map String TreeNode)

insert_node :: InputLine -> (Map.Map String InputLine) -> (Map.Map String TreeNode) -> (Map.Map String TreeNode)
insert_node line line_map node_map = do
  case Map.lookup (name line) node_map of
      Just n  -> node_map
      Nothing -> do
        let child_lines = map (\ l -> line_by_name line_map l) (children line)
        let new_map = to_node_map child_lines line_map node_map
        let child_nodes = map (\ l -> node_by_name new_map l) (children line)
        Map.insert (name line) (TreeNode (name line) (weight line) child_nodes) new_map

to_node_map [] line_map node_map = node_map
to_node_map lines line_map node_map = do
  let new_node_map = insert_node (head lines) line_map node_map
  to_node_map (tail lines) line_map new_node_map

make_tree :: [InputLine] -> TreeNode
make_tree lines = do
  let by_name = Map.fromList $ map name_line lines
  let node_map = to_node_map lines by_name Map.empty
  get_root node_map
  where
    name_line line = (name line, line)
