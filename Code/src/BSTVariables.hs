module BSTVariables where

import Data
import Expr

-- | Stores a Binary Search Tree Node.
data Node = VarNode {  name  :: Name,
                       value :: Value,
                       left  :: Node,
                       right :: Node   }
          | FuncNode { name  :: Name,
                       argn  :: [Name],
                       expr  :: Expr,
                       left  :: Node,
                       right :: Node   }
          | Nil
  deriving (Show, Eq)

-- | Inserts node into a tree given the root & returns the new root
insertNode :: Node -> Node -> Node
insertNode root new_node
  = case (compare (name new_node) (name root)) of --comparing the names of the variables
      EQ -> case new_node of
              (VarNode n v l r) -> root { value = v }
              (FuncNode n a e l r) -> root { argn = a, expr = e }
      LT -> if left root == Nil
              then root {left = new_node }
              else root {left = (insertNode (left root) new_node)}
      GT -> if right root == Nil
              then root {right = new_node}
              else root {right = (insertNode (right root) new_node)}

-- | Returns the Node with the given name.
getNode :: Node -> Name -> Either String Node
getNode root name'
  = if root == Nil
      then Left $ "\'" ++ name' ++ "\' not found."
      else case (compare name' (name root)) of
             EQ -> Right (root)
             GT -> getNode (right root) name'
             LT -> getNode (left root) name'

-- | Given the root of the tree, removes the node with the given name.
removeNode :: Node -> Name -> Node
removeNode root name'
  = if root == Nil
      then root
      else case (compare name' (name root)) of
             GT -> root { right = removeNode (right root) name' }
             LT -> root { left = removeNode (left root) name' }
             EQ -> do if right root == Nil
                        then left root
                        else if left root == Nil
                               then right root
                               else do let newroot = right root
                                       let newroot_leftBranch = left newroot
                                       newroot {left = insertNode root newroot_leftBranch }
