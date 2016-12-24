{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- | A simple and slow directed graph implementation using only lists and pairs.

module Graph
( Graph (..)
, vertex
, element
, node
, nodes
, edges
, vertices
, verticesToNodes
, nodesToVertices
, empty
, isEmpty
, existsNode
, existsEdge
, existsSelfLoop
, addNode
, addEdge
, fromList
, fromNodeList
, fromNodeList1
, fromEdgeList
, deleteNode
, deleteEdge
, adjacents
, putNodeElement
, getNodeElement
, depthFirstSearch
, breadthFirstSearch
, acyclic
, existsPath
  ) where

import Data.List as List

type Vertex = Int
type Element = String
type Edge = (Vertex, Vertex)
type Node = (Vertex, Element)
type Graph = ([Node], [Edge])

instance {-# OVERLAPS #-} Show Graph where
    show g = "\n Graph:" ++ "\n"
              ++ "\tVertices: " ++ "{" ++ List.intercalate "," [show v | v <- vertices g] ++ "}" ++ "\n"
              ++ "\tEdges: " ++ "{" ++ List.intercalate "," [show e | e <- edges g] ++ "}" ++ "\n"
              ++ "\tElements: " ++ "{" ++ List.intercalate "," [show (element n) | n <- nodes g] ++ "}\n"

instance {-# OVERLAPS #-} Show Node where
    show n = show (vertex n)

-- Get the vertex of a given node
vertex :: Node -> Vertex
vertex n = fst n

-- Get the element of a given node
element :: Node -> Element
element n = snd n

-- All nodes of the graph
nodes :: Graph -> [Node]
nodes g = fst g

-- All edges of the graph
edges :: Graph -> [Edge]
edges  g = snd g

-- All vertices of the graph
vertices :: Graph -> [Vertex]
vertices g = map vertex (nodes g)

-- Get a node given its vertex id
node :: Vertex -> Graph -> Node
node v g 
  | v == vertex (head (nodes g)) = head (nodes g)
  | otherwise = node v (tail (nodes g), edges g)

-- List of nodes from a list of vertices
verticesToNodes :: [Vertex] -> Graph -> [Node]
verticesToNodes [] _ = []
verticesToNodes (v : vs) g = node v g : verticesToNodes vs g

-- List of vertices from a list of nodes
nodesToVertices :: [Node] -> [Vertex]
nodesToVertices [] = []
nodesToVertices (v : vs) = vertex v : nodesToVertices vs

-- Create an empty graph
empty :: Graph
empty = ([], [])

-- Check if the graph has at least one node
isEmpty :: Graph -> Bool
isEmpty ([], _) = True
isEmpty g = False

-- Check if a node exists
existsNode :: Vertex -> Graph -> Bool
existsNode _ ([], _) = False
existsNode v g = any (\x -> x == v) (vertices g)

-- Check if an edge between two nodes exists
existsEdge :: Edge -> Graph -> Bool
existsEdge (u, v) g = any (\(x, y) -> x == u  && y == v) (edges g)

-- Check if there is at least one self loop inside the graph
existsSelfLoop :: Edge -> Graph -> Bool
existsSelfLoop (u, v) g = any (\(x, y) -> x == y) (edges g)

-- Add a node to the graph if its vertex doesn't exist
addNode :: Graph -> Node -> Graph
addNode g n
  | existsNode (vertex n) g = g
  | otherwise = (n:(nodes g), edges g)

-- Add a new edge to the graph only if both nodes exist and if the edge already doesn't exist
addEdge :: Graph -> Edge -> Graph
addEdge g (u, v) 
  | (existsNode u g) && (existsNode v g) && (not(existsEdge (u, v) g)) = (nodes g, (u, v):(edges g))
  | otherwise = g

-- Create a new graph from a list of vertices
fromList :: [Vertex] -> Graph
fromList [] = empty
fromList v = addNode (fromList (tail v)) (head v, "")  

-- Create a new graph from a list of nodes
fromNodeList :: [Node] -> Graph
fromNodeList = foldl addNode empty

-- Add a new list of nodes to an existing graph
fromNodeList1 :: Graph -> [Node] -> Graph
fromNodeList1 g = foldl addNode g

-- Add a new list of edges to an existing graph
fromEdgeList :: Graph -> [Edge] -> Graph
fromEdgeList g = foldl addEdge g

-- Delete a node only if it is not connected to any node
deleteNode :: Vertex -> Graph -> Graph
deleteNode v g
  | (existsNode v g) && not (any (\(x, y) -> x == v || y == v) (edges g)) = (deleteN v (nodes g), edges g)
  | otherwise = g
  where deleteN u xs = if (vertex (head xs)) == u then tail xs else head xs : deleteN u (tail xs)

-- Delete an edge if exists
deleteEdge :: Edge -> Graph -> Graph
deleteEdge (u, v) g
  | existsNode u g && existsNode v g && existsEdge(u, v) g = (nodes g, deleteA u v (edges g))
  | otherwise = g
  where deleteA z k xs = if ((fst (head xs) == z) && (snd (head xs) == k)) then tail xs else head xs : deleteA z k (tail xs)

-- List of adjacent nodes of a given vertex
adjacents :: Vertex -> Graph -> [Node]
adjacents v g
  | existsNode v g = verticesToNodes (adjacentNodes v (edges g)) g
  | otherwise = []
  where adjacentNodes v [] = []
        adjacentNodes v (x : xs)
          | fst x == v = snd x : adjacentNodes v xs
          | otherwise = adjacentNodes v xs

-- Update the element contained in a node
putNodeElement :: Vertex -> Element -> Graph -> Graph
putNodeElement v e g
  | existsNode v g = (putContent v e (nodes g), edges g)
  | otherwise = g
  where putContent u k nodes = if (vertex (head nodes)) == u then (u, k) : tail nodes else head nodes : putContent u k (tail nodes)

-- Read the element contained in a node
getNodeElement :: Vertex -> Graph -> Maybe Element
getNodeElement v g 
  | existsNode v g = Just (element (node v g))
  | otherwise = Nothing

-- Depth First Search 
depthFirstSearch :: Vertex -> Graph -> [Node]
depthFirstSearch v g
  | existsNode v g = if (null (adjacents v g)) then [node v g] else dfs [v] [node v g] g
  | otherwise = []
    where dfs _ [] _ = []
          dfs vs (n:ns) g = n : dfs (vs ++ [vertex n]) (inStackNodes (vertex n) (vs ++ [vertex n]) (adjacents (vertex n) g) ns) g
            where inStackNodes v ns [] xs = xs
                  inStackNodes v ns vs xs = (filter (\x-> (not(any (\y -> x == y || v == (vertex x)) xs))) (filter (\z -> not(elem (vertex z) ns)) vs)) ++ xs

-- Breadth First Search
breadthFirstSearch :: Vertex -> Graph -> [Node]
breadthFirstSearch v g
  | existsNode v g = if (null(adjacents v g)) then [node v g] else bfs [v] [node v g] g
  | otherwise = []
    where bfs _ [] _ = []
          bfs vs (n:ns) g = n : bfs (vs ++ [vertex n]) (inQueueNodes (vertex n) (vs ++ [vertex n]) (adjacents (vertex n) g) ns) g
            where inQueueNodes v ns [] xs = xs
                  inQueueNodes v ns vs xs = xs ++ (filter (\x -> (not (any (\y->x == y || v == (vertex x)) xs))) (filter (\z-> not (elem (vertex z) ns)) vs))

-- Verify the absence of cycles inside the graph
acyclic :: Graph -> Bool
acyclic g
  | isEmpty g = True
  | otherwise = not (any (\x-> existsPath x x g) (vertices g))

-- Verify that a path between two nodes exists. Self-loop are not considered
existsPath :: Vertex -> Vertex -> Graph -> Bool
existsPath _ _ (_, []) = False
existsPath u v g
  | (existsNode u g) && (existsNode v g) = path v [u] [(node u g)] g
  | otherwise = False
  where path _ _ [] _ = False
        path u vs (n : ns) g
          | (not (existsEdge ((vertex n), u) g)) && (not (any (\x -> vertex x == u)(ns))) = go
          | otherwise = True
          where go = path u (vs ++ [vertex n]) (inQueueNodes (vertex n) (vs ++ [vertex n]) (adjacents (vertex n) g) ns) g
                inQueueNodes v ns [] xs = xs
                inQueueNodes v ns vs xs = xs ++ (filter (\x -> (not (any (\y->x == y || v == (vertex x)) xs))) (filter (\z-> not (elem (vertex z) ns)) vs))
