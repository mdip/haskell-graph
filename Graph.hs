{-

Date: 2011
Author: Marco Di Pietro

The following code is licensed under the GPL V3 license
(https://www.gnu.org/licenses/gpl-3.0.txt).

   *******************************************
   *                                         *
   *        Graph data type structure        *
   *  		                             *
   * main function: main                     *
   * or: inputGraph <graph>                  *
   *                                         *
   *******************************************

Graph: pair of lists composed by the nodes list and the edges list
Node: vertex-element pair
Edge: vertex-vertex pair
Vertex: Int value >=0

The element inside each node is a string.

You can modify the type modifying "type Element = ...". 
The data type in a node must be a list ([Int], String or [Char] etc...)
If you modify the type, please modify also the stdin functions

usage example: 
> main 
or
> inputGraph ([(0,"test1"),(4,"test2"),(2,""),(6,"")],[(4,0),(2,6),(0,6),(2,2),(6,4),(4,2)])

-}


import System.IO

-- TYPES --------------------------

type Vertex = Int
type Element = String
type Edge = (Vertex, Vertex)
type Node = (Vertex, Element)
type Graph = ([Node], [Edge])


-- Read a single element given a node
vertex :: Node -> Vertex
vertex n = fst n

element :: Node -> Element
element n = snd n


-- Support functions ------------------------------

--safetail [] = []
--safetail (_:xs) = xs

-- First node of the node list that represents the graph
firstNode :: Graph -> Node
firstNode   g = head (fst g)

-- Tail of the list of nodes that represents the graph
nextNodes :: Graph -> [Node]
nextNodes ([], _) = []
nextNodes  g = tail (fst g)

-- List of all nodes of the graph
getNodes :: Graph -> [Node]
getNodes  ([], _) = []
getNodes   g = fst g

-- First edge of the node list that represents the graph
firstEdge :: Graph -> Edge
firstEdge g = head (snd g)

-- Tail of the list of edges that represents the graph
nextEdges :: Graph -> [Edge]
nextEdges (_, []) = []
nextEdges g = tail (snd g)

-- List of all edges of the graph
getEdges :: Graph -> [Edge]
getEdges (_, []) = []
getEdges  g = snd g

-- Find a node from its vertex id
findNode :: Vertex -> Graph -> [Node]
findNode _ ([], _) = []
findNode v g 
   | v == vertex (firstNode g) = [firstNode g]
   | otherwise = findNode v (nextNodes g, [])

-- List of all vertices of the graph
vertices :: Graph -> [Vertex]
vertices g = map (vertex) (getNodes g)

-- List of vertices from a list of nodes
verticesToNodes :: [Vertex] -> Graph -> [Node]
verticesToNodes [] _ = []
verticesToNodes (v : vs) g = findNode v g ++ verticesToNodes vs g

-- List of nodes from a list of vertices
nodesToVertices :: [Node] -> [Vertex]
nodesToVertices [] = []
nodesToVertices (v : vs) = (vertex v) : nodesToVertices vs

-- List of adjacent vertices of a vertex v
listAdjacents :: Vertex -> [Edge] -> [Vertex]
listAdjacents v [] = []
listAdjacents v (x : xs)
   | (fst x) == v = snd x : listAdjacents v xs
   | otherwise = listAdjacents v xs

-- List of edges where the starting vertex is a given vertex v
listEdgesAdjacency :: Vertex -> [Edge] -> [Edge]
listEdgesAdjacency v [] = []
listEdgesAdjacency v (x : xs)
   | (fst x) == v  = x : listEdgesAdjacency v xs
   | otherwise = listEdgesAdjacency v xs


--purgeList [] = []
--purgeList (x:xs) = x : purgeList (filter (\y -> not(x==y)) xs)

purgeListNodes [] = []
purgeListNodes (x : xs) = x : purgeListNodes (filter (\y -> not(vertex x == vertex y)) xs)

purgeListEdges [] = []
purgeListEdges (x : xs) = x : purgeListEdges (filter (\y -> not(x == y)) xs)



-- GRAPH OPERATORS ----------------------------

-- Crete an empty graph
createGraph :: Graph
createGraph = ([], [])


-- Check if the graph has at least one node
emptyGraph :: Graph -> Bool
emptyGraph ([], _) = True
emptyGraph g  = False


-- Check if a node exists
existsNode :: Vertex -> Graph -> Bool
existsNode _ ([], _) = False
existsNode v g = any (\x -> x == v) (vertices g)


-- Check if an edge from node u to node v exists
existsEdge :: Edge -> Graph -> Bool
existsEdge (u, v) g = any (\(x, y) -> x == u && y == v) (getEdges g)


-- Add a un node to the graph if it isn't already in the set of vertices
-- and if the vertex has a positive integer index
addNode :: Vertex -> Graph -> Graph
addNode v g
   | v<0 || existsNode v g = g
   | otherwise = ((getNodes g) ++ [(v, [])], getEdges g)


-- Add a new edge to the graph only if both nodes exist
-- and if the edge already doesn't exist
addEdge :: Edge -> Graph -> Graph
addEdge (_, _) ([], _) = ([], [])
addEdge (u, v) g 
   | (existsNode u g) && (existsNode v g) && (not(existsEdge (u, v) g)) = (getNodes g, (getEdges g)++[(u, v)])
   | otherwise = g


-- Delete a node only if it isn't connected to any node
deleteNode :: Vertex -> Graph -> Graph
deleteNode v g
   | (existsNode v g) && not (any (\(x, y) -> x == v || y == v) (getEdges g)) = (deleteN v (getNodes g), getEdges g)
   | otherwise = g
   where deleteN u xs = if (vertex (head xs)) == u 
                         then tail xs
                      else head xs : deleteN u (tail xs)


-- Delete an edge if it exists
deleteEdge :: Edge -> Graph -> Graph
deleteEdge (_, _) ([], _) = ([], [])
deleteEdge (u,v) g
   | existsNode u g && existsNode v g && existsEdge(u, v) g = (getNodes g, deleteA u v (getEdges g))
   | otherwise = g
   where deleteA z k xs = if ((fst (head xs) == z) && (snd (head xs) == k))
                           then tail xs
                        else head xs : deleteA z k (tail xs)


-- List of adjacent nodes of a given vertex
adjacents :: Vertex -> Graph -> [Node]
adjacents _ ([], _) = []
adjacents _ (_, []) = []
adjacents v g
   | existsNode v g = verticesToNodes (listAdjacents v (getEdges g)) g
   | otherwise = []


-- Update the data contained in a node of vertex v
putContentNode :: Vertex -> Element -> Graph -> Graph
putContentNode _ _ ([], _) = ([], [])
putContentNode v e g
   | (existsNode v g) = (putContentN v e (getNodes g), getEdges g)
   | otherwise = g
   where putContentN u k xs = if (vertex (head xs)) == u 
                              then (u, k) : tail xs
                              else head xs : putContentN u k (tail xs)


-- Read the data inside a node of vertex v
readNode :: Vertex -> Graph -> Element
readNode _ ([], _) = []
readNode v g 
   | existsNode v g = element (head(findNode v g))
   | otherwise = []



-- Graph search ----------------------------

-- Depth First Search 
depthFirstSearch :: Vertex -> Graph -> [Node]
depthFirstSearch v g
   | existsNode v g = if (null(adjacents v g)) then findNode v g else dfs [v] (findNode v g) g
   | otherwise = []


-- Recursive DFS
-- input: list of visited vertices, stack of nodes to visit, graph to visit
dfs :: [Vertex]-> [Node] -> Graph -> [Node]
dfs _ [] _ = []
dfs vs (n : ns) g = n : dfs (vs ++ [vertex n]) (inStackNodes (vertex n) (vs ++ [vertex n]) (adjacents (vertex n) g) ns) g

-- Create a stack of nodes starting from a list of nodes already saved (xs), 
-- a list of new nodes (vs, the adjacent nodes of v), the list of already visited nodes (ns) 
-- and the current vertex readed from the stack to avoid to push it again
inStackNodes :: Vertex -> [Vertex] -> [Node] -> [Node] -> [Node]
inStackNodes v ns [] xs = xs
inStackNodes v ns vs xs = (filter (\x-> (not(any (\y -> x == y || v == (vertex x)) xs))) (filter (\z -> not(elem (vertex z) ns)) vs)) ++ xs


-- Breadth First Search
breadthFirstSearch :: Vertex -> Graph -> [Node]
breadthFirstSearch v g
	| existsNode v g = if (null(adjacents v g)) then findNode v g else bfs [v] (findNode v g) g 
	| otherwise = []


-- Recursive BFS
-- input: queue of nodes to save those to visit, graph to visit
bfs :: [Vertex]-> [Node] -> Graph -> [Node]
bfs _ [] _ = []
bfs vs (n : ns) g = n : bfs (vs ++ [vertex n]) (inQueueNodes (vertex n) (vs ++ [vertex n]) (adjacents (vertex n) g) ns) g


-- Create a queue of nodes starting from a list of nodes already saved (xs), 
-- a list of new nodes (vs, the adjacent nodes of v), the list of already visited nodes (ns) 
-- and the current vertex readed from the queue to avoid to add it again
inQueueNodes :: Vertex -> [Vertex] -> [Node] -> [Node] -> [Node]
inQueueNodes v ns [] xs = xs
inQueueNodes v ns vs xs = xs ++ (filter (\x -> (not (any (\y->x==y || v==(vertex x)) xs))) (filter (\z-> not (elem (vertex z) ns)) vs))



-- Other functions --------------------------

-- Transpose the graph inverting its edges direction
transposed :: Graph -> Graph
transposed g = (getNodes g, [(x, y) | (y, x) <- (getEdges g)])

-- Verify the absence of cycles in the graph
acyclic :: Graph -> Bool
acyclic g
   | emptyGraph g = False
   | otherwise = not (any (\x-> existsPath x x g) (vertices g))

-- Verify that a node u can be reach a node v directly or indirectly using adjacencies
-- Every path has a length >= 1. We avoid here self-loops.
existsPath :: Vertex -> Vertex -> Graph -> Bool
existsPath _ _ ([], _) = False
existsPath _ _ ([_], _)= False
existsPath u v g
   | (existsNode u g) && (existsNode v g) = path v [u] (findNode u g) g
   | otherwise = False

{-
  Check the existence of a path between two nodes.
  input: target vertex, queue of nodes, graph to visit
  It's a BFS. 
  Verify that the target has already been added to the queue. 
  If the node "u" is inside the queue the computation stops.
  The node "u" is a fixed parameter for every call.
-}

path :: Vertex -> [Vertex] -> [Node] -> Graph -> Bool
path _ _ [] _ = False
path u vs (n : ns) g
	| (not(existsEdge ((vertex n), u) g)) && (not (any (\x -> vertex x == u)(ns))) = go
	| otherwise = True
	where go = path u (vs ++ [vertex n]) (inQueueNodes (vertex n) (vs ++ [vertex n]) (adjacents (vertex n) g) ns) g
                                           

-- Check if the graph is strongly connected.
highlyConnectedGraph :: Graph -> Bool
highlyConnectedGraph ([], _) = False
highlyConnectedGraph ([n], _) = False
highlyConnectedGraph g = hConn 0 g

-- Recursive function to check the connections between each pair of nodes
-- using a counter to call again the function for each node of the graph
hConn :: Int -> Graph -> Bool
hConn c g
   | (c < (length (vertices g))) = if existsConnection (vertex(firstNode g)) (getNodes g)  g 
                                     then hConn (c + 1) (nextNodes g ++ [firstNode g], getEdges g) 
                                  else False
   | otherwise = True


-- Check if a path exists from a node v to each other node of the graph
existsConnection :: Vertex -> [Node] -> Graph -> Bool
existsConnection _ [] _ = True 
existsConnection v (n : ns) g
   | existsPath v (vertex n) g  = existsConnection v ns g
   | otherwise = False



-- Print utility ----------------------------

printGraph :: [Vertex] -> Graph -> IO ()
printGraph [] _  = return ()
printGraph (v : vs) g  = do putStr (show v)
                           putStr "\t"
                           putStr (show (readNode v g))
                           putStr "\t"
                           putStr (show (nodesToVertices (adjacents v g)))
                           putStr "\n"
                           printGraph vs g



-- Menu ----------------------------

main = manageGraph createGraph

inputGraph :: Graph -> IO ()
inputGraph g = manageGraph (purgeListNodes (getNodes g), purgeListEdges (getEdges g))

manageGraph :: Graph -> IO ()
manageGraph g = do putStrLn "\n\n*** Menu' Graph ***"
                     putStrLn "\n1. Add node\n2. Add edge\n3. Delete node\n4. Delete edge\n5. Write data into a node\n6. Adjacent nodes\n7. Depth first search\n8. Breadth first search\n9. Transposed graph\n10. Check if a path exists\n11. Print graph\n0. Exit"
                     --putStr "\n\n*** Graph corrente ***\n"
                     --printGraph (vertices g) g
                     hFlush stdout
                     putStr "\n\nWhat do you want to do? (insert the number and press ENTER) : "
                     scelta <- getLine
                     let s = read (scelta)::Int
                     case () of _
                                 | s==1  -> gAddNode g
                                 | s==2  -> gAddEdge g
                                 | s==3  -> gDeleteNode  g
                                 | s==4  -> gDeleteEdge  g
                                 | s==5  -> gWriteNode    g
                                 | s==6  -> gAdjacentsNode g
                                 | s==7  -> gDfs g
                                 | s==8  -> gBfs g
                                 | s==9  -> gTransposed g
                                 | s==10 -> gExistsPath g
                                 | s==11 -> gPrintGraph g
                                 | s==0  -> putStr "\n\nExit. Bye!\n"
                                 | otherwise -> manageGraph g



gAddNode g = do putStr "Add node: "
                      v <- getLine
                      putStr "Write data for this new node (press ENTER to leave it empty): "
                      contenuto <- getLine 
                      if v==[]
                         then manageGraph g
                      else manageGraph (putContentNode (read(v)::Int) contenuto (addNode (read(v)::Int) g))
                               

gAddEdge g = do putStr "Start node: "
                      v <- getLine
                      putStr "Goal node: "
                      u <- getLine
                      if (v == [] || u == [])
                         then manageGraph g 
                      else manageGraph (addEdge ((read(v)::Int), (read(u)::Int)) g)


gDeleteNode g = do putStr "Node to delete: "
                     v <- getLine
                     if v == [] 
                        then manageGraph g 
                     else manageGraph (deleteNode (read(v)::Int) g)


gDeleteEdge g = do putStr "Start node: "
                     v <- getLine
                     putStr "Goal node: "
                     u <- getLine
                     if (v == [] || u == [])
                        then manageGraph g 
                     else manageGraph (deleteEdge ((read(v)::Int), (read(u)::Int)) g)


gWriteNode g = do putStr "Node to update: "
                   v <- getLine
                   putStr "Write data (press ENTER to leave empty): "
                   contenuto <- getLine 
                   if v == [] 
                      then manageGraph g 
                   else manageGraph (putContentNode (read(v)::Int) contenuto  g)


gAdjacentsNode g = do putStr "Node to examine: "
                      v <- getLine
                      if v == [] 
                         then manageGraph g 
                      else do putStr "Adjacence list: "
                              putStrLn (show v)
                              putStr (show(adjacents (read(v)::Int) g))
                              putStr "\n\nPress ENTER to continue..."
                              c<-getChar
                              manageGraph g


gDfs g = do putStr "Depth First Search\n"
            if not (emptyGraph g) 
               then do putStr "Start node: "
                       v <- getLine
                       if v == [] 
                          then manageGraph g 
                       else do putStr "\n\n*** DFS ***\n"
                               putStr (show (nodesToVertices(depthFirstSearch ((read v)::Int) g)))
                               putStr "\n\nPress ENTER to continue..."
                               c<-getChar
                               manageGraph g
            else do putStr "\nImpossible to visit the graph because it is empty!"
                    manageGraph g


gBfs g = do putStr "Breadth First Search\n"
            if not (emptyGraph g) 
               then do putStr "Start node: "
                       v <- getLine
                       if v == [] 
                          then manageGraph g 
                       else do putStr "\n\n*** BFS ***\n"
                               putStr (show (nodesToVertices(breadthFirstSearch ((read v)::Int) g)))
                               putStr "\n\nPress ENTER to continue..."
                               c<-getChar
                               manageGraph g
            else do putStr "\nImpossible to visit the graph because it is empty!"
                    manageGraph g




gTransposed g = do manageGraph (transposed g)


gExistsPath g = do putStr "Start node: "
                      v <- getLine
                      putStr "Goal node: "
                      u <- getLine
                      if (v == [] || u == []) 
                         then manageGraph g 
                      else do if (existsPath (read(v)::Int) (read(u)::Int) g) 
                                 then putStrLn "\nThere exists at least one path" 
                              else putStrLn "\nThere aren't any path"
                              putStr "\n\nPress ENTER to continue..."
                              c<-getChar
                              manageGraph g



gPrintGraph g = do printGraph (vertices g) g
                    if highlyConnectedGraph g 
                       then putStr "\n\nThe graph is strongly connected"
                    else putStr "\n\nThe graph has weak connections"
                    if acyclic g 
                       then putStr "\n\nThe graph is acyclic\n"
                    else putStr "\n\nThe graph is not acyclic\n"
                    manageGraph g


