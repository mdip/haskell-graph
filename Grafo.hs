{-
   *******************************************
   *                                         *
   *        Graph data type structure        *
   *  		                                 *
   * main function: main                     *
   * or: inputGraph <graph>                  *
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

-- TIPI --------------------------

type Vertex = Int
type Element = String
type Edge = (Vertex, Vertex)
type Node = (Vertex, Element)
type Graph = ([Node],[Edge])


-- Funzioni per leggere i singoli elementi di un Node
vertex :: Node -> Vertex
vertex n = fst n

element :: Node -> Element
element n = snd n


-- FUNZIONI DI SUPPORTO ------------------------------

--safetail [] = []
--safetail (_:xs) = xs

-- Primo node della list di nodes di un graph
firstNode :: Graph -> Node
firstNode   g = head (fst g)

-- List dei nodes di un graph successivi al first
nextNodes :: Graph -> [Node]
nextNodes ([],_) = []
nextNodes  g = tail (fst g)

-- List di tutti i nodes di un graph
getNodes :: Graph -> [Node]
getNodes  ([],_) = []
getNodes   g = fst g

-- Primo arco della list di archi di un graph
firstEdge :: Graph -> Edge
firstEdge g = head (snd g)

-- List degli archi di un graph successivi al first
nextEdges :: Graph -> [Edge]
nextEdges (_,[]) = []
nextEdges g = tail (snd g)

-- List di tutti gli archi di un graph
getEdges :: Graph -> [Edge]
getEdges (_,[]) = []
getEdges  g = snd g

-- Trova un node nel graph partendo dal suo vertex
findNode :: Vertex -> Graph -> [Node]
findNode _ ([],_) = []
findNode v g 
   | v == vertex (firstNode g) = [firstNode g]
   | otherwise = findNode v (nextNodes g, [])

-- List di tutti i vertices di un graph
vertices :: Graph -> [Vertex]
vertices g = map (vertex) (getNodes g)

-- Converte una list di vertices in una list di nodes
verticesToNodes :: [Vertex] -> Graph -> [Node]
verticesToNodes [] _ = []
verticesToNodes (v:vs) g = findNode v g ++ verticesToNodes vs g

-- Converte una list di nodes in una list di vertices
nodesToVertices :: [Node] -> [Vertex]
nodesToVertices [] = []
nodesToVertices (v:vs) = (vertex v) : nodesToVertices vs

-- List dei vertices adjacents ad un node con vertex v
listAdjacents :: Vertex -> [Edge] -> [Vertex]
listAdjacents v [] = []
listAdjacents v (x:xs)
   | (fst x)==v = snd x : listAdjacents v xs
   | otherwise = listAdjacents v xs

-- List degli archi in cui il vertex di partenza e' v
listEdgesAdjacency :: Vertex -> [Edge] -> [Edge]
listEdgesAdjacency v [] = []
listEdgesAdjacency v (x:xs)
   | (fst x)==v  = x : listEdgesAdjacency v xs
   | otherwise = listEdgesAdjacency v xs


--purgeList [] = []
--purgeList (x:xs) = x : purgeList (filter (\y -> not(x==y)) xs)

purgeListNodes [] = []
purgeListNodes (x:xs) = x : purgeListNodes (filter (\y -> not(vertex x== vertex y)) xs)

purgeListEdges [] = []
purgeListEdges (x:xs) = x : purgeListEdges (filter (\y -> not(x==y)) xs)



-- OPERATORI GRAFO ----------------------------

-- Crea un graph vuoto
createGraph :: Graph
createGraph = ([],[])


-- Verifica che il graph abbia almeno un node
emptyGraph :: Graph -> Bool
emptyGraph ([],_) = True
emptyGraph g  = False


-- Verifica l'existsnza di un node
existsNode :: Vertex -> Graph -> Bool
existsNode _ ([], _) = False
existsNode v g = any (\x -> x == v) (vertices g)


-- Verifica l'existsnza di un arco dal vertex u al vertex v
existsEdge :: Edge -> Graph -> Bool
existsEdge (u,v) g = any (\(x,y) -> x==u && y==v) (getEdges g)


-- Inserisce un node nel graph se non exists e se il vertex e' un intero positivo
addNode :: Vertex -> Graph -> Graph
addNode v g
   | v<0 || existsNode v g = g
   | otherwise = ((getNodes g)++[(v,[])], getEdges g)


-- Inserisce un arco nel graph solo se entrambi i nodes esistono e se l'arco non e' gia' stato aggiunto
addEdge :: Edge -> Graph -> Graph
addEdge (_,_) ([],_) = ([],[])
addEdge (u,v) g 
   | (existsNode u g) && (existsNode v g) && (not(existsEdge (u,v) g)) = (getNodes g, (getEdges g)++[(u,v)])
   | otherwise = g


-- Cancella un node solo se non ci sono archi uscenti o entranti
deleteNode :: Vertex -> Graph -> Graph
deleteNode v g
   | (existsNode v g) && not (any (\(x,y) -> x==v || y==v) (getEdges g)) = (deleteN v (getNodes g), getEdges g)
   | otherwise = g
   where deleteN u xs = if (vertex (head xs))==u 
                         then tail xs
                      else head xs : deleteN u (tail xs)


-- Cancella un arco solo se e' presente
deleteEdge :: Edge -> Graph -> Graph
deleteEdge (_,_) ([],_) = ([],[])
deleteEdge (u,v) g
   | existsNode u g && existsNode v g && existsEdge(u,v) g = (getNodes g, deleteA u v (getEdges g))
   | otherwise = g
   where deleteA z k xs = if ((fst (head xs)==z) && (snd (head xs)==k))
                           then tail xs
                        else head xs : deleteA z k (tail xs)


-- List dei nodes adjacents ad un vertex v di un node
adjacents :: Vertex -> Graph -> [Node]
adjacents _ ([],_) = []
adjacents _ (_,[]) = []
adjacents v g
   | existsNode v g = verticesToNodes (listAdjacents v (getEdges g)) g
   | otherwise = []


-- Scrive il contenuto di un node con vertex v sovrascrivendo l'attributo di tipo Element
putContentNode :: Vertex -> Element -> Graph -> Graph
putContentNode _ _ ([],_) = ([],[])
putContentNode v e g
   | (existsNode v g) = (putContentN v e (getNodes g), getEdges g)
   | otherwise = g
   where putContentN u k xs = if (vertex (head xs))==u 
                             then (u,k) : tail xs
                          else head xs : putContentN u k (tail xs)


-- Legge il valore di tipo Element presente in un node con vertex v
readNode :: Vertex -> Graph -> Element
readNode _ ([],_) = []
readNode v g 
   | existsNode v g = element (head(findNode v g))
   | otherwise = []



-- VISITA DI UN GRAFO ----------------------------

-- Depth First Search 
depthFirstSearch :: Vertex -> Graph -> [Node]
depthFirstSearch v g
   | existsNode v g = if (null(adjacents v g)) then findNode v g else dfs [v] (findNode v g) g
   | otherwise = []


-- Visita DFS ricorsiva
-- input: list di vertices visitati, pila di nodes per accumulare quelli da visitare, graph da visitare
dfs :: [Vertex]-> [Node] -> Graph -> [Node]
dfs _ [] _ = []
dfs vs (n:ns) g = n : dfs (vs++[vertex n]) (inStackNodes (vertex n) (vs++[vertex n]) (adjacents (vertex n) g) ns) g

-- Crea una pila di nodes partendo da una list di nodes gia' accumulati (xs), 
-- una list di nodes nuovi (vs cioe' gli adjacents a v), la list dei nodes gia' visitati (ns) ed 
-- il vertex del node appena letto dalla pila al fine di evitarne il reinserimento.
inStackNodes :: Vertex -> [Vertex] -> [Node] -> [Node] -> [Node]
inStackNodes v ns [] xs = xs
inStackNodes v ns vs xs = (filter (\x-> (not(any (\y->x==y || v==(vertex x)) xs))) (filter (\z-> not(elem (vertex z) ns)) vs))++xs



-- Breadth First Search
breadthFirstSearch :: Vertex -> Graph -> [Node]
breadthFirstSearch v g
	| existsNode v g = if (null(adjacents v g)) then findNode v g else bfs [v] (findNode v g) g 
	| otherwise = []


-- Visita BFS ricorsiva
-- input: coda di nodes per accumulare quelli da visitare, graph da visitare
bfs :: [Vertex]-> [Node] -> Graph -> [Node]
bfs _ [] _ = []
bfs vs (n:ns) g = n : bfs (vs++[vertex n]) (inQueueNodes (vertex n) (vs++[vertex n]) (adjacents (vertex n) g) ns) g


-- Crea una coda di nodes partendo da una list di nodes gia' accumulati (xs), 
-- una list di nodes nuovi (vs cioe' gli adjacents a v), la list dei nodes gia' visitati (ns) ed
-- il vertex del node appena letto dalla coda al fine di evitarne il reinserimento.
inQueueNodes :: Vertex -> [Vertex] -> [Node] -> [Node] -> [Node]
inQueueNodes v ns [] xs = xs
inQueueNodes v ns vs xs = xs ++ (filter (\x-> (not(any (\y->x==y || v==(vertex x)) xs))) (filter (\z-> not(elem (vertex z) ns)) vs))



-- FUNZIONI AGGIUNTIVE --------------------------

-- Traspone il graph invertendo il verso degli archi
transposed :: Graph -> Graph
transposed g = (getNodes g, [(x,y) | (y,x)<-(getEdges g)])

-- Verifica che non vi sia nessun ciclo nel graph
acyclic :: Graph -> Bool
acyclic g
   | emptyGraph g = False
   | otherwise = not(any (\x-> existsCammino x x g) (vertices g))

-- Verifica che un node u possa raggiungere un node v direttamente o indirettamente tramite le adiacenze
-- Ogni cammino considerato ha lunghezza >= 1. Ogni node ha un cammino di lunghezza zero per se stesso.
existsCammino :: Vertex -> Vertex -> Graph -> Bool
existsCammino _ _ ([],_) = False
existsCammino _ _ ([_],_)= False
existsCammino u v g
   | (existsNode u g) && (existsNode v g) = path v [u] (findNode u g) g
   | otherwise = False

{-
  Funzione path per determinare l'existsnza di un
  cammino tra due nodes.
  input: vertex arrivo, coda di nodes, graph da visitare
  e' una visita BFS con verifica di existsnza dell'arco per ogni coppia di nodes in sequenza
  fino ad arrivare al node di interesse. 
  Verifica che il node di arrivo sia gia' stato inserito nella coda. 
  Se il node "u" e' presente in coda mi fermo.
  Il vertex del node di arrivo "u" e' un parametro fisso per ogni chiamata della funzione path
-}

path :: Vertex -> [Vertex] -> [Node] -> Graph -> Bool
path _ _ [] _ = False
path u vs (n:ns) g
	| (not(existsEdge ((vertex n),u) g)) && (not(any(\x->vertex x==u)(ns))) = go
	| otherwise = True
	where go = path u (vs++[vertex n]) (inQueueNodes (vertex n) (vs++[vertex n]) (adjacents (vertex n) g) ns) g
                                           

-- Verifica che il graph sia fortemente connesso, cioe' che per ogni coppia di nodes esista un cammino
highlyConnectedGraph :: Graph -> Bool
highlyConnectedGraph ([], _) = False
highlyConnectedGraph ([n], _) = False
highlyConnectedGraph g = hConn 0 g

-- Verifica l'existsnza dei cammini tra le coppie di nodes, per ogni node del graph
-- si utilizza un contatore per ripetere la funzione su ogni node del graph
hConn :: Int -> Graph -> Bool
hConn c g
   | (c < (length (vertices g))) = if existsConnection (vertex(firstNode g)) (getNodes g)  g 
                                     then hConn (c+1) (nextNodes g ++ [firstNode g] ,getEdges g) 
                                  else False
   | otherwise = True


-- Confronta un node con tutti gli altri per verificare che esista un cammino per ogni node confrontato
existsConnection :: Vertex -> [Node] -> Graph -> Bool
existsConnection _ [] _ = True 
existsConnection v (n:ns) g
   | existsCammino v (vertex n) g  = existsConnection v ns g
   | otherwise = False



-- STAMPA GRAFO ----------------------------

printGraph :: [Vertex] -> Graph -> IO ()
printGraph [] _  = return ()
printGraph (v:vs) g  = do putStr (show v)
                           putStr "\t"
                           putStr (show (readNode v g))
                           putStr "\t"
                           putStr (show (nodesToVertices (adjacents v g)))
                           putStr "\n"
                           printGraph vs g



-- MENU' GESTIONE ----------------------------

main = manageGraph createGraph

inputGraph :: Graph -> IO ()
inputGraph g = manageGraph (purgeListNodes (getNodes g), purgeListEdges (getEdges g))

manageGraph :: Graph -> IO ()
manageGraph g = do putStrLn "\n\n*** Menu' Graph ***"
                     putStrLn "\n1. Inserisci node\n2. Inserisci arco\n3. Cancella node\n4. Cancella arco\n5. Scrivi contenuto node\n6. Adjacents node\n7. Depth first search\n8. Breadth first search\n9. Graph transposed\n10. Verifica existsnza cammino\n11. Print graph\n0. Esci"
                     --putStr "\n\n*** Graph corrente ***\n"
                     --printGraph (vertices g) g
                     hFlush stdout
                     putStr "\n\nQuale operazione vuoi eseguire? (indica il numero) : "
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



gAddNode g = do putStr "Inserisci vertex: "
                      v <- getLine
                      putStr "Inserisci il contenuto (premi invio per lasciare vuoto): "
                      contenuto <- getLine 
                      if v==[]
                         then manageGraph g
                      else manageGraph (putContentNode (read(v)::Int) contenuto (addNode (read(v)::Int) g))
                               

gAddEdge g = do putStr "Inserisci vertex node Partenza: "
                      v <- getLine
                      putStr "Inserisci vertex node Arrivo: "
                      u <- getLine
                      if (v==[] || u==[])
                         then manageGraph g 
                      else manageGraph (addEdge ((read(v)::Int), (read(u)::Int)) g)


gDeleteNode g = do putStr "Inserisci vertex: "
                     v <- getLine
                     if v==[] 
                        then manageGraph g 
                     else manageGraph (deleteNode (read(v)::Int) g)


gDeleteEdge g = do putStr "Inserisci vertex node Partenza: "
                     v <- getLine
                     putStr "Inserisci vertex node Arrivo: "
                     u <- getLine
                     if (v==[] || u==[])
                        then manageGraph g 
                     else manageGraph (deleteEdge ((read(v)::Int), (read(u)::Int)) g)


gWriteNode g = do putStr "Inserisci vertex: "
                   v <- getLine
                   putStr "Inserisci il contenuto (premi invio per lasciare vuoto): "
                   contenuto <- getLine 
                   if v==[] 
                      then manageGraph g 
                   else manageGraph (putContentNode (read(v)::Int) contenuto  g)


gAdjacentsNode g = do putStr "Inserisci vertex: "
                      v <- getLine
                      if v==[] 
                         then manageGraph g 
                      else do putStr "List di adiacenza per il node con vertex "
                              putStrLn (show v)
                              putStr (show(adjacents (read(v)::Int) g))
                              putStr "\n\nPremere invio per continuare..."
                              c<-getChar
                              manageGraph g


gDfs g = do putStr "Depth First Search\n"
            if not (emptyGraph g) 
               then do putStr "Da quale vertex vuoi partire: "
                       v <- getLine
                       if v==[] 
                          then manageGraph g 
                       else do putStr "\n\n*** DFS ***\n"
                               putStr (show (nodesToVertices(depthFirstSearch ((read v)::Int) g)))
                               putStr "\n\nPremere invio per continuare..."
                               c<-getChar
                               manageGraph g
            else do putStr "\nImpossibile visitare il graph perche' e' vuoto!"
                    manageGraph g


gBfs g = do putStr "Breadth First Search\n"
            if not (emptyGraph g) 
               then do putStr "Da quale vertex vuoi partire: "
                       v <- getLine
                       if v==[] 
                          then manageGraph g 
                       else do putStr "\n\n*** BFS ***\n"
                               putStr (show (nodesToVertices(breadthFirstSearch ((read v)::Int) g)))
                               putStr "\n\nPremere invio per continuare..."
                               c<-getChar
                               manageGraph g
            else do putStr "\nImpossibile visitare il graph perche' e' vuoto!"
                    manageGraph g




gTransposed g = do manageGraph (transposed g)


gExistsPath g = do putStr "Inserisci vertex node Partenza: "
                      v <- getLine
                      putStr "Inserisci vertex node Arrivo: "
                      u <- getLine
                      if (v==[] || u==[]) 
                         then manageGraph g 
                      else do if (existsCammino (read(v)::Int) (read(u)::Int) g) 
                                 then putStrLn "\nEsiste almeno un cammino" 
                              else putStrLn "\nNon exists nessun cammino"
                              putStr "\n\nPremere invio per continuare..."
                              c<-getChar
                              manageGraph g



gPrintGraph g = do printGraph (vertices g) g
                    if highlyConnectedGraph g 
                       then putStr "\n\nIl graph e' fortemente connesso"
                    else putStr "\n\nIl graph e' debolmente connesso"
                    if acyclic g 
                       then putStr "\n\nIl graph e' acyclic\n"
                    else putStr "\n\nIl graph non e' acyclic\n"
                    manageGraph g


