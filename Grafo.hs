{-
   *******************************************
   *                                         *
   *             Struttura Grafo             *
   *  funzione iniziale: main                *
   *                                         *
   * oppure: inputGrafo <grafo>              *
   * per passare un grafo come argomento     *
   *******************************************

Grafo: coppia di liste, la prima di nodi, la seconda di archi
Nodo: coppia vertice-elemento
Arco: coppia vertice-vertice
Vertice: valore di tipo Int >=0

L'elemento contenuto in un nodo e' una stringa.

Il tipo di dato contenuto nel nodo si puo' modificare agendo su "type Elemento = ...". 
Il tipo di dato contenuto nel nodo deve essere in una lista ([Int], String o [Char] ecc...)
Se viene modificato il tipo e' necessario modificare le funzioni di acquisizione da stdin

esempio utilizzo: 
> main 
oppure
> inputGrafo ([(0,"test"),(4,"prova"),(2,""),(6,"")],[(4,0),(2,6),(0,6),(2,2),(6,4),(4,2)])

-}


import System.IO

-- TIPI --------------------------

type Vertice = Int
type Elemento = String
type Arco = (Vertice, Vertice)
type Nodo = (Vertice, Elemento)
type Grafo = ([Nodo],[Arco])


-- Funzioni per leggere i singoli elementi di un Nodo
vertice :: Nodo -> Vertice
vertice n = fst n

elemento :: Nodo -> Elemento
elemento n = snd n


-- FUNZIONI DI SUPPORTO ------------------------------

--safetail [] = []
--safetail (_:xs) = xs

-- Primo nodo della lista di nodi di un grafo
primoNodo :: Grafo -> Nodo
primoNodo   g = head (fst g)

-- Lista dei nodi di un grafo successivi al primo
succNodi :: Grafo -> [Nodo]
succNodi ([],_) = []
succNodi  g = tail (fst g)

-- Lista di tutti i nodi di un grafo
getNodi :: Grafo -> [Nodo]
getNodi  ([],_) = []
getNodi   g = fst g

-- Primo arco della lista di archi di un grafo
primoArco :: Grafo -> Arco
primoArco g = head (snd g)

-- Lista degli archi di un grafo successivi al primo
succArchi :: Grafo -> [Arco]
succArchi (_,[]) = []
succArchi g = tail (snd g)

-- Lista di tutti gli archi di un grafo
getArchi :: Grafo -> [Arco]
getArchi (_,[]) = []
getArchi  g = snd g

-- Trova un nodo nel grafo partendo dal suo vertice
trovaNodo :: Vertice -> Grafo -> [Nodo]
trovaNodo _ ([],_) = []
trovaNodo v g 
   | v == vertice (primoNodo g) = [primoNodo g]
   | otherwise = trovaNodo v (succNodi g, [])

-- Lista di tutti i vertici di un grafo
vertici :: Grafo -> [Vertice]
vertici g = map (vertice) (getNodi g)

-- Converte una lista di vertici in una lista di nodi
verticiToNodi :: [Vertice] -> Grafo -> [Nodo]
verticiToNodi [] _ = []
verticiToNodi (v:vs) g = trovaNodo v g ++ verticiToNodi vs g

-- Converte una lista di nodi in una lista di vertici
nodiToVertici :: [Nodo] -> [Vertice]
nodiToVertici [] = []
nodiToVertici (v:vs) = (vertice v) : nodiToVertici vs

-- Lista dei vertici adiacenti ad un nodo con vertice v
listaAdiacenti :: Vertice -> [Arco] -> [Vertice]
listaAdiacenti v [] = []
listaAdiacenti v (x:xs)
   | (fst x)==v = snd x : listaAdiacenti v xs
   | otherwise = listaAdiacenti v xs

-- Lista degli archi in cui il vertice di partenza e' v
listaArchiAdiacenza :: Vertice -> [Arco] -> [Arco]
listaArchiAdiacenza v [] = []
listaArchiAdiacenza v (x:xs)
   | (fst x)==v  = x : listaArchiAdiacenza v xs
   | otherwise = listaArchiAdiacenza v xs


--epuraLista [] = []
--epuraLista (x:xs) = x : epuraLista (filter (\y -> not(x==y)) xs)

epuraListaNodi [] = []
epuraListaNodi (x:xs) = x : epuraListaNodi (filter (\y -> not(vertice x== vertice y)) xs)

epuraListaArchi [] = []
epuraListaArchi (x:xs) = x : epuraListaArchi (filter (\y -> not(x==y)) xs)



-- OPERATORI GRAFO ----------------------------

-- Crea un grafo vuoto
creaGrafo :: Grafo
creaGrafo = ([],[])


-- Verifica che il grafo abbia almeno un nodo
grafoVuoto :: Grafo -> Bool
grafoVuoto ([],_) = True
grafoVuoto g  = False


-- Verifica l'esistenza di un nodo
esisteNodo :: Vertice -> Grafo -> Bool
esisteNodo _ ([], _) = False
esisteNodo v g = any (\x -> x == v) (vertici g)


-- Verifica l'esistenza di un arco dal vertice u al vertice v
esisteArco :: Arco -> Grafo -> Bool
esisteArco (u,v) g = any (\(x,y) -> x==u && y==v) (getArchi g)


-- Inserisce un nodo nel grafo se non esiste e se il vertice e' un intero positivo
insNodo :: Vertice -> Grafo -> Grafo
insNodo v g
   | v<0 || esisteNodo v g = g
   | otherwise = ((getNodi g)++[(v,[])], getArchi g)


-- Inserisce un arco nel grafo solo se entrambi i nodi esistono e se l'arco non e' gia' stato aggiunto
insArco :: Arco -> Grafo -> Grafo
insArco (_,_) ([],_) = ([],[])
insArco (u,v) g 
   | (esisteNodo u g) && (esisteNodo v g) && (not(esisteArco (u,v) g)) = (getNodi g, (getArchi g)++[(u,v)])
   | otherwise = g


-- Cancella un nodo solo se non ci sono archi uscenti o entranti
cancNodo :: Vertice -> Grafo -> Grafo
cancNodo v g
   | (esisteNodo v g) && not (any (\(x,y) -> x==v || y==v) (getArchi g)) = (cancN v (getNodi g), getArchi g)
   | otherwise = g
   where cancN u xs = if (vertice (head xs))==u 
                         then tail xs
                      else head xs : cancN u (tail xs)


-- Cancella un arco solo se e' presente
cancArco :: Arco -> Grafo -> Grafo
cancArco (_,_) ([],_) = ([],[])
cancArco (u,v) g
   | esisteNodo u g && esisteNodo v g && esisteArco(u,v) g = (getNodi g, cancA u v (getArchi g))
   | otherwise = g
   where cancA z k xs = if ((fst (head xs)==z) && (snd (head xs)==k))
                           then tail xs
                        else head xs : cancA z k (tail xs)


-- Lista dei nodi adiacenti ad un vertice v di un nodo
adiacenti :: Vertice -> Grafo -> [Nodo]
adiacenti _ ([],_) = []
adiacenti _ (_,[]) = []
adiacenti v g
   | esisteNodo v g = verticiToNodi (listaAdiacenti v (getArchi g)) g
   | otherwise = []


-- Scrive il contenuto di un nodo con vertice v sovrascrivendo l'attributo di tipo Elemento
scriviNodo :: Vertice -> Elemento -> Grafo -> Grafo
scriviNodo _ _ ([],_) = ([],[])
scriviNodo v e g
   | (esisteNodo v g) = (scriviN v e (getNodi g), getArchi g)
   | otherwise = g
   where scriviN u k xs = if (vertice (head xs))==u 
                             then (u,k) : tail xs
                          else head xs : scriviN u k (tail xs)


-- Legge il valore di tipo Elemento presente in un nodo con vertice v
leggiNodo :: Vertice -> Grafo -> Elemento
leggiNodo _ ([],_) = []
leggiNodo v g 
   | esisteNodo v g = elemento (head(trovaNodo v g))
   | otherwise = []



-- VISITA DI UN GRAFO ----------------------------

-- Depth First Search 
depthFirstSearch :: Vertice -> Grafo -> [Nodo]
depthFirstSearch v g
   | esisteNodo v g = if (null(adiacenti v g)) then trovaNodo v g else dfs [v] (trovaNodo v g) g
   | otherwise = []


-- Visita DFS ricorsiva
-- input: lista di vertici visitati, pila di nodi per accumulare quelli da visitare, grafo da visitare
dfs :: [Vertice]-> [Nodo] -> Grafo -> [Nodo]
dfs _ [] _ = []
dfs vs (n:ns) g = n : dfs (vs++[vertice n]) (inPilaNodi (vertice n) (vs++[vertice n]) (adiacenti (vertice n) g) ns) g

-- Crea una pila di nodi partendo da una lista di nodi gia' accumulati (xs), 
-- una lista di nodi nuovi (vs cioe' gli adiacenti a v), la lista dei nodi gia' visitati (ns) ed 
-- il vertice del nodo appena letto dalla pila al fine di evitarne il reinserimento.
inPilaNodi :: Vertice -> [Vertice] -> [Nodo] -> [Nodo] -> [Nodo]
inPilaNodi v ns [] xs = xs
inPilaNodi v ns vs xs = (filter (\x-> (not(any (\y->x==y || v==(vertice x)) xs))) (filter (\z-> not(elem (vertice z) ns)) vs))++xs



-- Breadth First Search
breadthFirstSearch :: Vertice -> Grafo -> [Nodo]
breadthFirstSearch v g
	| esisteNodo v g = if (null(adiacenti v g)) then trovaNodo v g else bfs [v] (trovaNodo v g) g 
	| otherwise = []


-- Visita BFS ricorsiva
-- input: coda di nodi per accumulare quelli da visitare, grafo da visitare
bfs :: [Vertice]-> [Nodo] -> Grafo -> [Nodo]
bfs _ [] _ = []
bfs vs (n:ns) g = n : bfs (vs++[vertice n]) (inCodaNodi (vertice n) (vs++[vertice n]) (adiacenti (vertice n) g) ns) g


-- Crea una coda di nodi partendo da una lista di nodi gia' accumulati (xs), 
-- una lista di nodi nuovi (vs cioe' gli adiacenti a v), la lista dei nodi gia' visitati (ns) ed
-- il vertice del nodo appena letto dalla coda al fine di evitarne il reinserimento.
inCodaNodi :: Vertice -> [Vertice] -> [Nodo] -> [Nodo] -> [Nodo]
inCodaNodi v ns [] xs = xs
inCodaNodi v ns vs xs = xs ++ (filter (\x-> (not(any (\y->x==y || v==(vertice x)) xs))) (filter (\z-> not(elem (vertice z) ns)) vs))



-- FUNZIONI AGGIUNTIVE --------------------------

-- Traspone il grafo invertendo il verso degli archi
trasposto :: Grafo -> Grafo
trasposto g = (getNodi g, [(x,y) | (y,x)<-(getArchi g)])

-- Verifica che non vi sia nessun ciclo nel grafo
aciclico :: Grafo -> Bool
aciclico g
   | grafoVuoto g = False
   | otherwise = not(any (\x-> esisteCammino x x g) (vertici g))

-- Verifica che un nodo u possa raggiungere un nodo v direttamente o indirettamente tramite le adiacenze
-- Ogni cammino considerato ha lunghezza >= 1. Ogni nodo ha un cammino di lunghezza zero per se stesso.
esisteCammino :: Vertice -> Vertice -> Grafo -> Bool
esisteCammino _ _ ([],_) = False
esisteCammino _ _ ([_],_)= False
esisteCammino u v g
   | (esisteNodo u g) && (esisteNodo v g) = path v [u] (trovaNodo u g) g
   | otherwise = False

{-
  Funzione path per determinare l'esistenza di un
  cammino tra due nodi.
  input: vertice arrivo, coda di nodi, grafo da visitare
  e' una visita BFS con verifica di esistenza dell'arco per ogni coppia di nodi in sequenza
  fino ad arrivare al nodo di interesse. 
  Verifica che il nodo di arrivo sia gia' stato inserito nella coda. 
  Se il nodo "u" e' presente in coda mi fermo.
  Il vertice del nodo di arrivo "u" e' un parametro fisso per ogni chiamata della funzione path
-}

path :: Vertice -> [Vertice] -> [Nodo] -> Grafo -> Bool
path _ _ [] _ = False
path u vs (n:ns) g
	| (not(esisteArco ((vertice n),u) g)) && (not(any(\x->vertice x==u)(ns))) = prosegui
	| otherwise = True
	where prosegui = path u (vs++[vertice n]) (inCodaNodi (vertice n) (vs++[vertice n]) (adiacenti (vertice n) g) ns) g
                                           

-- Verifica che il grafo sia fortemente connesso, cioe' che per ogni coppia di nodi esista un cammino
fortementeConnesso :: Grafo -> Bool
fortementeConnesso ([], _) = False
fortementeConnesso ([n], _) = False
fortementeConnesso g = fconn 0 g

-- Verifica l'esistenza dei cammini tra le coppie di nodi, per ogni nodo del grafo
-- si utilizza un contatore per ripetere la funzione su ogni nodo del grafo
fconn :: Int -> Grafo -> Bool
fconn c g
   | (c < (length (vertici g))) = if esisteConnessione (vertice(primoNodo g)) (getNodi g)  g 
                                     then fconn (c+1) (succNodi g ++ [primoNodo g] ,getArchi g) 
                                  else False
   | otherwise = True


-- Confronta un nodo con tutti gli altri per verificare che esista un cammino per ogni nodo confrontato
esisteConnessione :: Vertice -> [Nodo] -> Grafo -> Bool
esisteConnessione _ [] _ = True 
esisteConnessione v (n:ns) g
   | esisteCammino v (vertice n) g  = esisteConnessione v ns g
   | otherwise = False



-- STAMPA GRAFO ----------------------------

stampaGrafo :: [Vertice] -> Grafo -> IO ()
stampaGrafo [] _  = return ()
stampaGrafo (v:vs) g  = do putStr (show v)
                           putStr "\t"
                           putStr (show (leggiNodo v g))
                           putStr "\t"
                           putStr (show (nodiToVertici (adiacenti v g)))
                           putStr "\n"
                           stampaGrafo vs g



-- MENU' GESTIONE ----------------------------

main = gestioneGrafo creaGrafo

inputGrafo :: Grafo -> IO ()
inputGrafo g = gestioneGrafo (epuraListaNodi (getNodi g), epuraListaArchi (getArchi g))

gestioneGrafo :: Grafo -> IO ()
gestioneGrafo g = do putStrLn "\n\n*** Menu' Grafo ***"
                     putStrLn "\n1. Inserisci nodo\n2. Inserisci arco\n3. Cancella nodo\n4. Cancella arco\n5. Scrivi contenuto nodo\n6. Adiacenti nodo\n7. Depth first search\n8. Breadth first search\n9. Grafo trasposto\n10. Verifica esistenza cammino\n11. Stampa grafo\n0. Esci"
                     --putStr "\n\n*** Grafo corrente ***\n"
                     --stampaGrafo (vertici g) g
                     hFlush stdout
                     putStr "\n\nQuale operazione vuoi eseguire? (indica il numero) : "
                     scelta <- getLine
                     let s = read (scelta)::Int
                     case () of _
                                 | s==1  -> gInserisciNodo g
                                 | s==2  -> gInserisciArco g
                                 | s==3  -> gCancellaNodo  g
                                 | s==4  -> gCancellaArco  g
                                 | s==5  -> gScriviNodo    g
                                 | s==6  -> gAdiacentiNodo g
                                 | s==7  -> gDfs g
                                 | s==8  -> gBfs g
                                 | s==9  -> gTrasposto g
                                 | s==10 -> gEsisteCammino g
                                 | s==11 -> gStampaGrafo g
                                 | s==0  -> putStr "\n\nEsecuzione terminata\n"
                                 | otherwise -> gestioneGrafo g



gInserisciNodo g = do putStr "Inserisci vertice: "
                      v <- getLine
                      putStr "Inserisci il contenuto (premi invio per lasciare vuoto): "
                      contenuto <- getLine 
                      if v==[]
                         then gestioneGrafo g
                      else gestioneGrafo (scriviNodo (read(v)::Int) contenuto (insNodo (read(v)::Int) g))
                               

gInserisciArco g = do putStr "Inserisci vertice nodo Partenza: "
                      v <- getLine
                      putStr "Inserisci vertice nodo Arrivo: "
                      u <- getLine
                      if (v==[] || u==[])
                         then gestioneGrafo g 
                      else gestioneGrafo (insArco ((read(v)::Int), (read(u)::Int)) g)


gCancellaNodo g = do putStr "Inserisci vertice: "
                     v <- getLine
                     if v==[] 
                        then gestioneGrafo g 
                     else gestioneGrafo (cancNodo (read(v)::Int) g)


gCancellaArco g = do putStr "Inserisci vertice nodo Partenza: "
                     v <- getLine
                     putStr "Inserisci vertice nodo Arrivo: "
                     u <- getLine
                     if (v==[] || u==[])
                        then gestioneGrafo g 
                     else gestioneGrafo (cancArco ((read(v)::Int), (read(u)::Int)) g)


gScriviNodo g = do putStr "Inserisci vertice: "
                   v <- getLine
                   putStr "Inserisci il contenuto (premi invio per lasciare vuoto): "
                   contenuto <- getLine 
                   if v==[] 
                      then gestioneGrafo g 
                   else gestioneGrafo (scriviNodo (read(v)::Int) contenuto  g)


gAdiacentiNodo g = do putStr "Inserisci vertice: "
                      v <- getLine
                      if v==[] 
                         then gestioneGrafo g 
                      else do putStr "Lista di adiacenza per il nodo con vertice "
                              putStrLn (show v)
                              putStr (show(adiacenti (read(v)::Int) g))
                              putStr "\n\nPremere invio per continuare..."
                              c<-getChar
                              gestioneGrafo g


gDfs g = do putStr "Depth First Search\n"
            if not (grafoVuoto g) 
               then do putStr "Da quale vertice vuoi partire: "
                       v <- getLine
                       if v==[] 
                          then gestioneGrafo g 
                       else do putStr "\n\n*** DFS ***\n"
                               putStr (show (nodiToVertici(depthFirstSearch ((read v)::Int) g)))
                               putStr "\n\nPremere invio per continuare..."
                               c<-getChar
                               gestioneGrafo g
            else do putStr "\nImpossibile visitare il grafo perche' e' vuoto!"
                    gestioneGrafo g


gBfs g = do putStr "Breadth First Search\n"
            if not (grafoVuoto g) 
               then do putStr "Da quale vertice vuoi partire: "
                       v <- getLine
                       if v==[] 
                          then gestioneGrafo g 
                       else do putStr "\n\n*** BFS ***\n"
                               putStr (show (nodiToVertici(breadthFirstSearch ((read v)::Int) g)))
                               putStr "\n\nPremere invio per continuare..."
                               c<-getChar
                               gestioneGrafo g
            else do putStr "\nImpossibile visitare il grafo perche' e' vuoto!"
                    gestioneGrafo g




gTrasposto g = do gestioneGrafo (trasposto g)


gEsisteCammino g = do putStr "Inserisci vertice nodo Partenza: "
                      v <- getLine
                      putStr "Inserisci vertice nodo Arrivo: "
                      u <- getLine
                      if (v==[] || u==[]) 
                         then gestioneGrafo g 
                      else do if (esisteCammino (read(v)::Int) (read(u)::Int) g) 
                                 then putStrLn "\nEsiste almeno un cammino" 
                              else putStrLn "\nNon esiste nessun cammino"
                              putStr "\n\nPremere invio per continuare..."
                              c<-getChar
                              gestioneGrafo g



gStampaGrafo g = do stampaGrafo (vertici g) g
                    if fortementeConnesso g 
                       then putStr "\n\nIl grafo e' fortemente connesso"
                    else putStr "\n\nIl grafo e' debolmente connesso"
                    if aciclico g 
                       then putStr "\n\nIl grafo e' aciclico\n"
                    else putStr "\n\nIl grafo non e' aciclico\n"
                    gestioneGrafo g


