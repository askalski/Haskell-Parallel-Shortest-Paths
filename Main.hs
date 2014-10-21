module Main where

import System.IO
import Debug.Trace
import Data.Array
import Data.Ix
import Control.Exception
import Control.Parallel.Strategies 
import Control.DeepSeq
import Control.Concurrent

data SubGraphStruct = SubGraph (Int, Int) (Array (Int, Int) Int) 
        --deriving (Show)
        
data GraphStruct = Graph Int Int (Array (Int, Int) SubGraphStruct) 
        --deriving (Show)

maxX :: Array (Int, Int) Int -> Int
maxX a = fst $ snd $ bounds a
maxY :: Array (Int, Int) Int -> Int
maxY a = snd $ snd $ bounds a
minX :: Array (Int, Int) Int -> Int
minX a = fst $ fst $ bounds a
minY :: Array (Int, Int) Int -> Int
minY a = snd $ fst $ bounds a
        
instance Show SubGraphStruct where
        show sg =                    
                let
                        SubGraph (_, _) a = sg
                        vallist :: Int -> [String]
                        vallist x = [show ((!) a (x, y)) | y <- [(minY a)..(maxY a)]]
                        myLine :: Int -> [Char]                        
                        myLine x = (foldl (\l val -> l ++ val ++ " ") "" (vallist x) ) ++ "\n"                
                in
                        foldl (\l val -> l ++ val) "" [myLine x | x<-[(minX a)..(maxX a)]]
                
--data GraphStruct = Graph Int Int (Array (Int, Int) SubGraphStruct)        
instance Show GraphStruct where
        show g = 
                let
                        Graph n k a = g
                        sSize = if (rem n k) > 0 then (quot n k) + 1 else (quot n k)        
                        fallsInto = \x -> (quot (x-1) sSize) + 1       
                        getVal :: (Int, Int) -> Int
                        getVal (x,y) =
                                let
                                        cell = (!) a (fallsInto x, fallsInto y)
                                        SubGraph (si, sj) cellA = cell
                                in
                                        (!) cellA (x,y)
                                                                
                        vallist :: Int -> [String]
                        vallist x = [show (getVal (x, y)) | y <- [1..n]]
                        myLine :: Int -> [Char]                        
                        myLine x = (foldl (\l val -> l ++ val ++ " ") "" (vallist x) ) ++ "\n"                
                in
                        foldl (\l val -> l ++ val) "" [myLine x | x<-[1..n]]
        
        

        
position :: SubGraphStruct -> (Int, Int)
position (SubGraph i _) = i

getArray :: SubGraphStruct -> (Array (Int, Int) Int)
getArray (SubGraph _ a) = a    

-- position in master graph, (starting indices, end indices (INCLUSIVE!)), and elements array.
getSubGraph :: (Int,Int) -> ((Int, Int), (Int, Int)) -> [((Int,Int),Int)] -> SubGraphStruct
getSubGraph (i,j) ((sx, sy), (ex, ey)) elems =
        SubGraph (i,j) (array ((sx, sy), (ex, ey)) (filter boundaries elems))
        --SubGraph (i,j) (array ((sx, sy), (ex, ey)) (elems))
        where
        boundaries :: ((Int, Int), Int) -> Bool
        boundaries ((x,y),_) = ((x >= sx) && (x <= ex) && (y >= sy) && (y <= ey))

subGraph :: GraphStruct -> (Int, Int) -> SubGraphStruct
subGraph (Graph n k a) (i,j) = (!) a (i,j)    

floydWarshall :: GraphStruct -> GraphStruct
floydWarshall g = 
        let
         Graph n k a = g
        in
         foldl singleIterationRelax g [1,2..k]


instance NFData SubGraphStruct

singleIterationRelax :: GraphStruct -> Int -> GraphStruct
singleIterationRelax g i =
        let 
         Graph n k a = g
         getRelaxedSubGraph :: (Int, Int) -> SubGraphStruct -> Array Int SubGraphStruct -> Array Int SubGraphStruct -> SubGraphStruct --trace("\nrelaxing " ++ show (ti, tj))         
         getRelaxedSubGraph (ti, tj) relaxedPivot relaxedPivotRow relaxedPivotCol | (ti == i && tj == i) = relaxedPivot 
         getRelaxedSubGraph (ti, tj) relaxedPivot relaxedPivotRow relaxedPivotCol | (ti == i && tj /= i) = (!) relaxedPivotRow tj
         getRelaxedSubGraph (ti, tj) relaxedPivot relaxedPivotRow relaxedPivotCol | (ti /= i && tj == i) = (!) relaxedPivotCol ti
         getRelaxedSubGraph (ti, tj) relaxedPivot relaxedPivotRow relaxedPivotCol | (ti /= i && tj /= i) = relaxSubGraphGeneral ((!) relaxedPivotRow tj) ((!) relaxedPivotCol ti) (subGraph g (ti, tj))         
        in
        do
         --here are some definitions as helper:
         --relaxSubGraphGeneral :: pivot-row cell -> pivot-col cell -> target cell -> relaxed target cell 
         --relaxedPivot :: SubGraphStruct       
         --relaxedPivotRow :: Array Int SubGraphStruct
         --relaxedPivotCol :: Array Int SubGraphStruct
         --relaxedGraph :: Array (Int, Int) SubGraphStruct         
         let relaxedPivot = relaxSubGraphPivot (subGraph g (i,i))                  
         --let relaxedPivotRowList = [(col, relaxSubGraphGeneral (subGraph g (i, col)) relaxedPivot (subGraph g (i, col)) )| col <- [1,2..(i-1)]++[(i+1),(i+2)..k]]
         let relaxedPivotRowList = parMap rdeepseq (\col -> (col, relaxSubGraphGeneral (subGraph g (i, col)) relaxedPivot (subGraph g (i, col)) )) ([1,2..(i-1)]++[(i+1),(i+2)..k])
         let relaxedPivotRow = array (1,k) relaxedPivotRowList
         let relaxedPivotColList = parMap rdeepseq (\row -> (row, relaxSubGraphGeneral relaxedPivot (subGraph g (row, i)) (subGraph g (row, i)) )) ([1,2..(i-1)]++[(i+1),(i+2)..k])                     
         let relaxedPivotCol = array (1,k) relaxedPivotColList
         --let relaxedGraph = array ((1,1), (k,k)) [((ti, tj), getRelaxedSubGraph (ti, tj) relaxedPivot relaxedPivotRow relaxedPivotCol) | ti <- [1,2..k], tj <- [1,2..k]]
         let relaxedGraphList = parMap rdeepseq (\indexPair -> (indexPair, getRelaxedSubGraph indexPair relaxedPivot relaxedPivotRow relaxedPivotCol)) [(ti,tj) | ti <- [1,2..k], tj <- [1,2..k]]
         let relaxedGraph = array ((1,1), (k,k)) relaxedGraphList
         Graph n k relaxedGraph
        
relaxSubGraphPivot :: SubGraphStruct -> SubGraphStruct
relaxSubGraphPivot (SubGraph (pi,pj) matrix) =
       --trace("\ncalling relaxSubGraphPivot with matrix = " ++ show matrix) $
        assert (pi == pj) $
        assert ((minX matrix) == (minY matrix)) $
        assert ((maxX matrix) == (maxY matrix)) $
       let
        k_low = minX matrix
        k_hi  = maxX matrix
        i_low = minX matrix
        i_hi  = maxX matrix
        j_low = minX matrix        
        j_hi  = maxX matrix
        indices = [(k, j, i) | k<-[k_low..k_hi], j<-[j_low..j_hi], i<-[i_low..i_hi]]
       in
        --trace("\nindices = " ++ show indices)
        SubGraph (pi,pj) (foldl (\pivot -> crossRelax pivot pivot pivot) matrix indices) -- actuall Floyd-Warshall algorithm         
        
--the final cell-relaxation method. Takes 3 arguments:
--1) pivot-row cell
--2) pivot-col cell
--3) target cell
-- and returns target cell relaxed accordingly
--relaxSubGraphGeneral :: pivot-row cell -> pivot-col cell -> target cell -> relaxed target cell
relaxSubGraphGeneral :: SubGraphStruct -> SubGraphStruct -> SubGraphStruct -> SubGraphStruct
relaxSubGraphGeneral (SubGraph (ri, rj) rowCell) (SubGraph (ci, cj) colCell) (SubGraph (ti, tj) targetCell) =
       --trace("\ncalling relaxSubGraphGeneral with matrix = " ++ show targetCell)
        assert (ri == cj) $     --do pivot row and col correspond? 
        assert (rj == tj) $     --do row of target and pivot-row-cell correspond?
        assert (ci == ti) $     --do col of target and pivot-col-cell correspond?
        assert ((minY colCell) == (minX rowCell)) $ 
        assert ((maxY colCell) == (maxX rowCell)) $ -- is pivot diagonal and square?
       let       
        --here's some funny stuff (can explain only with drawings, but the last assert in this method is very informant)
        i_low = minX targetCell
        i_hi  = maxX targetCell
        j_low = minY targetCell     
        j_hi  = maxY targetCell
        k_low = minX rowCell
        k_hi  = maxX rowCell        
        indices = [(k, j, i) | k<-[k_low..k_hi], j<-[j_low..j_hi], i<-[i_low..i_hi]]
       in
        --trace("\nindices = " ++ show indices)
        SubGraph (ti,tj) (foldl (crossRelax rowCell colCell) targetCell indices) -- actuall Floyd-Warshall algorithm

--takes 4 args! first is rowCell, second is colCell, third is targetCell, fourth is relaxation index, result is relaxed targetCell                             
crossRelax :: Array (Int, Int) Int -> Array (Int, Int) Int -> Array (Int, Int) Int -> (Int, Int, Int) -> Array (Int, Int) Int
crossRelax rowCell colCell matrix (k, j, i) =
                assert(elem (i,k) (indices colCell)) $
                assert(elem (k,j) (indices rowCell)) $
                assert(elem (i,j) (indices matrix )) $
               let
                ik = (!) colCell (i,k)
                kj = (!) rowCell (k,j)   
                ij = (!) matrix  (i,j)
               in
                if (ik + kj < ij) then
                        (//) matrix [((i,j), ik+kj)]
                        else matrix
        
graphSize :: GraphStruct -> Int
graphSize (Graph n k a) = n

getGraph :: Int -> Int -> [SubGraphStruct] -> GraphStruct
getGraph n k sgs = Graph n k (array ((1,1),(k,k)) (map (\x -> (position x, x)) sgs))        

-- takes two args - size of graph, and number of sub-matrices. 
readGraphFromIO :: Int -> Int -> IO GraphStruct
readGraphFromIO n k = do 
        graphData <- newreadGraphIn n
        let sSize = if (rem n k) > 0 then (quot n k) + 1 else (quot n k)
        --putStrLn ("sSize=" ++ (show sSize))
        let beginOf = (\i -> (i-1)*sSize + 1)
        let endOf = (\i -> min (i*sSize) n)
        let indices = [((i,j), ((beginOf i, beginOf j),(endOf i, endOf j))) | i <- [1..k], j<-[1..k] ]
        --putStrLn ("indices =" ++ (show indices))
        let subGraphs = fmap (\indexPair -> getSubGraph (fst indexPair) (snd indexPair) graphData) indices
        return $ getGraph n k subGraphs  
        
newreadGraphIn :: Int -> IO [((Int,Int),Int)]
newreadGraphIn n = newreadGraphInLoop n 1   
newreadGraphInLoop n lineIndex = if (lineIndex > n) then (return []) else        
       do
        line      <- readGraphLine lineIndex
        nextLines <- newreadGraphInLoop n (lineIndex+1)
        return $ line ++ nextLines
       where
        readGraphLine :: Int -> IO [((Int, Int), Int)]
        readGraphLine lineIndex = do
                x <- getLine
                let parsedLine = [(lineIndex, i) | i <- [1..n]] `zip` (map (\x -> read x :: Int) (words x))
                --putStrLn("parsedLine " ++ (show lineIndex) ++ " is " ++ (show parsedLine)) 
                return $ parsedLine
                --TODO tu można dodać kontrolę długości linii
                
main::IO()
main = do
        stringN <- getLine
        let n = read stringN :: Int
        numThreads <- getNumCapabilities 
        x <- readGraphFromIO n (numThreads*2)
        putStrLn (show $ floydWarshall x)
        