module ParallelGaussianElimination (run) where



import Control.Parallel.Strategies ()
import Control.Concurrent.Chan
import Control.Monad
import Data.List.HT (rotate)
import Data.List (zip5)
import Control.Concurrent.Async
import Control.Concurrent ()
import Control.Concurrent.MVar ()
import Control.DeepSeq ()
import Data.List.Split (chunksOf)


type Number = Double
type Matrix = [[Number]]
type Vector = [Number]


concatLastDimension :: [[[a]]] -> [[a]]
concatLastDimension = map concat


generateChannels :: Int -> IO [Chan Vector]
generateChannels threadsNum = replicateM threadsNum newChan


distributeData :: Int -> [a] -> [[a]]
distributeData threadsNum list
    | length list `mod` threadsNum /= 0 = error "Unknowns must be multiple of processors."
    | otherwise = go list
        where
            go [] = replicate threadsNum []
            go xs = let (chunk, rest) = splitAt threadsNum xs
                        nextChunks = go rest in
                              zipWith (:) chunk nextChunks


listIntervalMap :: [a] -> Int -> Int -> (Int -> a -> a) -> [a]
listIntervalMap list a b f  = go list a b
    where
        go [] _ _ = []
        go (x:xs) a' b'
            | a' <=0 && b' >= 0 = f (a - a') x : go xs (a' - 1) (b' - 1)
            | otherwise = x : go xs (a' - 1) (b' - 1)


processAlgo :: (Vector, Vector, Chan Vector, Chan Vector, Int) -> IO (Vector, Vector)
processAlgo (a, b, predChan, succChan, rank) = mainLoop 0 (a, b, y)
    where
        n = length a `div` length b
        np = length b
        size = n `div` np
        y = replicate np 0.0

        mainLoop :: Int -> (Vector, Vector, Vector) -> IO (Vector, Vector)
        mainLoop k (aL, bL, yL)
            | k == n = do
                print ("OUT " ++ show rank)
                return (aL, yL)
            | otherwise = do
                let start = if rank <= k `mod` size then kSize + 1 else kSize
                    kSize = k `div` size
                    ksn = kSize * n

                    divisionStep :: (Vector, Vector, Vector) -> IO (Vector, Vector, Vector, Vector)
                    divisionStep (aL', bL', yL')
                        | k `mod` size == rank = do

                            let aLL' = listIntervalMap aL' (ksn + k + 1) (ksn + n - 1) (\_ v -> v / (aL' !! (ksn + k)))
                            print ((bL' !! kSize) / (aLL' !! (ksn + k)))
                            let yL'' = listIntervalMap yL' kSize kSize (\_ _ -> (bL' !! kSize) / (aLL' !! (ksn + k)))
                            let aL'' = listIntervalMap aLL' (ksn + k) (ksn + k) (\_ _ -> 1.0)
                            print (yL'' !! kSize)
                            let curRow = listIntervalMap (replicate (n + 1) 0.0) 0 n (\j _ -> if j == n then yL'' !! kSize else aL'' !! (ksn + j))
                            print curRow
                            print ("before writeChan-a " ++ show rank ++ " " ++ show k)
                            writeChan succChan curRow
                            print ("after writeChan-a " ++ show rank ++ " " ++ show k)

                            return (aL'', bL', yL'', curRow)
                        | otherwise = do
                            print ("before readChan " ++ show rank ++ " " ++ show k)
                            curRow <- readChan predChan
                            print ("after readChan " ++ show rank ++ " " ++ show k)

                            when ((rank + 1) `mod` size /= k `mod` size) $ do
                                print ("before writeChan-b " ++ show rank ++ " " ++ show k)
                                writeChan succChan curRow
                                print ("after writeChan-b " ++ show rank ++ " " ++ show k)
                            return (aL', bL', yL', curRow)

                    eliminationLoop :: Int -> (Vector, Vector, Vector, Vector) -> IO (Vector, Vector, Vector)
                    eliminationLoop i (aL', bL', yL', curRow)
                        | i == np = return (aL', bL', yL')
                        | otherwise = do
                            print ("ee" ++ show rank ++ " " ++ show k)
                            let iN = i * n
                            let iNK = iN + k
                            let aLL' = listIntervalMap aL' (iNK + 1) (iN + n - 1) (\j v -> v - ((aL' !! iNK) * (curRow !! (j - iN))))
                            let aL'' = listIntervalMap aLL' iNK iNK (\_ _ -> 0.0)
                            let bL'' = listIntervalMap bL' i i (\_ v -> v - ((aLL' !! iNK) * (curRow !! n)))
                            print ("eliminationLoop " ++ show rank ++ " " ++ show k)
                            eliminationLoop (i + 1) (aL'', bL'', yL', curRow)
                print ("ITER " ++ show rank ++ " " ++ show k)
                tmp <- divisionStep (aL, bL, yL)
                print tmp
                print ("after divisionStep " ++ show rank ++ " " ++ show k)
                tmp' <- tmp `seq` eliminationLoop start tmp
                mainLoop (k + 1) tmp'


unzipTuples :: [(a, b)] -> ([a], [b])
unzipTuples [] = ([], [])
unzipTuples ((x, y):rest) =
    let (firstElements, secondElements) = unzipTuples rest
    in (x:firstElements, y:secondElements)


collectData :: [[a]] -> [a]
collectData dData = go dData []
    where
        go :: [[a]] -> [a] -> [a]
        go [] _ = []
        go ([]:_) list = list
        go ((x:xs):rest) list = go (rest ++ [xs]) (list ++ [x])
        

backSubstitution :: [[Double]] -> [Double] -> [Double]
backSubstitution [] [] = []
backSubstitution [[]] [] = []
backSubstitution upperTriangularMatrix constants = backSubstitution' (reverse upperTriangularMatrix) (reverse constants) [] (length constants)
    where
        backSubstitution' [] _ acc _ = acc
        backSubstitution' _ [] acc _ = acc
        backSubstitution' (row:rows) (b:bs) acc i = backSubstitution' rows bs (x:acc) (i - 1)
            where x = b - sum (zipWith (*) (drop i row) acc)


run :: Matrix -> Vector -> Int -> IO Vector
run a b threadsNum = do
    let distributedA = concatLastDimension $ distributeData threadsNum a :: [Vector]
        distributedB = distributeData threadsNum b :: [Vector]
        n = length a
    predChannels <- generateChannels threadsNum
    let succChannels = rotate 1 predChannels :: [Chan Vector]
    let inputList = zip5 distributedA distributedB predChannels succChannels [0..threadsNum-1]

    res <- mapConcurrently processAlgo inputList
    let (distributedResA, distributedResB) = unzipTuples res
        (resA, resB) = (collectData $ map (chunksOf n) distributedResA, collectData distributedResB)
    let result = backSubstitution resA resB

    return result














--    resultMVar <- newEmptyMVar
--    forM_ inputList $ \input -> do
--        forkIO $ do
--            result <- processAlgo input
--            putMVar resultMVar result
--            threadDelay 1000
--
--
--    print "ggg"
--    threadDelay 1000000
--    -- Collect results from threads
--    results <- replicateM (length inputList) (takeMVar resultMVar)
--    print "hhh"
--    threadDelay 1000000
--      -- Print the results
--    print results

----
----
----    -- Launch threads to process the input list
----    forM_ inputList $ \input -> do
----        forkIO $ do
----            result <- processAlgo input
----            putMVar resultMVar result
----            threadDelay 1000
----
----
----    print "ggg"
----    threadDelay 10000
----    -- Collect results from threads
----    results <- replicateM (length inputList) (takeMVar resultMVar)
----    print "hhh"
----    threadDelay 1000
----      -- Print the results
----    let a = results
----    print (results !! 0)
--
--
--
--
--    result <- mapConcurrently processAlgo inputList
--    threadDelay 1000000
--    print $ show result
--    return []
--
--
----    forM_ [0..n-1] $ \k -> do
----        let kSize = k `div` size
----            ksn = kSize * n
----        when (k `mod` size == rank) $ do
----                let aL = listIntervalMap aL (ksn + k) (ksn + n - 1)
----                                     (\i v -> if i == ksn + k then 1 else v / (aL !! (ksn + k)))
----                    yL = listIntervalMap yL kSize kSize (\_ _ -> (bL !! kSize) / (aL !! (ksn + k)))
----
----                    curRow = listIntervalMap curRow 0 n (\i _ -> if i == n then yL !! kSize else aL !! (ksn + i))
----                writeChan succChan curRow
----        when (k `mod` size /= rank) $ do
----            curRow <- readChan predChan
----            when ((rank+1) `mod` size /= k `mod` size) $ do
----                writeChan succChan curRow
----        let start = if rank <= k `mod` size then kSize + 1 else kSize
----        forM_ [start..np-1] $ \i -> do
----            let iN = i * n
----                iNK = iN + k
----                aL = listIntervalMap aL iNK (iN + n - 1)
----                                            (\j v -> if j == iNK then 0.0 else v - ((aL !! iNK) * (curRow !! j)))
----                bL = listIntervalMap bL i i (\i v -> v - ((aL !! iNK) * (curRow !! n)))
--
--
--
--
--    -- Use mapConcurrently to parallelize the IO operations
----    result <- mapConcurrently (uncurry5 processAlgo)
----                              (zip5 distributedA distributedB predChannels succChannels [0..threadsNum-1])
----    print "GGG"
----    print (show (head result))
----    -- Combine the results if necessary
----    let combinedResult = foldr1 (<>) result -- This depends on how you want to combine the results
----    print combinedResult
--    -- Deepseq the combined result if needed
----    let deepseqResult = rdeepseq combinedResult
--
----    let result = parMap rdeepseq (uncurry5 processMatricesInParallel)
----                                 (zip5 distributedA distributedB predChannels succChannels [0..threadsNum-1])