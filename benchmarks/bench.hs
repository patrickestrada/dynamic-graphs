{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

import Criterion.Main

import Control.Monad (forM)
import qualified Data.Graph.Dynamic.EulerTour as ETF
import qualified Data.Graph.Dynamic.Levels as Levels

main :: IO ()
main =
    defaultMainWith
        defaultConfig
        [ bgroup "tree" $ map tree [64 * n | n <- [1..16]]
        , bgroup "graph" $ map graph [16 * n | n <- [1..16]]
        , bgroup "graphInsert" $ map graphInsertion [32 * n | n <- [1 .. 16]]
        ]
  where
    graph n = bench (show n) $ nfIO (completeGraph n)
    tree n = bench (show n) $ nfIO (completeBinaryTree n)
    graphInsertion n = bench (show n) $ nfIO (graphInsertions n)

completeGraph :: Int -> IO [(Bool, Bool)]
completeGraph n = do
    levels <- Levels.edgeless' [0 .. n - 1]
    mapM_ (\(x, y) -> Levels.link levels x y) edges
    mapM
        ( \(x, y) -> do
            c1 <- Levels.connected levels x y
            Levels.cut levels x y
            c2 <- Levels.connected levels x y
            return (c1, c2)
        )
        edges
  where
    edges = [(x, y) | x <- [0 .. n - 1], y <- [x + 1 .. n - 1]]

completeBinaryTree :: Int -> IO [(Bool, Bool)]
completeBinaryTree n = do
    etf <- ETF.edgeless' [0 .. n - 1]
    mapM_ (\(x, y) -> ETF.link etf x y) edges
    mapM
        ( \(x, y) -> do
            c1 <- ETF.connected etf x y
            ETF.cut etf x y
            c2 <- ETF.connected etf x y
            return (c1, c2)
        )
        edges
    return []
  where
    edges = [(x, y) | x <- [0 .. n - 1], y <- filter (< n) [2 * x, 2 * x + 1]]

graphInsertions :: Int -> IO [Bool]
graphInsertions n = do
    levels <- Levels.empty' @Int
    _ <- forM [0 .. n - 1] $ Levels.insert levels
    _ <- forM [0 .. n - 2] $ \x -> Levels.link levels x (x + 1)
    _ <- forM [0 .. n `div` 2 - 1] $ \x -> Levels.link levels x (x + 2)
    forM [0 .. n - 1] $ Levels.delete levels
