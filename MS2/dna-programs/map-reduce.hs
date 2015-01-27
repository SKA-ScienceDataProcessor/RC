{-# LANGUAGE TemplateHaskell #-}
module Main(main) where

import Data.Char
import Data.Ord
import Data.List
import qualified Data.Map as Map

import DNA





wordMapper :: Mapper () (String,Int)
wordMapper = mapper
    (\_ -> initWord)
    splitWord
    shuffle
  where
    -- Read data. We hardcode fact that we have 4 mapper here
    initWord = do
        n <- rank
        liftIO $ print n
        case n of
          0 -> liftIO $ readFile "dict1"
          1 -> liftIO $ readFile "dict1"
          2 -> liftIO $ readFile "dict1"
          3 -> liftIO $ readFile "dict1"
          _ -> error "Too many mappers"
    -- Get single word
    splitWord txt = return $
      case span isLetter $ dropWhile (not . isLetter) txt of
        ("","")     -> Nothing
        (word,rest) -> Just (rest,(map toLower word,1))
    -- Choose to which reducer send pair. We simply choose based on first letter of the word
    shuffle _ ("",_) = 0
    shuffle n (c:_,_)  = (n * (ord c - ord 'a')) `div` 26


wordReducer :: CollectActor (String,Int) [(String,Int)]
wordReducer = collectActor
    (\m (k,v) -> return $ Map.insertWith (+) k v m)
    (return Map.empty)
    (return . Map.toList)


----------------------------------------------------------------
-- Distributed dot product
----------------------------------------------------------------

remotable [ 'wordMapper
          , 'wordReducer
          ]

main :: IO ()
main =  dnaRun rtable $ do
    -- Start mapper
    rM  <- selectMany (N 4) (NNodes 1) []
    liftIO $ print $ length rM
    shM <- startMappers rM Normal $(mkStaticClosure 'wordMapper)
    -- Start reducer
    rR  <- selectMany (N 2) (NNodes 1) []
    shR <- startCollectorGroupMR rR Normal $(mkStaticClosure 'wordReducer)
    -- Connect and gather output
    sendParam () (broadcast shM)
    connect shM shR
    fut <- delayGroup shR
    res <- gather fut
             (\acc wordCount -> Map.unionWith (+) acc (Map.fromList wordCount))
             Map.empty
    -- Print 100 of most common words
    liftIO $ mapM_ print $ take 1000 $ reverse $ sortBy (comparing snd) $ Map.toList res
    return ()
  where
    rtable = __remoteTable
