{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
module HaddockClean where

import Control.Applicative
import Data.Data
import Data.Generics.Uniplate.Data
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import System.IO
import System.Process

deriving instance Typeable TagTree
deriving instance Data t => Data (TagTree t)


----------------------------------------------------------------
-- HTML manipulation
----------------------------------------------------------------

type Trans = [TagTree String] -> [TagTree String]
  
findTag :: Eq t => t -> TagTree t -> [TagTree t]
findTag t' r = case r of
  TagLeaf (TagOpen t _) | t' == t -> [r]
  TagLeaf _                       -> []
  TagBranch t _ tree
    | t' == t   -> [r]
    | otherwise -> findTag t' =<< tree

findTagWithAttrs :: Eq t => t -> [(t,t)] -> TagTree t -> [TagTree t]
findTagWithAttrs t' ids' r = case r of
  TagLeaf (TagOpen t _) | t' == t -> [r]
  TagLeaf _                       -> []
  TagBranch t ids tree
    | t' == t && ids' == ids  -> [r]
    | otherwise               -> findTag t' =<< tree

inlineTag :: Eq t => t -> [(t,t)] -> [TagTree t] -> [TagTree t]
inlineTag tag attrs = concatMap go
  where
    go (TagBranch t as ts)
      | t==tag && as==attrs = ts
      | otherwise           = [TagBranch t as (go =<< ts)]
    go r = [r]

deleteById :: String -> [(String,String)] -> Trans
deleteById tag attrs  = transformTree $ \case
  TagBranch t as _ | t==tag && as==attrs -> []
  t                                      -> [t] 

unescape :: String -> String
unescape ('&':'g':'t':';':ss) = '>' : unescape ss
unescape ('&':'l':'t':';':ss) = '<' : unescape ss
unescape ('&':'q':'u':'o':'t':';':ss) = '"' : unescape ss
unescape (s:ss) = s : unescape ss
unescape [] = []

-- Extract haddock definition
haddockDef :: TagTree String -> [TagTree String]
haddockDef (TagBranch "div" [("class","top")] ts)
  = transformTree decl ts ++ [TagLeaf (TagText "\n\n")]
  where
    decl (TagBranch "p" [("class","src")] ts) =
      [TagLeaf $ TagText $ "````\n" ++ concat [t | TagText t <- universeBi ts] ++  "\n````\n\n"]
    decl t = [t]
haddockDef t = [t]


trans :: [TagTree String] -> [TagTree String]
trans
  = id
  -- . transformTree haddockDef
    -- Headers
  . transformTree (\case
      TagBranch "h1" _ [TagLeaf (TagText t)] -> [TagLeaf (TagText ("# " ++ t ++ "\n\n"))]
      t -> [t]
                  )
    -- Reformat some obvious tags
  . transformTree (\case
      TagBranch "code" [] [TagLeaf (TagText t)] ->
        [TagLeaf (TagText $ "`" ++ unescape t ++"`")]
      t -> [t])
  . transformTree (\case
      TagBranch "pre" [] [TagLeaf (TagText t)] ->
        [TagLeaf (TagText $ "\n\n````.haskell\n" ++ unescape t ++ "\n````\n\n")]
      t -> [t])
    -- Custom headers
  . transformTree (\case
      TagBranch "h3" [] [TagLeaf (TagText t)] -> [TagLeaf (TagText ("### " ++ t ++ "\n"))]
      t -> [t]
                  )
    -- Collapse tags
  . deleteById "div" [("id","interface")]
  . inlineTag "div" [("id","description")]
  . inlineTag "div" [("class","doc")]
    -- Clean HTML
  . deleteById "p"   [("class","caption")]
  . deleteById "div" [("id","table-of-contents")]
  . deleteById "div" [("id","module-header")]
  . deleteById "div" [("id","footer")]
  . deleteById "div" [("id","synopsis")]
  . deleteById "div" [("id","package-header")]
  . transformTree (\case
       TagBranch "p" [] ts -> ts  ++ [TagLeaf (TagText "\n\n")]
       t                   -> [t]
                  )
  . transformTree (\case
       TagBranch "a" _ ts -> ts
       t                  -> [t]
                  )
    -- Extract body
  . (\[TagBranch "div"  _ ts] -> ts)
  . concatMap (findTagWithAttrs "div" [("id","content")])
  . (\[TagBranch "body" _ ts] -> ts)
  . concatMap (findTag "body")

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

get :: IO [TagTree String]
get = do
  s <- readFile "dist/doc/html/dna-ms4/DNA.html"
  return $ tagTree $ parseTags s

go = do
  s <- get
  (Just h,_,_,p) <- createProcess (proc "less" []){ std_in = CreatePipe }
  let res = unescape
          $ renderTags
          $ flattenTree
          $ trans
          $ s
  hPutStr h res
  hClose h
  writeFile "../../RC.wiki/DNA.md" res
  waitForProcess p
  return ()
