-- Based on O.Kiselyov work
-- Copyright (C) 2009 Kyra
-- Copyright (C) 2016 Braam Research, LLC.

{-# LANGUAGE GADTs #-}

module Text.Scanf where

import Data.List ( isPrefixOf, uncons )
import Data.Char ( isDigit, isSpace )
-- import Control.Parallel ( par, pseq )

t2j, t2n :: Bool -> a -> Maybe a
t2j True  v = Just v
t2j False _ = Nothing
t2n = t2j . not

readInt :: Read a => String -> Maybe (a, String)
readInt s =
  let (nums,rest) = span isDigit s
  in null nums `t2n` (read nums, rest)

readIntR :: Read a => String -> Maybe (a, String)
readIntR = readInt . reverse

type Scan a = String -> Maybe (a, String)

data Scanner a b where
    SLit :: String -> Scanner a a
    SDrop:: Int -> Scanner a a
    SSkip:: (Char -> Bool) -> Scanner a a
    SC   :: Scan b -> Scanner a (b -> a)
    (:^) :: Scanner b c -> Scanner a b -> Scanner a c

-- Basics
lit :: String -> Scanner a a
lit  = SLit

dr :: Int -> Scanner a a
dr  = SDrop

skip :: (Char -> Bool) -> Scanner a a
skip  = SSkip

gen :: b -> Scanner a (b -> a)
gen v = SC (\bs -> Just (v, bs))

int :: Scanner a (Int -> a)
int  = SC readInt

intR :: Scanner a (Int -> a)
intR  = SC readIntR

bigint :: Scanner a (Integer -> a)
bigint  = SC readInt

intwe :: Int -> Scanner a (Int -> a)
intwe w =
  SC $ \bs -> let (pre, post) = splitAt w bs
              in readInt pre >>= (\(v, rest) -> null rest `t2j` (v, post))

char :: Scanner a (Char -> a)
char = SC uncons

-- Utils
litc :: Char -> Scanner a a
litc = SLit . (:[])

sk :: Char -> Scanner a a
sk c = SSkip (== c)

skws :: Scanner a a
skws = SSkip isSpace

-- Eats em all
str :: Scanner a (String -> a)
str = SC $ \bs -> Just (bs, [])

strw :: Int -> Scanner a (String -> a)
strw w = SC (Just . (splitAt w))

str_to :: Char -> Scanner a (String -> a)
str_to stop =
  SC $ \input -> let (done, rest) = break (== stop) input
                 in Just (done, rest)

-- Default unoptimized
deffmt :: Read b => Scanner a (b -> a)
deffmt = SC parse
  where
    parse s = case reads s of
                [(v,s')] -> Just (v, s')
                _        -> Nothing

sdouble :: Scanner a (Double -> a)
sdouble = deffmt

type ReadM a = String -> Maybe (a, String)

ints :: Scanner a b -> b -> ReadM a
ints (SLit s) x inp = t2j (isPrefixOf s inp) (x, drop (length s) inp)
ints (SDrop n) x inp = t2j (length inp >= n) (x, drop n inp)
ints (SSkip p) x inp = Just (x, dropWhile p inp)
ints (SC sc) f inp = fmap (\(v, s) -> (f v, s)) (sc inp)
ints (a :^ b) f inp = ints a f inp >>= (\(vb, inp') -> ints b vb inp')

sscanf :: Scanner a b -> b -> String -> Maybe a
sscanf fm f = fmap fst . ints fm f

liftR :: (a -> b) -> ReadM a -> ReadM b
liftR f rd = fmap (\(v,r) -> (f v, r)) . rd

infixr 4 ^:
(^:) :: ReadM a -> ReadM a -> ReadM a
(^:) r1 r2 s = go (r1 s) (r2 s)
  where
    go v@(Just _) _ = v
    go _ c = c
{-
(^:) r1 r2 s = par r2r (pseq r1r (go r1r r2r))
  where
    r2r = r2 s
    r1r = r1 s
    go v@(Just _) _ = v
    go _ c = c
 -}

{-
ts = sscanf sdouble id "543.98767"
ts1 = sscanf (sdouble :^ str) (\s1 s2 -> (s1, s2)) "543.98767swean"
ts2 = sscanf (str_to_and_eat ';' :^ str) (\s1 s2 -> (s1, s2)) "gago;swin"

fmt30 = lit "abc" :^ int :^ lit "cde"
fmt3 = fmt30 :^ sdouble :^ char
ts3 = sscanf fmt3 (\i f c -> (i,f,c)) "abc5cde15.0c"
-- Just (5,15.0,'c')

fmt4 = skDigs :^ lit ":" :^ skDigs :^ str
ts4 = sscanf fmt4 id "40000:0 dsoiewr"
ts41 = sscanf fmt4 id "40000::0 dsoiewr"
 -}
