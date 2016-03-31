{-# LANGUAGE ViewPatterns #-}

module Logging.Glue where

import GHC.RTS.Events

import Text.Scanf

getUserMessage :: Event -> Maybe String
getUserMessage (Event _ (UserMessage m)) = Just m
getUserMessage _ = Nothing

getContinuation, getSplit :: String -> Maybe (Int, String)
getContinuation = ints (lit "[[" :^ int :^ lit "]]") id
getSplit s = fmap (\(n, r)->(n, reverse r)) $ ints (lit "]]" :^ intR :^ lit "[[") id (reverse s)

glueMessages0 :: [Event] -> [Event]
glueMessages0 = map mapEvts
  where
    mapEvts (Event _ (UserMessage m)) = error ("Found naked user msg: " ++ m)
    mapEvts e = case spec e of
                  es@(EventBlock _ _ b) -> e {spec = es {block_events = glue b}}
                  _ -> e
    -- Events inside the block are ordered
    -- thus we should have all message parts adjacent.
    glue :: [Event] -> [Event]
    glue (x : xs) | Nothing <- getUserMessage x = x : glue xs
    glue (x : xs) | Just m <- getUserMessage x =
            case getSplit m of
                   Nothing          -> x : glue xs
                   Just (sn, chunk) -> glueinner sn [chunk] xs
    glue [] = []
    glue _  = error "Internal error (should not happen)."
    glueinner n acc (x : xs)
      | Just m <- getUserMessage x
      , Just (sn, chunk) <- getContinuation m =
          if sn /= n
            then error "Invalid message chunk: wrong continuation number."
            else case getSplit chunk of
                        Nothing         -> x {spec = UserMessage (concat $ reverse (chunk:acc))} : glue xs
                        Just (n1, m1) -> glueinner n1 (m1:acc) xs
    glueinner _ _ _ = error "Invalid message chunk: no continuation."

glueMessages :: EventLog -> EventLog
glueMessages (EventLog hdr (Data evts)) = EventLog hdr (Data $ glueMessages0 evts)
