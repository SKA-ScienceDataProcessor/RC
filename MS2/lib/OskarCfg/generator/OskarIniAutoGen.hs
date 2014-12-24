{-# LANGUAGE TemplateHaskell #-}

module OskarIniAutoGen (gen_all) where

import Data.List (isPrefixOf)
import Language.Haskell.TH

class ShowRecWithPrefix a where
  showRecWithPrefix :: String -> a -> [String]

{-
data OskarTyp2

test :: DecsQ
test = gen_all
  [d|
      data OskarTyp1 = OskarTyp1 {
          gag_f1 :: Int
        , gag_f2 :: Maybe String
        , gag_f3 :: Maybe OskarTyp2
        }
    |]

test_res :: (Dec -> String) -> IO String
test_res fun = runQ $ fmap (fun . sndl) test
  where sndl (_:e:_) = e

test_ast = test_res show
test_ppt = test_res pprint
 -}

scat :: String -> String -> String
scat [] s = s
scat ps s = ps ++ '/':s

show_immediate :: Show a => String -> String -> a -> String
show_immediate pfx field_name v = scat pfx field_name ++ '=' : show v

var :: String -> ExpQ
var = varE . mkName

strip_rec_uniq_prefix :: String -> String
strip_rec_uniq_prefix pn = let
    (_, un) = break (== '_') pn
  in case un of
       (_ : n : rest) -> n : rest
       _       -> error "Invalid name or record member."

show_immediate_q :: String -> ExpQ
show_immediate_q sel_fn_name =
   [| \pfx v -> [show_immediate pfx (strip_rec_uniq_prefix sel_fn_name) ($(var sel_fn_name) v)]
    |]

show_immediate_maybe_q :: String -> ExpQ
show_immediate_maybe_q sel_fn_name =
   [| \pfx v -> case ($(var sel_fn_name) v) of
         Nothing -> []
         Just s -> show_immediate pfx (strip_rec_uniq_prefix sel_fn_name) s ($(var sel_fn_name) v)
    |]

show_req_q :: String -> ExpQ
show_req_q sel_fn_name =
   [| \pfx v -> showRecWithPrefix (scat pfx $ strip_rec_uniq_prefix sel_fn_name) ($(var sel_fn_name) v)
    |]

show_req_maybe_q :: String -> ExpQ
show_req_maybe_q sel_fn_name =
   [| \pfx v -> case ($(var sel_fn_name) v) of
         Nothing -> []
         Just rr -> showRecWithPrefix (scat pfx $ strip_rec_uniq_prefix sel_fn_name) rr
    |]


mkAll :: [ExpQ] -> ExpQ
mkAll leq =
  [| \pfx v -> concatMap (\f -> f pfx v) $(listE leq)
   |]

gen_fun :: Dec -> DecQ
gen_fun (DataD _cxt tname _tys [RecC _cname vartyps] _drv) =
    instanceD
      (return [])
      (appT (conT ''ShowRecWithPrefix) (conT tname)) [valD (varP 'showRecWithPrefix) (normalB fexp) []]
  where
    fexp = mkAll (map showvar vartyps)
    showvar (vname, _, vtyp) = let name = pprint vname in apps (select_fun vtyp) name
    apps fq pfx = appE (varE fq) (stringE pfx)
    select_mb t
      | "Oskar" `isPrefixOf` (pprint t) = 'show_req_maybe_q
      | otherwise = 'show_immediate_maybe_q
    select_plain t
      | "Oskar" `isPrefixOf` (pprint t) = 'show_req_q
      | otherwise = 'show_immediate_q
    select_fun (AppT (ConT n) t)
      | n == ''Maybe = select_mb t
      | otherwise = error "Unimplemented 137!"
    select_fun t = select_plain t
gen_fun _ = error "Can't handle data declaration in this form"

gen_all :: DecsQ -> DecsQ
gen_all decsq = do
  decs <- decsq
  decsi <- mapM gen_fun decs
  return (decs ++ decsi)
