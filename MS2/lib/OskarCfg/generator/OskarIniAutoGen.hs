{-# LANGUAGE TemplateHaskell #-}

module OskarIniAutoGen where

import GHC.Exts (IsString(..))
import Data.List (
    isPrefixOf
  , intercalate
  )
import Data.Time (
    UTCTime(..)
  , fromGregorian
  )
import Language.Haskell.TH

class NoAuto a

newtype OList a = OList [a]
instance Show a => Show (OList a) where
  show (OList l) = intercalate " " (map show l)

newtype OStr = OStr String
instance Show OStr where show (OStr s) = s
instance IsString OStr where fromString = OStr

data OBool = OTrue | OFalse
instance Show OBool where
  show OTrue = "true"
  show OFalse = "false"

data OImageType =
    OImageTypeStokes
  | OImageTypeStokesI
  | OImageTypeStokesQ
  | OImageTypeStokesU
  | OImageTypeStokesV
  | OImageTypePolLinear
  | OImageTypePolXX
  | OImageTypePolYY
  | OImageTypePolXY
  | OImageTypePolYX
  | OImageTypePSF

instance Show OImageType where
  show OImageTypeStokes    = "STOKES"
  show OImageTypeStokesI   = "I"
  show OImageTypeStokesQ   = "Q"
  show OImageTypeStokesU   = "U"
  show OImageTypeStokesV   = "V"
  show OImageTypePolLinear = "LINEAR"
  show OImageTypePolXX     = "XX"
  show OImageTypePolYY     = "YY"
  show OImageTypePolXY     = "XY"
  show OImageTypePolYX     = "YX"
  show OImageTypePSF       = "PSF"

class Def a where
  def :: a

instance Def [a] where def = []
instance Def OStr where def = OStr []
instance Def (OList a) where def = OList []
instance Def (Maybe a) where def = Nothing
instance (Def a, Def b) => Def (a,b) where def = (def, def)
instance Def Int where def = 0
instance Def Double where def = 0
instance Def UTCTime where def = UTCTime (fromGregorian 0 0 0) 0
instance Def OBool where def = OFalse
instance Def OImageType where def = OImageTypeStokesI

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
scat ps s = ps ++ '\\':s

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
         Just s -> [show_immediate pfx (strip_rec_uniq_prefix sel_fn_name) s]
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
    showvar (vname, _, vtyp) = select_fun vtyp (cut_suffix $ pprint vname)
    cut_suffix = reverse . drop 1 . dropWhile (/= '_') . reverse
    select_mb t
      | "OskarSettings" `isPrefixOf` (pprint t) = show_req_maybe_q
      | otherwise = show_immediate_maybe_q
    select_plain t
      | "OskarSettings" `isPrefixOf` (pprint t) = show_req_q
      | otherwise = show_immediate_q
    select_fun (AppT (ConT n) t)
      | n == ''Maybe = select_mb t
      | n == ''OList = select_plain t
      | otherwise = error ("Unimplemented: " ++ pprint n)
    select_fun t = select_plain t
gen_fun _ = error "Can't handle data declaration in this form"

gen_default :: Dec -> DecQ
gen_default (DataD _cxt tname _tys [RecC cname vartyps] _drv) =
    instanceD
      (return [])
      (appT (conT ''Def) (conT tname)) [
          valD (varP 'def) (normalB $ return $ foldl AppE (ConE cname) (map VarE $ replicate (length vartyps) 'def))
        []]
gen_default _ = error "Can't handle data declaration in this form when generating default."

gen_all :: DecsQ -> DecsQ
gen_all decsq = do
  decs <- decsq
  let exclude_names = concatMap checkNoauto decs
  runIO (putStrLn $ "Excluded:\n" ++ pprint exclude_names)
  decssi <- mapM gen_fun (filter (toInclude exclude_names) decs)
  let decsd = filter isData decs
  decsdi <- mapM gen_default decsd
  return (decsd ++ decssi ++ decsdi)

isData :: Dec -> Bool
isData DataD{} = True
isData _ = False

checkNoauto :: Dec -> [Name]
checkNoauto (InstanceD [] (AppT (ConT classname) (ConT n)) [])
  | classname == ''NoAuto = [n]
  | otherwise = []
checkNoauto _ = []

toInclude :: [Name] -> Dec -> Bool
toInclude exclude_set (DataD _cxt tname _tys _cons _drv)
  | tname `elem` exclude_set = False
  | otherwise = True
toInclude _ _ = False
