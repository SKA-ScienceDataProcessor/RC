{-# LANGUAGE
    PackageImports
  , MultiWayIf
  #-}

module Main where

import Data.List (
    isPrefixOf
  , delete
  )
import Data.Char (
    isUpper
  , toUpper
  , toLower
  )
import Text.PrettyPrint (
    Doc
  , empty
  , render
  , char
  , text
  , nest
  , (<+>)
  , ($+$)
  )
import qualified Data.Map as M
import Text.Printf (printf)
import System.Environment (getArgs)
-- import System.Directory (createDirectoryIfMissing)

import "language-c" Language.C
import "language-c" Language.C.System.GCC
import "language-c" Language.C.Analysis

main :: IO ()
main = do
  args <- getArgs
  if null args
    then putStrLn "Please provide a path to OSKAR source directory."
    else an (head args) >>= pr

an :: FilePath -> IO GlobalDecls
an path_to_oskar =
  do parse_result <- parseCFile (newGCC "gcc") Nothing
                       ["-I" ++ path_to_oskar ++ "/settings/struct"] "oskar_Settings_fixed.h"
     case parse_result of
       Left parse_err -> error (show parse_err)
       Right ast      -> let Right (gd, _) = runTrav_ (analyseAST ast) in return gd

pr :: GlobalDecls -> IO ()
pr gd = do
  -- createDirectoryIfMissing True "generated"
  writeFile "../OskarCfg.hs" . render $
    text "module OskarCfg where" $+$ (M.foldr (\tag doc -> prtag tag $+$ doc) empty (gTags gd))

upperCase :: String -> String
upperCase (s:ss) = toUpper s : ss
upperCase [] = []

fixName :: Pretty a => a -> String
fixName sueref = let
    s = render $ pretty sueref in
  if | "oskar_" `isPrefixOf` s -> "Oskar" ++ drop 6 s
     | '$' `elem` s -> "ENUM_" ++ delete '$' s
     | True -> s

-- Make prefix for record fields
genPrefix :: String -> String
genPrefix = (++ "_") . map toLower . filter isUpper

-- We ignore _expr, because all relevant OSKAR enums
-- are "good".
pprEnumVal :: Enumerator -> Doc
pprEnumVal (Enumerator ident _expr _etyp _info) = pretty ident

showTn :: TypeName -> String
showTn (TyIntegral TyInt) = "Int"
showTn (TyFloating TyDouble) = "Double"
showTn _ = error "Unrecognized type name"

printfPtr :: String -> String
printfPtr = printf "[%s]"

showPtr :: Type -> String
showPtr (DirectType (TyIntegral TyChar) _ _) = "String"
showPtr (DirectType tn _ _) = printfPtr $ showTn tn
showPtr (PtrType pt _ _) = printfPtr $ showPtr pt
showPtr td@(TypeDefType {}) = printfPtr $ showVarType td
showPtr _ = error "Unrecognized * type"

showVarType :: Type -> String
showVarType (DirectType tn _ _) = showTn tn
showVarType (PtrType pt _ _) = showPtr pt
showVarType (TypeDefType (TypeDefRef ident _ _) _ _) = fixName ident
showVarType (ArrayType (DirectType tn _ _) (ArraySize _ (CConst(CIntConst (CInteger 2 _ _) _))) _ _) = 
  let ts = showTn tn in printf "(%s, %s)" ts ts

showVarType x = error $ "Unrecognized type " ++ (render $ pretty x)

prtag :: TagDef -> Doc
prtag (EnumDef (EnumType sueref (e:erest) _attrs _info)) =
  text "data" <+> text (fixName sueref) <+> char '='
  $+$ (nest 4 $ pprEnumVal e)
  $+$ (nest 2 $ foldr (\enum doc -> text "|" <+> pprEnumVal enum $+$ doc) empty erest)
-- We ignore _cty_kind because have only relevant structs here -- no unions.
prtag (CompDef (CompType sueref _cty_kind (m:mrest) _attrs _info)) =
  let
    tname = fixName sueref
    tnamedoc = text tname
    prefix = genPrefix tname
    pprStrucVal (MemberDecl (VarDecl varname _attrs vartype) _expr _info) =
      (text . (prefix ++) . render . pretty $ varname) <+> text "::" <+> text (showVarType vartype)
    pprStrucVal _ = error "Unrecognised input."
  in
  text "data" <+> tnamedoc <+> char '=' <+> tnamedoc <+> char '{'
  $+$ (nest 4 $ pprStrucVal m)
  $+$ (nest 2 $ foldr (\var doc -> text "," <+> pprStrucVal var $+$ doc) empty mrest)
  $+$ (nest 2 $ char '}')
prtag _ = error "Impossible happened. Check if only 'gTags' is used."
