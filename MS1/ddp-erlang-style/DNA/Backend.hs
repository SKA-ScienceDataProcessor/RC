-- |Backend.hs
--
-- Backend implementation for DNA project.
--
-- Copyright (C) 2014 Braa Research, LLC.

module DNA.Backend where

import Control.Distributed.Process
import Control.Distributed.Process.Platform

findSlavesByURIs :: [String] -> Int -> IO [NodeId]
findSlavesByURIs uris _ignoredTimeout = do
	nodes' <- mapM parseNode uris
	let errURIs = concatMap snd nodes'
	case errURIs of
		[] -> return $ concatMap fst nodes'
		errs -> do
			error $ "error parsing uris: "++ show errs
	where
		parseNode uri = do
			case parseURI uri of
				Just (URI _ (Just (URIAuth _ host port)) _ _ _) -> return (error uri, [])
				_ -> return ([], [uri])
