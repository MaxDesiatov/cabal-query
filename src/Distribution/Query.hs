{-# LANGUAGE ScopedTypeVariables #-}

-- | This package uses "Data.Generics.PlateData", so
--
-- * when Cabal package format changes, we don't have to rewrite anything
--
-- * all queries are statically typed
--
-- * as a disadvantage, we may suffer some performance loss when doing very complex queries,
--   anyway most of processing goes while we read package descriptions, not querying them
--
-- Example of enduser querying code:
--
-- @
--module Main where
--
-- import qualified Data.ByteString.Lazy as B
--import System.Environment
--import Distribution.Query
--import Distribution.Compiler
--import Distribution.License
--import Distribution.ModuleName hiding (main)
--import Distribution.Package
--import Distribution.PackageDescription
--import Distribution.Version
--import Distribution.Text
--import Language.Haskell.Extension
--
-- main = (head \`fmap\` getArgs) >>=
--        B.readFile >>=
--        mapM_ (putStrLn . show . (\x -> (display $ package x, display $ license x))) .
--        queryIndex (Not (Id (== GPL)) :& Not (Id (== BSD3)))
-- @
--
-- You can query any field of 'PackageDescription' no matter how deep it is.
-- You don't need to provide any type signature for comparison functions,
-- which are wrapped in 'Id', as long as you use data constructors for which type can be inferred.
--
-- See 'PackageDescription' fields for details.
module Distribution.Query
    ( queryFiles
    , queryIndex
    , module Distribution.Query.Types ) where

import Codec.Archive.Tar as T hiding (unpack)
import Data.ByteString.Internal (w2c)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Generics.PlateData
import Data.Maybe
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse

import Distribution.Query.Types

-- | Queries an index file, which is commonly located at
-- <~/.cabal/packages/hackage.haskell.org/00-index.tar> in POSIX systems.
queryIndex :: Query
           -> ByteString           -- ^ The index file must be read as lazy ByteString
           -> [PackageDescription] -- ^ Returns a list of package descriptions which satisfy the query
queryIndex q = procQuery q . foldEntries foldF [] (const []) . T.read
    where
      foldF :: Entry -> [PackageDescription] -> [PackageDescription]
      foldF c rest =
          case entryContent c of
            NormalFile s _ -> maybe rest (:rest) $ parsePD s
            _              -> rest

parsePD :: ByteString -> Maybe PackageDescription
parsePD s =
    case (parsePackageDescription $ map w2c $ B.unpack s) of
      ParseOk _ x -> Just $ packageDescription x
      _           -> Nothing

-- | Queries .cabal files.
queryFiles :: Query
           -> [ByteString]         -- ^ All files must be read as lazy ByteStrings
           -> [PackageDescription] -- ^ Returns a list of package descriptions which satisfy the query
queryFiles q = procQuery q . catMaybes . map parsePD

procQuery :: Query -> [PackageDescription] -> [PackageDescription]
procQuery q = filter (doQuery q)

doQuery :: Query -> PackageDescription -> Bool
doQuery (Id  f)    pd = or [f x | x <- universeBi pd]
doQuery (Not q)    pd = not $ doQuery q pd
doQuery (q1 :& q2) pd = doQuery q1 pd && doQuery q2 pd
doQuery (q1 :| q2) pd = doQuery q1 pd || doQuery q2 pd