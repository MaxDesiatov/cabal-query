{-# LANGUAGE TemplateHaskell, ViewPatterns #-}
module Distribution.Query
    ( queryFiles
    , queryIndex
    , exampleQuery ) where

import Codec.Archive.Tar as T hiding (unpack)
import Data.ByteString.Internal (w2c)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Data
import Data.DeriveTH
import Data.Generics.PlateData
import Data.List.Utils (join)
import Data.Maybe
import Distribution.Compiler
import Distribution.License
import Distribution.ModuleName hiding (main)
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Version
import Language.Haskell.Extension

import Distribution.Query.TH

$(deriveMany makeTypeable
  [''PackageDescription
  ,''Executable
  ,''Library
  ,''BuildType
  ,''VersionRange
  ,''Dependency
  ,''SourceRepo
  ,''CompilerFlavor
  ,''License
  ,''PackageIdentifier
  ,''BuildInfo
  ,''ModuleName
  ,''PackageName
  ,''RepoType
  ,''RepoKind
  ,''Extension])

$(deriveMany makeData
  [''PackageDescription
  ,''Executable
  ,''Library
  ,''BuildType
  ,''VersionRange
  ,''Dependency
  ,''SourceRepo
  ,''CompilerFlavor
  ,''License
  ,''PackageIdentifier
  ,''BuildInfo
  ,''PackageName
  ,''RepoType
  ,''RepoKind
  ,''Extension
  ,''Version])


-- Workaround for ModuleName, see http://code.google.com/p/ndmitchell/issues/detail?id=209
instance Data ModuleName where
    gfoldl  _ z   = z . simple . join "." . components
    gunfold _ z _ = z $ simple ""
    toConstr    _ = con_C
    dataTypeOf  _ = ty_T

con_C :: Constr
con_C = mkConstr ty_T "ModuleName" [] Prefix

ty_T :: DataType
ty_T = mkDataType "Distribution.ModuleName.ModuleName" [con_C]

queryIndex :: (PackageDescription -> [a]) -> ByteString -> [[a]]
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


queryFiles :: (PackageDescription -> [a]) -> [ByteString] -> [[a]]
queryFiles q = procQuery q . catMaybes . map parsePD

procQuery :: (PackageDescription -> [a]) -> [PackageDescription] -> [[a]]
procQuery q = filter (not . null) . map q

exampleQuery :: PackageDescription -> [(PackageIdentifier, License)]
exampleQuery l = [(x, y) | x <- universeBi l, y <- universeBi l, y == GPL]