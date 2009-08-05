{-# LANGUAGE TemplateHaskell #-}
module Main where

import Codec.Archive.Tar as T hiding (unpack)
import Control.Monad hiding (join)
import Data.ByteString.Internal (w2c)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Data
import Data.DeriveTH
import Data.Generics.PlateData
import Data.List.Stream
import Data.List.Utils (join)
import Distribution.Compiler
import Distribution.License
import Distribution.ModuleName hiding (main)
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Version
import Language.Haskell.Extension
import System.Environment

import Prelude hiding (map, head, null, (++), filter)

import TH

import Debug.Trace.Helpers

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

instance Data ModuleName where
    gfoldl _ z = z . simple . join "." . components
    gunfold _ z _ = z $ simple ""
    toConstr _ = con_C
    dataTypeOf _ = ty_T

con_C :: Constr
con_C = mkConstr ty_T "ModuleName" [] Prefix

ty_T :: DataType
ty_T = mkDataType "Distribution.ModuleName.ModuleName" [con_C]

extractFields :: (Data a, Data b) => (b -> Bool) -> a -> [b]
extractFields f l = [ x | x <- universeBi l, f x ]

readIndex :: ByteString -> [PackageDescription]
readIndex = ignoreParseErrors . map (return . packageDescription <=< parsePackageDescription) .
            foldEntries foldF [] (const []) . T.read
    where
      foldF :: Entry -> [String] -> [String]
      foldF e rest = (\(NormalFile s _) -> map w2c $ B.unpack s) (entryContent e) : rest

      ignoreParseErrors :: [ParseResult a] -> [a]
      ignoreParseErrors l = [ x | ParseOk _ x <- l ]

main :: IO ()
main = print . filter (not . null) . map (universeBi :: PackageDescription -> [License]) . readIndex =<< B.readFile . head =<< getArgs

licenseAndPackageId :: (PackageIdentifier, License) -> Bool
licenseAndPackageId (_, _) = True