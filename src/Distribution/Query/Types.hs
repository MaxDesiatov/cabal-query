{-# LANGUAGE TemplateHaskell, GADTs #-}
module Distribution.Query.Types
    ( Query(..) ) where

import Data.Data
import Data.DeriveTH
import Data.List.Utils (join)
import Distribution.Compiler
import Distribution.License
import Distribution.ModuleName hiding (main)
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Version
import Language.Haskell.Extension

import Distribution.Query.TH

-- | Heterogenous query tree.  Example of constructed query:
--
-- > Not (Id (== GPL)) :& Not (Id (== BSD3))
--
-- 'Id' takes comparison function as its argument.
-- Commonly this must be a partially applied ('/=') or ('==').
-- Data instance is required for 'PackageDescription' traversal.
-- All appropriate instances are generated automagically,
-- so you don't have to bother as long as Cabal doesn't change its
-- package description format.
data Query where
    (:&) :: Query -> Query -> Query
    (:|) :: Query -> Query -> Query
    Not  :: Query -> Query
    Id   :: Data a => (a -> Bool) -> Query

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
