module TH where

import Language.C.Analysis.TravMonad
import Data.DeriveTH
import Language.Haskell.TH

deriveMany :: Derivation -> [Name] -> Q [Dec]
deriveMany = concatMapM . derive