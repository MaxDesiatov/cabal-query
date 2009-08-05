module Distribution.Query.TH where

import MonadUtils
import Data.DeriveTH
import Language.Haskell.TH

deriveMany :: Derivation -> [Name] -> Q [Dec]
deriveMany = concatMapM . derive
