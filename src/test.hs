{-# LANGUAGE TemplateHaskell #-}
import Data.Data
import Data.DeriveTH
import Distribution.ModuleName

$(derive makeData ''ModuleName)