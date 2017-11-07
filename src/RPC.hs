module RPC
  ( Procedure
  , Parameters(..)
  , JsonString(..)
  , UserID(..)
  , ProcedureID(..)
  , ProcedureRegister(..)
  , execute
  ) where

import qualified Data.Map as M

type Procedure = (Parameters -> IO JsonString)
data Parameters =
  Parameters UserID
             (M.Map String String)
  deriving (Show)
newtype JsonString =
  JsonString String

instance Show JsonString where
  show (JsonString x) = x
  
newtype UserID =
  UserID String
  deriving (Show)

data ProcedureID =
  ProcedureID [String]
  deriving (Eq, Ord, Show)

data ProcedureRegister =
  ProcedureRegister (M.Map ProcedureID Procedure)

execute ::
     ProcedureRegister -> ProcedureID -> Parameters -> Maybe (IO JsonString)
execute (ProcedureRegister r) path params = do
  procedure <- M.lookup path r
  return $ procedure params
