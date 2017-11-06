module Main where

import qualified Data.Map as M
import RPC
  
getParams :: M.Map String String -> Maybe (String, String)
getParams m = do
  x <- M.lookup "x" m
  y <- M.lookup "y" m
  return $ (x, y)

concatProcedure :: Procedure
concatProcedure =
  \(Parameters _ m) ->
    case (getParams m) of
      Just (x, y) -> return $ JsonString (x ++ y)
      otherwise -> fail "Missing parameters!"

register :: ProcedureRegister
register =
  ProcedureRegister $
  M.fromList [(ProcedureID ["demo", "concat"], concatProcedure)]

dummyInput :: Parameters
dummyInput =
  Parameters (UserID "demo") (M.fromList [("x", "Hello, "), ("y", "RPC!")])

main :: IO ()
main = do
  case res of
    Just action -> fmap show action >>= putStrLn
    Nothing -> putStrLn "Procedure not found!"
  where
    res = execute register (ProcedureID ["demo", "concat"]) dummyInput
