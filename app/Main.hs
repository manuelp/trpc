{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Map as M
import RPC
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
  
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

-- main :: IO ()
-- main = do
--   case res of
--     Just action -> fmap show action >>= putStrLn
--     Nothing -> putStrLn "Procedure not found!"
--   where
--     res = execute register (ProcedureID ["demo", "concat"]) dummyInput

app :: Application
app _ respond = do
    putStrLn "I've done some IO here"
    respond $ responseLBS
        status200
        [("Content-Type", "text/plain")]
        "Hello, Web!"  

main :: IO ()
main = do
    putStrLn $ "http://localhost:8080/"
    run 8080 app
