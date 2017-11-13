{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Map as M
import Data.Maybe
import RPC
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Text.Encoding (encodeUtf8)

main :: IO ()
main = do
    putStrLn $ "http://localhost:8080/"
    run 8080 app

-- https://www.stackage.org/haddock/lts-9.12/wai-3.2.1.1/Network-Wai.html#t:Application
--
-- type Application
--    = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app :: Application
app request respond = do
  putStrLn . show . queryString $ request
  putStrLn . show $ path
  putStrLn . show $ params
  fromMaybe (return missingProcedure) res >>= respond
  where
    path = ProcedureID . map encodeUtf8 . pathInfo $ request
    params = getParameters (UserID "test") . queryString $ request
    res :: Maybe (IO Response)
    res = execute register path params

missingProcedure :: Response
missingProcedure =
  responseLBS
    status404
    [("Content-Type", "text/plain")]
    (LBS.pack "Unknown procedure")

-- https://www.stackage.org/haddock/lts-9.12/http-types-0.9.1/Network-HTTP-Types-URI.html#t:Query
--
-- type Query = [QueryItem]
-- type QueryItem = (ByteString, Maybe ByteString) 
getParameters :: UserID -> Query -> Parameters
getParameters u q =
  let f (x, Just y) = Just (x, y)
      f _ = Nothing
      params = concat $ map (maybeToList . f) q
  in Parameters u $ M.fromList params

--
-- ----------------------------------------
--

register :: ProcedureRegister
register =
  ProcedureRegister $
  M.fromList [(ProcedureID ["demo", "concat"], concatProcedure)]

concatProcedure :: Procedure
concatProcedure =
  \(Parameters _ m) ->
    case (getParams m) of
      Just (x, y) ->
        return $
        responseLBS
          status200
          [("Content-Type", "text/plain")]
          (LBS.append (LBS.fromStrict x) (LBS.fromStrict y))
      otherwise -> fail "Missing parameters!"
  where
    getParams m = do
      x <- M.lookup "x" m
      y <- M.lookup "y" m
      return $ (x, y)
