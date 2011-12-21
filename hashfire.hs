import Control.Monad (join)

import Data.Object
import qualified Data.Object.Json as JSON

import Network.HTTP.Enumerator
import Network.HTTP.Types

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import qualified Codec.Binary.Base64.String as B64

import System.Environment (getArgs)

domain = "your domain goes here"
apikey = "your API key goes here"

postURL :: Integer -> String
postURL room = "https://" ++ domain ++ ".campfirenow.com/room/" ++ show room ++ "/speak.json"

message :: String -> B.ByteString
message text = JSON.encode $ Mapping [
    ("message", Mapping [
      ("type", Scalar "PasteMessage"),
      ("body", Scalar text)
    ])
  ]

headers :: [Header]
headers = [contentTypeHeader "application/json", authHeader apikey]
  where
    contentTypeHeader = headerContentType . B.pack
    authHeader = headerAuthorization . B.pack . basicAuth
    basicAuth user = "Basic " ++ B64.encode (user ++ ":X")

getResponseMessage :: Response -> IO String
getResponseMessage res = JSON.decode (convert res) >>= fromMapping
    >>= lookupMapping "message" >>= lookupScalar "body"
  where
    convert = B.concat . L.toChunks . responseBody

parseArgs :: IO (Integer, String)
parseArgs = do
    args <- getArgs
    checkArgs args

    msg <- getMessage (last args)

    let room = read (head args)
    return (room, msg)
  where
    checkArgs args | length args == 2 = return ()
                   | otherwise        = error "Usage: bashfire <roomID> <message>"
    getMessage "-" = getContents
    getMessage msg = return msg

main = do
  (room, msg) <- parseArgs

  req0 <- parseUrl $ postURL room

  let req = req0 {
    method = methodPost,
    requestHeaders = headers,
    requestBody = RequestBodyBS (message msg)
  }

  res <- withManager $ httpLbs req
  msg <- getResponseMessage res

  putStrLn "------------------"
  putStrLn msg
  putStrLn "------------------"
