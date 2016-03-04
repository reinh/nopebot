{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Lib
    ( startApp

    ) where

import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy     as BS
import           Data.Maybe               (fromMaybe)
import           Data.Monoid              ((<>))
import           Data.Text                (Text)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.Environment       (getEnv)
import           System.Random.Shuffle    (shuffleM)

nopes =
    [ "https://media.giphy.com/media/gX0wdXBYL8Ius/giphy.gif"
    , "http://i.imgur.com/I8kq0uJ.gif"
    , "http://i.imgur.com/n3RGs0O.gif"
    , "http://i.imgur.com/hXGtYfh.gif"
    , "http://i.imgur.com/Sj6HN.gif"
    , "http://i.imgur.com/fFUjGje.gif"
    , "http://i.imgur.com/GPqtwr7.gif"
    , "http://i.imgur.com/NpAbl.gif" ]

data SlashRequest = SlashRequest
  { token       :: Text
  , teamId      :: Text
  , teamDomain  :: Text
  , channelId   :: Text
  , channelName :: Text
  , userId      :: Text
  , userName    :: Text
  , command     :: Text
  , text        :: Text
  , responseUrl :: Text
  } deriving (Show)

instance FromFormUrlEncoded SlashRequest where
    fromFormUrlEncoded form =
        SlashRequest
          <$> field "token"
          <*> field "team_id"
          <*> field "team_domain"
          <*> field "channel_id"
          <*> field "channel_name"
          <*> field "user_id"
          <*> field "user_name"
          <*> field "command"
          <*> field "text"
          <*> field "response_url"
      where
          field :: Text -> Either String Text
          field name = note (show name) (lookup name form)

data SlashResponse = SlashResponse
    { responseType :: ResponseType
    , responseText :: Text
    } deriving (Show)

instance ToJSON SlashResponse where
    toJSON (SlashResponse typ img) =
        object [ "response_type" .= typ
               , "attachments" .= [attachment]]
      where
        attachment =
            object
                [ "title" .= ("Nope." :: String)
                , "image_url" .= img
                , "color" .= ("danger" :: String)]

data ResponseType = Ephemeral | InChannel deriving (Show)

instance ToJSON ResponseType where
    toJSON Ephemeral = "ephemeral"
    toJSON InChannel = "in_channel"

type API = "nope" :> ReqBody '[FormUrlEncoded] SlashRequest :> Post '[JSON] SlashResponse

startApp :: IO ()
startApp = do
    port <- read <$> getEnv "PORT"
    putStrLn $ "Started app on port " ++ show port
    run port app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server nope = do
    gif <- head <$> liftIO (shuffleM nopes)
    let res = SlashResponse InChannel gif
    logRequest nope res
    return res

logRequest nope res =
    liftIO $ do
       putStrLn "Request:"
       print nope
       putStrLn "Response:"
       BS.putStr (encodePretty res)
       putStrLn "\n"

-- Utils

note :: String -> Maybe a -> Either String a
note s = maybe (Left s) Right
