{-# LANGUAGE OverloadedStrings #-}
import           Control.Exception        (SomeException)
import           Control.Exception.Lifted (handle)
import           Control.Monad.IO.Class   (liftIO)
import           Data.Aeson               (Value, encode, object, (.=), ToJSON, toJSON)
import           Data.Aeson.Parser        (json)
import           Data.Conduit             (($$))
import           Data.Conduit.Attoparsec  (sinkParser)
import           Network.HTTP.Types       (status200, status400)
import           Network.Wai              (Application, Response, responseLBS)
import           Network.Wai.Conduit      (sourceRequestBody)
import           Network.Wai.Handler.Warp (run)
import 			 TeachingTools
import Control.Monad

data TCourse = TCourse { courseid :: Integer, name :: String } deriving (Show)

instance ToJSON TCourse where
	toJSON (TCourse c n) = object ["ID" .= c, "Name" .= n]

main :: IO ()
main = run 3002 app

app :: Application
app req sendResponse = handle (sendResponse . invalidJson) $ do
    value <- sourceRequestBody req $$ sinkParser json
    newValue <- liftIO $ modValue value
    sendResponse $ responseLBS
        status200
        [("Content-Type", "application/json")]
        $ encode newValue

invalidJson :: SomeException -> Response
invalidJson ex = responseLBS
    status400
    [("Content-Type", "application/json")]
    $ encode $ object
        [ "message" .= show ex
        ]

-- Application-specific logic would go here.
modValue :: Value -> IO Value
modValue request 
	| parseRequest request == "GET_ALL_COURSES" = liftM ( (\v ->object ["Courses" .= v]) . toJSON . map (toJSON . makeCourse)) getMyCourses
	where 
		makeCourse :: [String] -> TCourse
		makeCourse l = TCourse (read (head l) :: Integer) (foldl1 (++) (tail l)) 

parseRequest :: Value -> String
parseRequest _ = "GET_ALL_COURSES"
