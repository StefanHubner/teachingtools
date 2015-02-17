{-# LANGUAGE OverloadedStrings #-}
import           Control.Exception        (SomeException)
import           Control.Exception.Lifted (handle)
import           Control.Monad.IO.Class   (liftIO)
import           Data.Aeson               
import           Data.Conduit             (($$))
import           Data.Conduit.Attoparsec  (sinkParser)
import           Network.HTTP.Types       (status200, status400)
import           Network.Wai              (Application, Response, responseLBS)
import           Network.Wai.Conduit      (sourceRequestBody)
import           Network.Wai.Handler.Warp (run)
import 			 TeachingTools
import Control.Monad
import Control.Applicative

data TCourse = TCourse { courseid :: Integer, name :: String } deriving (Show)
data TRequest = TRequest { requestID :: String, parameters :: [String] }

instance FromJSON TRequest where
	parseJSON (Object v) = TRequest  <$> v .: "requestID" <*> v .: "parameters"
	parseJSON _ 		 = mzero

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
	| r == Success "GET_ALL_COURSES"   = liftM ( (\v ->object ["Courses" .= v]) . toJSON . map (toJSON . makeCourse)) getMyCourses
	| otherwise 				       = return $ toJSON 'x' 
	where 
		r = liftM requestID (fromJSON request :: Result TRequest)
		makeCourse :: [String] -> TCourse
		makeCourse l = TCourse (read (head l) :: Integer) (foldl1 (++) (tail l)) 
