module TeachingTools.Settings where

data Settings = Settings { dbfile :: String, tmpdir :: String, pdflatex :: String, pdfreader :: String, printapp :: String, redirect :: Bool } deriving (Show)
settings :: Settings
settings = Settings "/home/u1239326/uvthome/teaching/teaching.db" "/tmp/" "/usr/bin/pdflatex" "/usr/bin/evince" "/usr/bin/lpr" True 
