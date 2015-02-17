module TeachingTools.Settings where

import Database.HDBC.MySQL

data Settings = Settings { 
	dbfile :: String, 
	tmpdir :: String, 
	pdflatex :: String, 
	pdfreader :: String, 
	printapp :: String, 
	mysqldb :: MySQLConnectInfo,
	redirect :: Bool } 

settings :: Settings
settings = Settings "/home/u1239326/uvthome/teaching/teaching.db" "/tmp/" "/usr/bin/pdflatex" "/usr/bin/evince" "/usr/bin/lpr" (defaultMySQLConnectInfo {mysqlHost = "hubner.info", mysqlUser = "ttuser", mysqlDatabase = "TeachingTools", mysqlPassword = "tt123" }) True 
