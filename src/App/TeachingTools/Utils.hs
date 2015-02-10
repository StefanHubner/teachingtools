module TeachingTools.Utils where 

import Database.HDBC
import Data.List

getStandaloneLatexTable :: String -> Bool -> [[String]] -> String 
getStandaloneLatexTable format emptycol l = latexPreamble ++ latexHeader ++ unlines (map latexLine l) ++ latexFooter
	where
		latexPreamble = "\\documentclass[a4paper,12pt]{article}\\usepackage{longtable}\\usepackage{nopageno}\\usepackage{setspace}\\usepackage[margin=1cm]{geometry}\\doublespacing\n"
		latexHeader = "\\begin{document}\n\\begin{longtable}{" ++ format ++ "}\\hline\n"
		latexFooter = "\\end{longtable}\\end{document}\n"
		--latexLine = foldr (\e a -> e ++ " & " ++ a)  "\\\\ \\hline"
		latexLine line = intercalate " & " line ++ (if emptycol then " & " else "") ++ "\\\\ \\hline"

parseResultAsStringList :: [[(String, SqlValue)]] -> [[String]] 
parseResultAsStringList = map (convRow . map snd) 
	where	
		convRow = foldr (\ x -> (++) [fromSql x :: String]) []

formatResult :: Integer -> String
formatResult rowsChanged = show rowsChanged ++ " change(s) made."

