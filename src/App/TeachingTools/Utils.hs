module TeachingTools.Utils where 

import Database.HDBC

getStandaloneLatexTable :: String -> [[String]] -> String 
getStandaloneLatexTable format l = latexPreamble ++ latexHeader ++ unlines (map latexLine l) ++ latexFooter
	where
		latexPreamble = "\\documentclass[a4paper,12pt]{article}\\usepackage{nopageno}\\usepackage{setspace}\\usepackage[margin=1cm]{geometry}\\doublespacing\n"
		latexHeader = "\\begin{document}\n\\begin{tabular}{" ++ format ++ "}\\hline\n"
		latexFooter = "\\end{tabular}\\end{document}\n"
		latexLine = foldr (\e a -> e ++ " & " ++ a)  "\\\\ \\hline"

parseResultAsStringList :: [[(String, SqlValue)]] -> [[String]] 
parseResultAsStringList = map (convRow . map snd) 
	where	
		convRow = foldr (\ x -> (++) [fromSql x :: String]) []

formatResult :: Integer -> String
formatResult rowsChanged = show rowsChanged ++ " change(s) made."

