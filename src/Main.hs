module Main where

    import System.IO

    main = do
        handle <- openFile "E:\\soft\\vcs\\hg\\source\\naiveBayesClassifier\\src\\resources\\glass.txt" ReadMode
        contents <- hGetContents handle
        putStr (unlines (groupBy contents '\n' []))
        hClose handle


    groupBy :: String -> Char -> String -> [String]
    groupBy "" _ "" = []
    groupBy "" _ r  = [r]
    groupBy (x:xs) c ""
        | x == c        = groupBy xs c ""
        | otherwise     = groupBy xs c [x]
    groupBy (x:xs) c r
        | x == c        = r : groupBy xs c ""
        | otherwise     = groupBy xs c (r ++ [x])
