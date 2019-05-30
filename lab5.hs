{-
    Файл 1:
    имя отчесво фамилия адрес
    или
    имя фамилия адрес
    
    Файл 2:
    адрес аська
-}

import Data.List
import System.Environment

isEmail :: String -> Bool
isEmail s = if elem '@' s && elem '.' s then
                let
                    checkDogCount d = length d == 1
                    checkDogPos d = d /= 0
                    checkPointsCount p = length p > 0
                    checkPointsPos _ [] = True
                    checkPointsPos d (p:px) = if (p == 0) || (d == p - 1) then False else checkPointsPos d px
                    checkLastPoint p l = (p == l - 3) || (p == l - 4)
                    check d p l = if checkDogCount d && checkDogPos (d!!0) && checkPointsCount p then
                        if checkPointsPos (d!!0) p && checkLastPoint (last p) l then
                            True
                         else
                            False
                     else
                        False
                in
                    check (elemIndices '@' s) (elemIndices '.' s) (length s)
             else
                False

nameList :: String -> [(String, String)]
nameList s = let
                compil [] _ acc = acc
                compil (w:wx) accname acc = if isEmail w then compil wx [] (((unwords ((last accname):(take (length accname - 1) accname))), w):acc) else compil wx (accname ++ [w]) acc                
             in
                compil (words s) [] []

icqList :: String -> [(String, Int)]
icqList s = let
                compil [] _ acc = acc
                compil (w:wx) addr acc = if isEmail w then compil wx w acc else compil wx [] ((addr, (read w :: Int)):acc)                
             in
                compil (words s) [] []

formatNames :: [(String, String)] -> [(String, Int)] -> String
formatNames nx ix = let
                        findICQ _ [] = 0
                        findICQ addr (i:ix) = if addr == fst i then snd i else findICQ addr ix
                        list [] ix acc = acc
                        list (n:nx) ix acc = list nx ix (acc ++ "<li>" ++ (fst n) ++ ", <a href=\"mailto:" ++ (snd n) ++ "\">" ++ (snd n) ++ "</a>" ++ (if findICQ (snd n) ix /= 0 then ", ICQ: " ++ show (findICQ (snd n) ix) else "") ++ ".</li>\n")
                    in
                        "<ul>\n" ++ list (sort nx) ix [] ++ "</ul>\n"

generateHTML :: String -> String -> IO ()
generateHTML file_names file_icqs = do
    names <- readFile (file_names)
    icqs <- readFile (file_icqs)
    let formated = formatNames (nameList names) (icqList icqs)
    let html = "<!DOCTYPE html>\n<html lang=\"ru\">\n<head>\n<title>Список имён</title>\n</head>\n<body>\n" ++ formated ++ "</body>"
    let new_file = if elem '.' (file_names) then take (last (elemIndices '.' (file_names))) (file_names) ++ ".htm" else (file_names) ++ ".htm"
    writeFile new_file html

main = do
    args <- getArgs
    if length args < 2 then do
        putStrLn "Необходимые параметры: <Файл с именами> <Файл с ICQ>"
     else do
        generateHTML (args!!0) (args!!1)
