{-
    Файл 1:
    имя отчесво фамилия адрес
    или
    имя фамилия адрес
    
    Файл 2:
    адрес аська
    
    
    Запускается:
    Вариант 1:
        Загружаешь файл и вызываешь generateHTML с путями к файлам.
    Вариант 2:
        В командной строке пишешь:
            runhaskell lab5.hs <путь к первому файлу> <путь ко второму файлу>
-}

-- Модуль с функциями для списков
import Data.List
-- Модуль с функцией getArgs, возвращающей аргупенты коммандной строки.
import System.Environment

-- Проверка на email 
isEmail :: String -> Bool
isEmail s = if elem '@' s && elem '.' s then {-Проверка первого уровня, есть @ и точка домена-}
                let {-Проверки второго уровня-}
                    checkDogCount d = length d == 1 {-@ должна быть одна-}
                    checkDogPos d = d /= 0 {-@ не может быть первым символом адреса-}
                    checkPointsCount p = length p > 0 {-Должна быть как минимум одна точка-}
                    checkPointsPos _ [] = True {-Проверка всех найденных точек на правильность размещения-}
                    checkPointsPos d (p:px) = if (p == 0) || (d == p - 1) then False else checkPointsPos d px
                    checkLastPoint p l = (p == l - 3) || (p == l - 4) {-Проверка доменной точки-}
                    {-Здесь объединяются все вторичные проверки-}
                    check d p l = if checkDogCount d && checkDogPos (d!!0) && checkPointsCount p then
                        if checkPointsPos (d!!0) p && checkLastPoint (last p) l then
                            True
                         else
                            False
                     else
                        False
                in
                    {-Вызываем вторичную проверку, передавая все найденные @, точки и длину адреса-}
                    check (elemIndices '@' s) (elemIndices '.' s) (length s)
             else
                False
-- Формируем список (Имя, Адрес)
nameList :: String -> [(String, String)]
nameList s = let
                compil [] _ acc = acc
                {-Фиксируем пару Имя-Адрес только при нахождении адреса-}
                compil (w:wx) accname acc = if isEmail w then compil wx [] {-На первое место ставим фамилию-}(((unwords ((last accname):(take (length accname - 1) accname))), w):acc) else compil wx (accname ++ [w]) acc                
             in
                compil (words s) [] []

-- Формируем список (Адрес, ICQ)
icqList :: String -> [(String, Int)]
icqList s = let
                compil [] _ acc = acc
                {-При встрече адреса фиксируем пару, пытаемся привести номер в числовое представление-}
                {-Если не число, программа крашнется-}
                compil (w:wx) addr acc = if isEmail w then compil wx w acc else compil wx [] ((addr, (read w :: Int)):acc)                
             in
                compil (words s) [] []

-- Форматируем наши списки, генерируя список HTML
formatNames :: [(String, String)] -> [(String, Int)] -> String
formatNames nx ix = let
                        findICQ _ [] = 0
                        {-Ищем аську по адресу-}
                        findICQ addr (i:ix) = if addr == fst i then snd i else findICQ addr ix
                        list [] ix acc = acc
                        {-Последовательно формируем список, если есть аська, добавляем-}
                        list (n:nx) ix acc = list nx ix (acc ++ "<li>" ++ (fst n) ++ ", <a href=\"mailto:" ++ (snd n) ++ "\">" ++ (snd n) ++ "</a>" ++ (if findICQ (snd n) ix /= 0 then ", ICQ: " ++ show (findICQ (snd n) ix) else "") ++ ".</li>\n")
                    in
                        {-Имена сортируются по фамилии-}
                        "<ul>\n" ++ list (sort nx) ix [] ++ "</ul>\n"

--Генерируем HTML и записываем в файл с расширением .htm
generateHTML :: String -> String -> IO ()
generateHTML file_names file_icqs = do
    names <- readFile (file_names) {-Читаем список имён из файла 1-}
    icqs <- readFile (file_icqs) {-Читаем список имён из файла 2-}
    let formated = formatNames (nameList names) (icqList icqs) {-Формируем HTML список-}
    {-Встраиваем список в шаблон HTML страницы-}
    let html = "<!DOCTYPE html>\n<html lang=\"ru\">\n<head>\n<title>Список имён</title>\n</head>\n<body>\n" ++ formated ++ "</body>"
    {-Находим имя для нового файла-}
    {-Если у файла было расширение, земеняем-}
    {-Если не было, просто добавляем-}
    let new_file = if elem '.' (file_names) then take (last (elemIndices '.' (file_names))) (file_names) ++ ".htm" else (file_names) ++ ".htm"
    writeFile new_file html {-Записываем в файл нашу страницу-}

main = do
    args <- getArgs {-Запрашиваем аргументы из коммандной строки-}
    if length args < 2 then do {-Если их мало, сообщаем пользователю-}
        putStrLn "Необходимые параметры: <Файл с именами> <Файл с ICQ>"
     else do {-Иначе вызываем генератор HTML-}
        generateHTML (args!!0) (args!!1)
