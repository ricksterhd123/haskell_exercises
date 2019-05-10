--
-- MATHFUN
-- Template for the Haskell assignment program (replace this comment)
-- Add your student number
--

--import           words
import           Data.Char  (isDigit)
import           Data.List  (intercalate, take)
import           Data.Maybe (fromJust, isNothing)
import           Text.Read  (readMaybe)
--
-- Types
--

-- Define Album type here
newtype Album = Hit (String, String, Int, Int)
    deriving (Eq)

-- useful functions



testData :: [Album]
testData = [
    Hit ("Greatest Hits"                         ,"Queen"               ,1981   ,6300000),
    Hit ("Gold: Greatest Hits"                   ,"ABBA"                ,1992   ,5400000),
    Hit ("Sgt. Pepper's Lonely Hearts Club Band" ,"The Beatles"         ,1967   ,5340000),
    Hit ("21"                                    ,"Adele"               ,2011   ,5110000),
    Hit ("(What's the Story) Morning Glory?"     ,"Oasis"               ,1995   ,4940000),
    Hit ("Thriller"                              ,"Michael Jackson"     ,1982   ,4470000),
    Hit ("The Dark Side of the Moon"             ,"Pink Floyd"          ,1973   ,4470000),
    Hit ("Brothers in Arms"                      ,"Dire Straits"        ,1985   ,4350000),
    Hit ("Bad"                                   ,"Michael Jackson"     ,1987   ,4140000),
    Hit ("Rumours"                               ,"Fleetwood Mac"       ,1977   ,4090000),
    Hit ("Greatest Hits II"                      ,"Queen"               ,1991   ,3990000),
    Hit ("Back to Black"                         ,"Amy Winehouse"       ,2006   ,3940000),
    Hit ("The Immaculate Collection"             ,"Madonna"             ,1990   ,3700000),
    Hit ("25"                                    ,"Adele"               ,2015   ,3500000),
    Hit ("Stars"                                 ,"Simply Red"          ,1991   ,3450000),
    Hit ("Come On Over"                          ,"Shania Twain"        ,1998   ,3430000),
    Hit ("x"                                     ,"Ed Sheeran"          ,2014   ,3380000),
    Hit ("Legend"                                ,"Bob Marley"          ,1984   ,3380000),
    Hit ("Bat Out of Hell"                       ,"Meat Loaf"           ,1977   ,3370000),
    Hit ("Back to Bedlam"                        ,"James Blunt"         ,2004   ,3360000),
    Hit ("Urban Hymns"                           ,"The Verve"           ,1997   ,3340000),
    Hit ("Bridge over Troubled Water"            ,"Simon & Garfunkel"   ,1970   ,3260000),
    Hit ("1"                                     ,"The Beatles"         ,2000   ,3230000),
    Hit ("Spirit"                                ,"Leona Lewis"         ,2007   ,3170000),
    Hit ("Crazy Love"                            ,"Michael Bublé"       ,2009   ,3130000),
    Hit ("No Angel"                              ,"Dido"                ,2000   ,3090000),
    Hit ("White Ladder"                          ,"David Gray"          ,1998   ,3020000),
    Hit ("The Fame"                              ,"Lady Gaga"           ,2009   ,2990000),
    Hit ("Only by the Night"                     ,"Kings of Leon"       ,2008   ,2980000),
    Hit ("A Rush of Blood to the Head"           ,"Coldplay"            ,2002   ,2960000),
    Hit ("Talk on Corners"                       ,"The Corrs"           ,1997   ,2960000),
    Hit ("Spice"                                 ,"Spice Girls"         ,1996   ,2960000),
    Hit ("Life for Rent"                         ,"Dido"                ,2003   ,2900000),
    Hit ("Beautiful World"                       ,"Take That"           ,2006   ,2880000),
    Hit ("The Joshua Tree"                       ,"U2"                  ,1987   ,2880000),
    Hit ("Hopes and Fears"                       ,"Keane"               ,2004   ,2860000),
    Hit ("The War of the Worlds"                 ,"Jeff Wayne"          ,1978   ,2800000),
    Hit ("X&Y"                                   ,"Coldplay"            ,2005   ,2790000),
    Hit ("Jagged Little Pill"                    ,"Alanis Morissette"   ,1995   ,2780000),
    Hit ("Tubular Bells"                         ,"Mike Oldfield"       ,1973   ,2760000),
    Hit ("Scissor Sisters"                       ,"Scissor Sisters"     ,2004   ,2760000),
    Hit ("...But Seriously"                      ,"Phil Collins"        ,1989   ,2750000),
    Hit ("Tracy Chapman"                         ,"Tracy Chapman"       ,1988   ,2710000),
    Hit ("Parachutes"                            ,"Coldplay"            ,2000   ,2710000),
    Hit ("The Man Who"                           ,"Travis"              ,1999   ,2687500),
    Hit ("Greatest Hits"                         ,"ABBA"                ,1975   ,2606000),
    Hit ("I've Been Expecting You"               ,"Robbie Williams"     ,1998   ,2586500),
    Hit ("Come Away with Me"                     ,"Norah Jones"         ,2002   ,2556650),
    Hit ("Graceland"                             ,"Paul Simon"          ,1986   ,2500000),
    Hit ("Ladies & Gentlemen: The Best of"       ,"George Michael"      ,1998   ,2500000)]

--
--
--  Your functional code goes here
--
--

------------------------------------------
-- Core functionality [i]
------------------------------------------
-- define an instance of Show type class which allows (show Album) :: Album -> String
instance Show Album where
    show (Hit (title, artist, releaseDate, sales)) = title ++ ", " ++ artist ++ ", " ++ show releaseDate ++ ", " ++ show sales

-- read :: String -> Album
--instance Read Album where
--    read str = splitOn ()
-- map show on all albumbs in the list
-- intercalate adds "\n" => newline on each string at the end.
albumsToString :: [Album] -> String
albumsToString albums = intercalate "\n" (map show albums)


-- descending order take the first 10 elements from the top.
top10 :: [Album] -> [Album]
top10 = take 10

-- filter the list of Albums between an upper and lower releaseDate
-- keeps the list in descending order of sales
betweenTwo :: [Album] -> Int -> Int -> [Album]
betweenTwo albums lower upper
    = filter  isBetweenLowerUpper albums
    where
    isBetweenLowerUpper (Hit (_,_,releaseDate,_)) = releaseDate >= lower && releaseDate <= upper

-- adapted from worksheet 5
-- made polymorphic however for reuseability,
-- however it can work just as fine as:
-- prefix :: [String] -> [String] -> Bool
prefix :: Eq a => [a] -> [a] -> Bool
prefix [] _          = True
prefix _ []          = False
prefix (y:ys) (x:xs) = y == x && prefix ys xs

-- Get a list of albums with a title matching the prefix string.
albumsOfTitlePrefix :: [Album] -> String -> [Album]
albumsOfTitlePrefix albums prefixStr
    = filter (\(Hit (title, _, _, _)) -> prefix prefixStr title) albums

-- Get a list of albums with an artist matching the prefix string.
albumsOfArtistPrefix :: [Album] -> String -> [Album]
albumsOfArtistPrefix albums prefixStr
    = filter (\(Hit (_, artist, _, _)) -> prefix prefixStr artist) albums

-- Takes a list of albums and an artist name and returns an integer sum total of sales.
totalAlbumsSalesByArtist :: [Album] -> String -> Int
totalAlbumsSalesByArtist albums artistName
    = foldl (\x (Hit (_, _, _, sales)) -> x + sales) 0 (albumsOfArtistPrefix albums artistName)

-- Get the total number of albums produced by an artist.
totalAlbumsProducedByArtist :: [Album] -> String -> Int
totalAlbumsProducedByArtist albums artistName = foldl (\x _ -> x + 1) 0 (albumsOfArtistPrefix albums artistName)

-- Get a list of all artists
listOfArtists :: [Album] -> [String]
listOfArtists albums = [artist | (Hit (_, artist, _, _)) <- albums]

-- Pair each artist with the number of albums they've produced
pairArtistsNumberOfAlbums :: [Album] -> [(String, Int)]
pairArtistsNumberOfAlbums albums
    = zip (listOfArtists albums) (map (totalAlbumsProducedByArtist albums) (listOfArtists albums))

-- Somehow I've managed to get it working with just foldr
-- I thought that because foldr :: (a -> b -> b) -> b -> [a] -> b
-- applies a function (a -> b -> b) on items in the list [a] and on the value b and returns type b
-- if you subsitute a => Album and b => [Album]
-- it works out nicely because then foldr :: (Album -> [Album] -> [Album]) -> [Album] -> [Album] -> [Album] which fits the type signature
-- and so i made my lambda function simply check if aSales >= bSales and then swap the first Album and the second Album to keep the list in descending order
-- and also to remove the last element in the list I use 'init' which simply takes every element but the last element from the list.
--
-- addTop50Album :: [Album] -> Album -> [Album]
-- addTop50Album albums newAlbum
--     = foldr (\(Hit (aTitle, aArtist, aReleaseDate, aSales)) (Hit (bTitle, bArtist, bReleaseDate, bSales) : xs)
--                 -> if aSales >= bSales then Hit (aTitle, aArtist, aReleaseDate, aSales) : Hit (bTitle, bArtist, bReleaseDate, bSales) : xs
--                    else Hit (bTitle, bArtist, bReleaseDate, bSales) : Hit (aTitle, aArtist, aReleaseDate, aSales) : xs ) [newAlbum] (init albums)

-- Sort the albums into descending order
sortTop50AlbumsDescendingSales :: [Album] -> [Album]
sortTop50AlbumsDescendingSales albums
    = foldr (\(Hit (aTitle, aArtist, aReleaseDate, aSales)) (Hit (bTitle, bArtist, bReleaseDate, bSales) : xs)
                -> if aSales >= bSales then Hit (aTitle, aArtist, aReleaseDate, aSales) : Hit (bTitle, bArtist, bReleaseDate, bSales) : xs
                   else Hit (bTitle, bArtist, bReleaseDate, bSales) : Hit (aTitle, aArtist, aReleaseDate, aSales) : xs ) [head albums] (tail albums)

-- Add the new album to the front of the 'init' of the list and sort it in descending order by sales.
addTop50Album :: [Album] -> Album -> [Album]
addTop50Album albums newAlbum
    = sortTop50AlbumsDescendingSales (newAlbum:init albums)

-- Get the first album which matches the title or artist name
-- the logic of this function works because there should be no two albums with the same title and artistName
getAlbum :: [Album] -> String -> String -> Maybe Album
getAlbum albums title artist
    | not (null matchedAlbums) = Just (head matchedAlbums)
    | otherwise = Nothing
    where
    matchedAlbums = albumsOfArtistPrefix (albumsOfTitlePrefix albums title) artist

-- increase the album sales by addSales if both albums are equal.
increaseSalesIfEq :: Int -> Maybe Album -> Album -> Album
increaseSalesIfEq addSales album (Hit (bTitle, bArtist, bReleaseDate, bSales))
    | album == Just (Hit (bTitle, bArtist, bReleaseDate, bSales)) = Hit (bTitle, bArtist, bReleaseDate, bSales + addSales)
    | otherwise = Hit (bTitle, bArtist, bReleaseDate, bSales)

--  increase the sales figure for one of the albums given its title & artist and the additional
-- sales, possibly changing the album’s position in the list (if no album with the given
-- details exists, the function should do nothing)
increaseAlbumSales :: [Album] -> String -> String -> Int -> [Album]
increaseAlbumSales albums title artist addSales
    = sortTop50AlbumsDescendingSales (map (increaseSalesIfEq addSales (getAlbum albums title artist)) albums)


-- Demo function to test basic functionality (without persistence - i.e.
-- testData doesn't change and nothing is saved/loaded to/from albums file).
demo :: Int -> IO ()

--demo 1
demo 1 = putStrLn (albumsToString testData)
demo 2 = putStrLn (albumsToString (top10 testData))
demo 3 = putStrLn (albumsToString (betweenTwo testData 2000 2008))
demo 4 = putStrLn (albumsToString (albumsOfTitlePrefix testData "Th"))
demo 5 = print (totalAlbumsSalesByArtist testData "Queen")
demo 6 = putStrLn (intercalate "\n" (map show (pairArtistsNumberOfAlbums testData)))
demo 7 = putStrLn (albumsToString (addTop50Album testData (Hit ("Progress", "Take That", 2010, 2700000)) ))
demo 8 = putStrLn (albumsToString (increaseAlbumSales testData "21" "Adele" 400000))
demo _ = putStrLn "\n\n\n No functionality \n\n\n"


-- main :: Int -> IO ()
--
--
-- main 0      = do putStrLn "+++++++++++++++++++++++++++++++++++++++++++++++"
--                  putStrLn "* (0) Display this menu"
--                  putStrLn "* (1) Display albums"
--                  putStrLn "* (2) Give top ten albums"
--                  putStrLn "* (3) Find albums between two given years"
--                  putStrLn "* (4) Search by title"
--                  putStrLn "* (5) Total sales by artist"
--                  putStrLn "* (6) Pair artists by number of albums they made"
--                  putStrLn "* (7) Remove 50th lowest selling "
--                  putStrLn "* (8) Increase sales figure for one of the albums"
--                  putStrLn "+++++++++++++++++++++++++++++++++++++++++++++++"
--                  putStrLn "Please enter a number: "
--                  number <- getInt
--
--                  if number < 0 && number > 8 then main (-1) else main 0
--                  return ()


--main 1
--main 2
--main 3
--main 4
--main 5
--main 6
--main 7
--main 8
-- invalid :: IO ()
-- inavlid = do putStrLn "Please try again..."
--                  return ()
--
-- main _      = do putStrLn "TODO"
--
--
-- Your user interface (and loading/saving) code goes here
--

-- readAlbumLn :: IO (Maybe Album)
-- readAlbumLn
--     = do str <- getLine
--     where
--     [title, artist, releaseDateStr, salesStr] = words (str)
--     releaseDate = readMaybe releaseDateStr
--     sales    = readMaybe salesStr
--
--     if isNothing ( Just releaseDate) || isNothing( Just sales) then
--         return Nothing
--     else return Just (Hit (title, artist, fromJust releaseDate, fromJust sales))

-- readAlbumFile :: String -> IO()
-- readAlbumFile path = do file <- readFile path
--                         return ()
--                 -- let fileLines = lines (file :: String)
--                 -- --putStrLn "* Reading file..."
--                 -- return map (fromJust readAlbumLn) fileLines
--
--
-- -- check if string is a number
-- isNumber :: String -> Bool
-- isNumber []        = True
-- isNumber (chr:str) = isDigit chr && isNumber str
--
-- getInt :: IO Int
-- getInt = do input <- getLine
--             let isDigit = isNumber input
--             if isDigit then
--                 return (read input :: Int)  --
--             else
--                 return (-1) -- return -1 to represent an error

