module StringHelpers where
    import Data.Char as Char

--    capWord :: [Char] -> [Char]
    capWord [] = []
    capWord (h:t) = Char.toUpper h : map Char.toLower t

--    capWords :: [Char] -> [Char]
    capWords [] = []
    capWords (h:t) = capWord h : map capWord t

    hr :: String
    hr = "=====================================================================\n"
