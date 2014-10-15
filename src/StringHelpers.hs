module StringHelpers where
    import Data.Char as Char

    capWord [] = []
    capWord (h:t) = Char.toUpper h : map Char.toLower t

    capWords [] = []
    capWords (h:t) = capWord h : map capWord t