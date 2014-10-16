import Control.Monad
import CharacterSheet
import Hero

main :: IO ()
main = forever $ do
    hero <- genHero
    putStrLn $ displayHero hero
    getLine
