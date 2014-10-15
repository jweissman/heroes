import Control.Monad
import CharacterSheet
import Hero

main :: IO ()
main = forever $ do
    hero <- genHero
    putStrLn $ name hero
    putStrLn $ displayCharacterSheet (sheet hero)
