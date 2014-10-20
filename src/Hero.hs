module Hero where
    import Nicknames
    import Data.Char
    import System.Random
    import City
    import Probability
    import CharacterSheet
    import BasicNames
    import FantasyName
    import FantasyProfession
    import FantasyRace
    import Alignment
    import StringHelpers
    
    data Hero = Hero { forename :: String
		     , surname :: String
		     , nickname :: String
                     , sheet :: CharacterSheet
		     , hometown :: City }
      deriving (Eq, Show, Read)

    -- generate random named hero
    genHero :: IO Hero
    genHero = do
      characterSheet <- genCharacterSheet
      --characterName <- genName characterSheet
      f <- genBasicName
      s <- genBasicName
      n <- pickFrom (nicknameGuesses characterSheet)
      city <- genCity
      return Hero { forename = f, surname = s, nickname = n, sheet = characterSheet, hometown = city }

    heroName (Hero {forename = f, surname = s, nickname = n}) = (capWord f ++ " " ++ capWord s ++ " the " ++ capWord n)

    characterBio hero =  "\n\n   " ++ capWord (forename hero) ++ " the " ++ humanizedRace (race (sheet hero)) ++ " and " ++ a ++ " works as " ++  j ++ " and is from " ++ cityDescription 
      where cityDescription = describeCity (hometown hero)
            a = map toLower $ humanizedAlignment (alignment (sheet hero))
	    j = map toLower $ humanizedJob (job (sheet hero))
      
    displayHero :: Hero -> String
    displayHero hero = hr ++ "\n  " ++ n ++ bio ++ "\n\n" ++ description ++ "\n\n"
      where bio = characterBio hero
	    description = characterDescription (sheet hero)
	    n = heroName hero
