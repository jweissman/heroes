module Hero where
    import CharacterSheet
    import BasicNames
    import FantasyName
    import FantasyProfession
    import FantasyRace
    import Alignment
    import StringHelpers
    
    data Hero = Hero { heroName :: String
                     , sheet :: CharacterSheet
		     , hometown :: String }
      deriving (Eq, Show, Read)


    --data CityScale = Hamlet | Village | Town | MajorCity | Metropolis
    --data City = City { cityName :: String, scale :: CityScale, species :: FantasyRace, descriptor :: String }

    --genCity = do
    --  n        <- genBasicName
    --  citySpecies <- randomIO :: FantasyRace
    --  cityScale   <- randomIO :: CityScale
    --  cityAdj     <- pickFrom adjectives
    --  return City { cityName = n, scale = cityScale, species = citySpecies, descriptor = cityAdj }

    --describeCity city = "the " ++ descriptor city ++ " " ++ scale city ++ " of " ++ cityName city

    -- generate random named hero
    genHero :: IO Hero
    genHero = do
      characterSheet <- genCharacterSheet
      characterName <- genName characterSheet
      city <- genBasicName

      return Hero { heroName = characterName, sheet = characterSheet, hometown = capWord city }

    characterSynopsis hero = let s = humanizedRace (race (sheet hero))
			         j = humanizedJob (job (sheet hero))
			         a = humanizedAlignment (alignment (sheet hero))
                             in s ++ " " ++ j ++ " " ++ a 

    characterBio hero = "The " ++ characterSynopsis hero ++ " from " ++ hometown hero ++ "." --city --cityDescription where cityDescription = describeCity (city hero)
      
    displayHero :: Hero -> String
    displayHero hero = hr ++ "\n  " ++ n ++ "\n\n        " ++ bio ++ "\n\n" ++ description ++ "\n\n"
      where bio = characterBio hero
	    description = characterDescription (sheet hero)
	    n = heroName hero
