module City where  
    import Control.Monad
    import StringHelpers
    import System.Random
    import FantasyRace
    import BasicNames
    import Probability
    import Geography

    data CityScale = Hamlet | Village | Town | MajorCity | Metropolis
      deriving (Eq, Show, Read, Enum, Bounded)

    describeCityScale Hamlet     = "hamlet"
    describeCityScale Village    = "village"
    describeCityScale Town       = "town"
    describeCityScale MajorCity  = "city"
    describeCityScale Metropolis = "metropolis"

    --data NarrativeElement = Concept | Person | Place | Thing
    --data Aspect = Aspect { aspectName :: String, aspectType :: AspectType, baseElement :: NarrativeElement }
    --data EventType = MilitaryEvent | EconomicEvent | SocialEvent | TechnologicalEvent | ScientificEvent | PoliticalEvent
    --data Event = Event EventType [Aspect]
    --data Epoch = Epoch [Event]
    --data Period = Period [Epoch]
    --data History = History [Period]
    --data Civilization
    --data People = People { peopleName :: String, peopleDescriptor :: String, peopleHistory :: History }
   
    data City = City { region :: Region, cityName :: String, scale :: CityScale, descriptor :: String }
      deriving (Eq, Show, Read)

    genCity :: IO City
    genCity = do
      sc  <- randomIO :: IO CityScale
      --population <- sequence $ replicate popCount genPerson where popCount = popSize sc
      n   <- genBasicName
      --eth <- genRace
      rgn <- genRegion
      a   <- pickFrom adjectives
      return (City { cityName = n, scale = sc, descriptor = a, region = rgn })
      
    describeCity city = "the " ++ d ++ " " ++ sc ++ " of " ++ n ++ " in " ++ rgn
      where d = (descriptor city)
            --sp = humanizedRace (populationSpecies city)
            sc = describeCityScale (scale city)
            n = capWord (cityName city)
	    rgn = describeRegion (region city)

