module Geography where  
    import Data.Char
    import Data.List
    import Control.Monad
    import StringHelpers
    import System.Random
    import FantasyRace
    import BasicNames
    import Probability

    data Latitude = Arctic | Boreal | Temperate | Subtropical | Tropical
      deriving (Eq, Show, Read, Enum, Bounded)
    data Humidity = Humid | Semihumid | Semiarid | Arid
      deriving (Eq, Show, Read, Enum, Bounded)

    --data TerrestrialBiome = Grassland | Rainforest | Tundra
    --data AquaticBiome = Freshwater | Marine
    --data Biome = TerrestrialBiome | AquaticBiome

    data EcosystemFeature = Abiotic | Biome
    data LandscapeFeature = Tundra | Plateau | Canyon | Sea | Ocean | River | Lake | Estuary | Hill | Mountain | Holes | Jungle | Cyberspace | Astral --Plateau | Canyon | Sea | Ocean | River 


    data GeographicFeature = EcosystemFeature | LandscapeFeature --
      deriving (Eq, Show, Read, Bounded, Enum)

    --genFeatureClass = do
    --  fc <- randomIO :: IO GeographicFeature

    -- really just need if/then but
    -- not confident about syntax...
    -- i.e., in combo with io!
    genFeature = do
      f <- randomIO :: IO GeographicFeature
      return f

    data LocalGeography = LocalGeography Latitude Humidity [GeographicFeature] --Biome --TerrestrialBiome
      deriving (Eq, Show, Read)
      
    genGeography = do
      lat <- randomIO :: IO Latitude
      hum <- randomIO :: IO Humidity
      fs  <- replicateM 4 genFeature
      return (LocalGeography lat hum fs)

    describeGeography (LocalGeography lat hum fs) = map toLower (show hum) ++ " " ++ map toLower (show lat) -- ++ " with " ++ (intercalate ", " (map show fs))


    --genFeature = do  
    --  feature <- (randomIO :: IO GeographicFeature)
    --  return feature

    --genFeatures :: IO [GeographicFeature]
    --genFeatures n = do
    --  f <- replicateM n genFeature
    --  return f

    --describeFeatures fs = intercalate ", " (map show fs)

    --data RegionKind = Realm | 
    data Region = Region { regionDescriptor :: String, regionName :: String, geography :: LocalGeography }
      deriving (Eq, Show, Read)

    genRegion :: IO Region
    genRegion = do
      n <- genBasicName 
      g <- genGeography -- genFeatures 3 
      d <- pickFrom adjectives
      return (Region { regionName = n, geography = g, regionDescriptor = d })

    describeRegion rgn = "the " ++ (describeGeography (geography rgn)) ++  " region of " ++ (capWord (regionName rgn)) -- ++ " in " ++ (capWord (regionName (parent rgn))) -- ++ " called the " ++ (regionDescriptor rgn)  --nm



