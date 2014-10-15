module FantasyRace where
    import System.Random
    import Probability
    
    data RacialModifier = Dark | Deep | Brutal | Weird | High | Sea | Sky | Star | Cold | Fire | Construct | Insectile | Chaos | Corrupted | Augmented | Titanic | Crystal | Elder | Exiled | Mountain | Swamp | Ice | Dream | Desert | Magma | Urban | Arctic | Dusk | Savage | Barbarian | Neo | Arena | Mystic
      deriving (Eq, Show, Read, Enum, Bounded)

    data Species = Halfling | Elf | Human | Dwarf 
    -- | Orc | Gnome | Fae | Drakeling | Aberration | Giant | Outsider | Minotaur | Centaur | Imp | Dryad | Goblin | Sylph | Sprite | Pixie | Lycanthrope | Changeling | Vampire | Zombie | Golem | Satyr | Naiad | Cyclops
      deriving (Eq, Show, Read, Enum, Bounded)

    data Race = Race { species :: Species, modifier :: RacialModifier } 
      deriving (Eq, Show, Read)

    humanizedRace :: Race -> String
    humanizedRace race = show (modifier race) ++ " " ++ show (species race)

    genRace = do
      racialModifier      <- randomIO :: IO RacialModifier
      sp                  <- randomIO :: IO Species
      
      return Race { species = sp, modifier = racialModifier }