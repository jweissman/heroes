{-# LANGUAGE FlexibleInstances, UndecidableInstances, ScopedTypeVariables, OverlappingInstances #-}

import Data.List --as List
import System.IO
import System.Random
import Test.QuickCheck
import Data.Char as Char
import Control.Monad

--- string utils...
capWord [] = []
capWord (h:t) = Char.toUpper h : map Char.toLower t

capWords [] = []
capWords (h:t) = capWord h : (map capWord t)

--
-- probabilities
-- 
rollDie :: Integer -> IO Integer
rollDie faces = getStdRandom (randomR (1,faces)) 

rollDice :: Int -> Integer -> IO [Integer]
rollDice n faces = sequence $ replicate n (rollDie faces)

-- some sweet 'tax..
d = rollDice

--select random elems from bounded enums like r <- randomIO :: Klass
instance (Bounded a, Enum a) => Random a where
   random gen = randomR (minBound :: a, maxBound :: a) gen
   randomR (f, t) gen =
     (toEnum r :: a, nextGen)
     where
       (rnd, nextGen) = next gen
       r = fromEnum f + (rnd `mod` length [f..t])
       
randomIndex l = getStdRandom (randomR (0, length l - 1))

pickFrom :: [a] -> IO a
pickFrom l = do
  index <- randomIndex l
  return (l !! index)


data Language = CommonSpeech | ElvishSpeech | DwarvenSpeech | InfernalLanguage | CelestialLanguage | ReptilianSpeech | InsectSpeech | SacredLanguage | DeadLanguage | FaeSpeech | SilentLanguage | BlackSpeech | AvianSpeech | AstralSpeech | SecretLanguage | AncientSpeech | AlienSpeech | OutsiderSpeech | TrueSpeech
  deriving (Eq, Show, Read, Enum, Bounded)

humanizedLanguages langs = intercalate ", " (map show langs)

data RacialModifier = Dark | Deep | Brutal | Weird | High | Sea | Sky | Star | Cold | Fire | Construct | Insectile | Chaos | Corrupted | Augmented | Titanic | Crystal | Elder | Exiled | Mountain | Swamp | Ice | Dream | Desert | Magma | Urban | Arctic | Dusk | Savage | Barbarian | Neo | Arena | Mystic
  deriving (Eq, Show, Read, Enum, Bounded)

data Species = Halfling | Elf | Human | Dwarf | Orc | Gnome | Fae | Drakeling | Aberration | Giant | Outsider | Minotaur | Centaur | Imp | Dryad | Goblin | Sylph | Sprite | Pixie | Lycanthrope | Changeling | Vampire | Zombie | Golem | Satyr | Naiad | Cyclops
  deriving (Eq, Show, Read, Enum, Bounded)

data Race = Race { species :: Species, modifier :: RacialModifier } 
  deriving (Eq, Show, Read)

humanizedRace :: Race -> String
humanizedRace race = show (modifier race) ++ " " ++ show (species race)

--darkElf = Race { modifier = Dark, species = Elf }


data Profession = Aristocrat | Commoner | Warrior | Soldier | Bard | Trader | Pilgrim | Thief | Apothecary | Hunter | Cleric | Ranger | Monk | Sorceror | Paladin | Healer | Rogue | Merchant | Seeker | Archmage | Oracle | Priest | Wizard | Knight | Shadowmage | Commander | Diplomat | Ninja | Prophet | Pirate | Swashbuckler | Samurai | Warlord | Sage | Gambler | Scout | Assassin | WitchDoctor | Templar | Sniper | Trapper | Dancer | Dervish | Alchemist | Trickster | Inquisitor | Missionary
    deriving (Eq, Show, Read, Enum, Bounded)

data ProfessionalSubtype = Academic | Colonial | Rural | Telepathic | Drifter | Novitiate | Infernal | Celestial | Nomad | Forester | Herald | Criminal | Royal | Clandestine | Military | Shadow | Dread | Scavenger | Feral | Primitive | Dilettante | Outcast | Celebrity | Eldritch
  deriving (Eq, Show, Read, Enum, Bounded)

data Job = Job { profession :: Profession, subtype :: ProfessionalSubtype }
  deriving (Eq, Show, Read)

humanizedJob job = show (subtype job) ++ " " ++ show (profession job)

data MoralAlignment = Good | MoralNeutral | Evil
  deriving (Eq, Show, Read, Enum, Bounded)

-- amoral is a weird choice here, since it is paired with good/evil (neutral is better)
data EthicalAlignment = Lawful | EthicalNeutral | Chaotic
  deriving (Eq, Show, Read, Enum, Bounded)

data Alignment = Alignment { ethical :: EthicalAlignment
			   , moral :: MoralAlignment } deriving (Eq, Show, Read)

aligned :: EthicalAlignment -> MoralAlignment -> Alignment
aligned e m = Alignment { ethical = e, moral = m }

describeAlignment :: EthicalAlignment -> MoralAlignment -> String
describeAlignment Lawful  Good            = "crusader"
describeAlignment EthicalNeutral Good     = "benefactor"
describeAlignment Chaotic Good            = "rebel"
describeAlignment Lawful MoralNeutral     = "judge"
describeAlignment EthicalNeutral MoralNeutral     = "undecided"
describeAlignment Chaotic MoralNeutral    = "free spirit"
describeAlignment Lawful Evil             = "dominator"
describeAlignment EthicalNeutral Evil     = "malefactor"
describeAlignment Chaotic Evil            = "destroyer"

humanizedAlignment :: Alignment -> String
humanizedAlignment alignment = d
  where d = capWord (describeAlignment (ethical alignment) (moral alignment))


data Stats = Stats { strength :: Integer
	           , constitution :: Integer
		   , charisma :: Integer
		   , intelligence :: Integer
		   , wisdom :: Integer
		   , dexterity :: Integer } 
  deriving (Eq, Show, Read)
		   
-- this is not very haskell-y i don't think! not much of this is of course
-- but anyway this was really build to use a list of rolled dice values...
buildStats (str:con:cha:int:wis:dex:xs) =  Stats { strength = str
					         , constitution = con
						 , charisma = cha
						 , intelligence = int
						 , wisdom = wis
						 , dexterity = dex }

						
--data StatQuality = Terrible | Poor | Average | ... ?
judgeStat stat
  | stat <= 5  = "terrible"
  | stat <= 8  = "poor"
  | stat <= 12 = "average"
  | stat <= 15 = "good"
  | stat <= 18 = "great"
  | stat <= 19 = "epic"
  | stat >  19 = "deity"

terrible stat = judgeStat stat == "terrible"
poor     stat = judgeStat stat == "poor"
average  stat = judgeStat stat == "average" || good stat
good     stat = judgeStat stat == "good" || great stat
great    stat = judgeStat stat == "great" || epic stat
epic     stat = judgeStat stat == "epic" || deity stat
deity    stat = judgeStat stat == "deity"


humanizeStat name stat = "\n    " ++ capWord name ++ ": " ++ show stat ++ " (" ++ judgeStat stat ++ ")" 

humanizedStats stats = humanizeStat "constitution" con  ++    
		       humanizeStat "charisma" cha      ++    
		       humanizeStat "wisdom" wis        ++    
		       humanizeStat "intelligence" int  ++    
		       humanizeStat "dexterity" dex     ++    
		       humanizeStat "strength" str
  where str = strength stats
        con = constitution stats
	cha = charisma stats
	wis = wisdom stats
	int = intelligence stats
	dex = dexterity stats

data SkillClass = Novice | Adept | Adequate | Expert | Master
data Skill      = Will | Athletics | Acrobatics | Gymnastics | Brawling | Swimming | Climbing | Shooting | Ride | Focus | Lore | Astrology | Botany | Mechanics | Logic | Mathematics | Philosophy | Theology | Law | FirstAid | Medicine | Bartering | Bluffing | Appraising | Pandering | Economics | Leatherworking | Blacksmithing | Armory | Brewing | Farming | Fletching | Energy | Alchemy | AnimalRapport | Spellcraft | Enchanting | Mindreading | Creativity | Aesthetics | Sculpture | Singing | Acting | Performance | Prestidigitation | Speech | Conversation | Rhetoric | Reading | Writing | Speaking | Storytelling | Observation | Persuasion | Provocation | Administration | Intimidation | Inspiration | Arbitrartion | Perception | Survival | Tactics | DisablingTraps | CreatureLore | Herbalism | Deception | Stealing | Surveillance | Sneaking | Cryptography | Spirit | Healing | Blessing | Prayer | OccultLore | Patience
  deriving (Eq, Show, Read, Bounded, Enum)

humanizedSkills sks = intercalate ", " (map show sks)

data CharacterSheet = CharacterSheet { race :: Race
				     , job :: Job
				     , alignment :: Alignment
				     , stats :: Stats
				     , languages :: [Language]
				     , skills :: [Skill] }
  deriving (Eq, Show, Read)

characterDescription :: CharacterSheet -> String
characterDescription character = 
  "\n  Race: " ++ r ++ 
  "\n  Job: " ++ j ++ 
  "\n  Alignment: " ++ a ++ 
  "\n" ++
  "\n  Skills" ++
  "\n  ------" ++ 
  "\n    * speaks " ++ l ++
  "\n    * skilled in " ++ sk ++
  "\n" ++
  "\n  Statistics" ++
  "\n  ----------" ++ 
  "\n" ++
  st

  where a = humanizedAlignment (alignment character)
        r = humanizedRace (race character)
	j = humanizedJob (job character)
	st = humanizedStats (stats character)
	l = humanizedLanguages (languages character)
	sk = humanizedSkills (skills character)

displayCharacterSheet :: CharacterSheet -> String
displayCharacterSheet character = "=====================================================================\n" ++ characterDescription character ++ "\n\n"

data Hero = Hero { name :: String
	         , sheet :: CharacterSheet }
  deriving (Eq, Show, Read)

adjectives = [ "fine", "foul", "mad", "clean", "sweet", "cold", "hot", "green", "red", "blue", "black", "violet", "white", "gray", "dead", "easy", "broad", "shy", "odd", "fierce", "big", "little", "high", "low", "proud", "happy", "straight", "wide", "square", "round", "deep", "shallow", "bitter", "faint", "kind", "full", "cool", "warm", "heavy", "light", "dry", "wet", "crooked", "straight", "iron", "wood", "steel", "diamond", "indigo", "bright", "sour", "flat", "rough", "twist", "still", "quiet", "fell", "fey", "slow", "swift", "quick", "fleet" ]

nouns = [ "baker", "shrine", "frog", "beast", "king", "ville", "field", "glen", "brook", "serpent", "river", "tree", "creek", "ton", "mount", "hill", "vale", "shire", "shrine", "hook", "brush", "twig", "berry", "shine", "winter", "bear", "hunt", "wick", "wax", "flax", "seed", "turtle", "pen", "fold", "sill", "tell", "tale", "satyr", "saw", "awn", "fall", "knife", "fork", "turn", "cross", "drone", "don", "dome", "hold", "throne", "chill", "breeze", "flay", "song", "shadow", "shout", "kill", "point", "home", "hint", "barge", "smith", "joiner", "star", "ash", "birch", "september", "ruby", "pearl", "crystal", "switch", "curve", "mash", "court", "spin", "sand", "slip", "wall", "bridge", "moat" ]

genBasicName = do
  adj <- pickFrom adjectives
  noun <- pickFrom nouns
  return (adj ++ noun)


nicknameGuessesFromStrength str
  | poor str  = ["weak", "puny", "measly"]
  | great str = ["strong", "bear", "titan", "powerful", "muscular", "giant"]
  | otherwise = [] 

nicknameGuessesFromConstitution con
  | poor con  = ["dying", "doomed", "diseased", "ill", "sick"]
  | great con = ["healthy", "sturdy", "durable", "vigorous", "dragon"]
  | otherwise = []

nicknameGuessesFromCharisma cha
  | poor cha  = ["unclean", "filthy", "vulgar", "ugly", "redolent"]
  | great cha = ["beautiful", "handsome", "glorious", "radiant", "aromatic", "charming", "fragrant"]
  | otherwise = []

nicknameGuessesFromIntelligence int
  | poor int  = ["slow-witted","dim"]
  | great int = ["bright", "quick-witted", "sharp-tongued"]
  | otherwise = []

nicknameGuessesFromWisdom wis
  | poor wis  = ["confused", "dull"]
  | great wis = ["thoughtful", "clever", "wise", "brilliant"]
  | otherwise = [] 

nicknameGuessesFromDexterity dex
  | poor dex  = ["slow"]
  | great dex = ["quick","fleet", "swift"]
  | otherwise = []


nicknameGuessesFromStats stats = concat statGuessElements
  where fromStrength = nicknameGuessesFromStrength (strength stats)
        fromCha = nicknameGuessesFromCharisma (charisma stats)
        fromWis = nicknameGuessesFromWisdom   (wisdom stats)
        fromInt = nicknameGuessesFromIntelligence (intelligence stats)
        fromCon = nicknameGuessesFromConstitution (constitution stats)
	fromDex = nicknameGuessesFromDexterity (dexterity stats)
	statGuessElements = [fromStrength, fromCha, fromWis, fromInt, fromCon, fromDex]

nicknameGuessesFromJob job
  | job == Aristocrat = ["illustrious", "king", "queen", "prince", "haughty", "glorious", "eminent"]
  | job == Cleric = ["faithful"]
  | otherwise = []

nicknameGuessesFromSkills skills
  | Leatherworking `elem` skills = ["leatherman", "leathermaster"]
  | Blacksmithing `elem` skills  = ["mastersmith"]
  | Armory `elem` skills         = ["armorer"]
  | Will `elem` skills           = ["strong-willed", "fortitudinous"]
  | Patience `elem` skills       = ["cautious", "patient"]
  | otherwise = []

nicknameGuessesFromMoralAlignment morality
  | morality == Good    = ["kind", "sweet", "good", "valorous", "noble"]
  | morality == MoralNeutral = ["even-handed", "impartial", "neutral"] 
  | morality == Evil    = ["wicked", "devious", "cruel", "evil", "terrible"]

nicknameGuessesFromEthicalAlignment ethics
  | ethics == Lawful  = ["honorable", "true", "just"]
  | ethics == EthicalNeutral  = ["unconverted", "free-hearted", "independent"]
  | ethics == Chaotic = ["wild", "unhinged", "unpredictable"]

nicknameGuessesFromAlignment alignment = concat [fromEthics, fromMorals]
  where fromMorals = nicknameGuessesFromMoralAlignment (moral alignment)
	fromEthics = nicknameGuessesFromEthicalAlignment (ethical alignment)
	
nicknameGuesses character = concat guessElements
  where fromStats     = nicknameGuessesFromStats  	(stats character)  
	fromSkills    = nicknameGuessesFromSkills  	(skills character)
	fromAlignment = nicknameGuessesFromAlignment 	(alignment character)
	fromJob       = [] 
	-- nicknameGuessesFromJob    	(job (profession character))
	guessElements = [fromStats, fromSkills, fromJob, fromAlignment]

-- really should be part of a structure right?
genName sheet = do
  forename <- genBasicName
  surname  <- genBasicName
  nickname <- (pickFrom (nicknameGuesses sheet))
  return (let fullName = f ++ " " ++ s ++ " the " ++ n
              --p = capWord namePrefix
	      f = capWord forename
	      s = capWord surname
	      n = capWord nickname
           in fullName)


-- generate skeletal character sheet
genCharacterSheet = do
  rolledValues <- 6 `d` 20
  racialModifier <- randomIO :: IO RacialModifier
  sp <- randomIO :: IO Species
  morality <- randomIO :: IO MoralAlignment
  ethics <- randomIO :: IO EthicalAlignment
  professionalSubtype <- randomIO :: IO ProfessionalSubtype
  prof <- randomIO :: IO Profession
  languages <- sequence $ replicate 3 (randomIO :: IO Language)
  skills    <- sequence $ replicate 5 (randomIO :: IO Skill)

  return (let r = Race { modifier = racialModifier, species = sp }
              st = (buildStats rolledValues)
              a = (aligned ethics morality)
              j = Job { profession = prof, subtype = professionalSubtype }
              l = (nub (CommonSpeech:languages))
              sk = (nub skills)
              in CharacterSheet { race = r, alignment = a, job = j, stats = st, languages = l, skills = sk })

-- generate random named hero
genHero = do
  characterSheet <- genCharacterSheet
  characterName <- genName characterSheet
  return Hero { name = characterName, sheet = characterSheet } 

main :: IO ()
main = forever $ do
    hero <- genHero

    putStrLn $ name hero
    putStrLn $ displayCharacterSheet (sheet hero)

    --putStrLn "============================="
