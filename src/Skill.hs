module Skill where
    import Data.List
    import Control.Monad
    import System.Random
    import Probability
    import Quality

    data SkillGroup = Generic | Rhetoric | Athletic | Arcane | Academic | Economic | Natural | Clandestine | Religious | Performance
      deriving (Eq, Show, Read, Bounded, Enum)
      
    --data SkillDegree = Novice | Adept | Adequate | Expert | Master
    --data SkillRestrictionStatistic = SkillRestriction Statistic
    --data SkillRestrictionClass     = SkillRestriction Profession
  
    --data Skill

    data SkillType = SkillType String SkillGroup --[SkillRestriction]
      deriving (Eq, Show, Read)

    tactics  = SkillType "tactics"  Generic
    strategy  = SkillType "strategy" Generic
    lore      = SkillType "lore" Generic
    conversation = SkillType "conversation" Generic

    athletics = SkillType "athletics" Athletic
    brawl     = SkillType "brawling" Athletic
    climb     = SkillType "climbing" Athletic
    ride      = SkillType "riding" Athletic
    swim      = SkillType "swimming" Athletic

    bluff 	 = SkillType "deception" Clandestine
    crypto       = SkillType "cryptography" Clandestine
    disableTraps = SkillType "disabling traps" Clandestine
    pickpocket   = SkillType "pickpocketing" Clandestine
    surveil      = SkillType "surveillance" Clandestine

    occultLore = SkillType "occult lore" Arcane
    astrology  = SkillType "astrology" Arcane
    enchant    = SkillType "enchanting" Arcane
    alchemy    = SkillType "alchemy" Arcane

    botany = SkillType "botany" Academic
    maths  = SkillType "mathematics" Academic

    theology = SkillType "theology" Religious

    persuade = SkillType "persuasion" Rhetoric


    generalSkills = [ strategy, athletics ]
    athleticSkills = [ athletics, brawl, climb, ride ]
    clandestineSkills = [ bluff, crypto, disableTraps, pickpocket, surveil ]
    arcaneSkills = [ occultLore, astrology, enchant, alchemy ]
    academicSkills = [ botany, maths ]
    religiousSkills = [ theology ]
    rhetoricSkills = [ persuade ]

    allSkills = generalSkills ++ athleticSkills ++ clandestineSkills ++ arcaneSkills ++ academicSkills ++ religiousSkills ++ rhetoricSkills

    --sneakySkills = [ deception, bluffing ]
    
    data Skill   = Skill SkillType Integer  --Will | Athletics | Acrobatics | Gymnastics | Brawling | Swimming | Climbing | Shooting | Ride | Focus | Lore | Astrology | Botany | Mechanics | Logic | Mathematics | Philosophy | Theology | Law | FirstAid | Medicine | Bartering | Bluffing | Appraising | Pandering | Economics | Leatherworking | Blacksmithing | Armory | Brewing | Farming | Fletching | Energy | Alchemy | AnimalRapport | Spellcraft | Enchanting | Mindreading | Creativity | Aesthetics | Sculpture | Singing | Acting | Performance | Prestidigitation | Speech | Conversation | Rhetoric | Reading | Writing | Speaking | Storytelling | Observation | Persuasion | Provocation | Administration | Intimidation | Inspiration | Arbitrartion | Perception | Survival | Tactics | DisablingTraps | CreatureLore | Herbalism | Deception | Stealing | Surveillance | Sneaking | Cryptography | Spirit | Healing | Blessing | Prayer | OccultLore | Patience
      deriving (Eq, Show, Read) --, Bounded, Enum)

    --data Skill = Skill { kind :: SkillType, level :: SkillDegree }

    --judgeSkill sk = judge sk

    humanizeSkill (Skill (SkillType n g) v) = show (judge v) ++ " " ++ n -- (skType sk)

    humanizedSkills :: [Skill] -> String
    humanizedSkills sks = intercalate ", " (map humanizeSkill sks)

    --guessSkillsFromJob job
    --  | job == Thief = [ Sneaking, Surveillance, Stealing ]
    --  | job == Archer = [ Shooting ]
    --  | otherwise = []

    rollSkill ofType = do
      wVal <- 1 `d` 20
      return (Skill ofType (head wVal))

    genSkill = do
      skType <- pickFrom allSkills -- randomIO :: IO SkillType
      val <- 1 `d` 20
      --sk <- rollSkill skType
      --val <- 1 `d` 20
      return (Skill skType (head val)) --(rollSkill skType $) --Skill skType val

    --rollSkill sk = do
    --  val <- 1 `d` 20
    --  return Skill sk val

    genSkills n = do
      sks <- replicate n genSkill --map rollSkill baseSkills
      return sks
      --map rollSkill baseSkills --replicateM n genSkill --(randomIO :: IO Skill)
