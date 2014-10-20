module FantasyProfession where
    import System.Random
    import Probability
    import Quality
    import Skill
    import FantasyStats
    import Alignment
    import Precondition

    data ProfessionalRarity      = Basic | Adventurer | Epic 
      deriving (Eq, Show, Read)

    data Profession = Profession { professionName :: String, professionType :: SkillGroup, professionalRarity :: ProfessionalRarity, professionalRestrictions :: [Precondition], professionalSkills :: [SkillType] }
      deriving (Eq, Show, Read)

    --profession rarity profType name skills restrictions = Profession { professionName = name, professionType = profType, professionalRarity = rarity, professionalSkills = skills, professionalRestrictions = restrictions }

    basicProfession 	     = Profession { professionName = "", professionalRarity = Basic, professionType = Natural, professionalRestrictions = [], professionalSkills = [] }
    basicNaturalProfession   = basicProfession --{ professionType = Natural }
    basicPerformerProfession = basicProfession { professionType = Performance }
    basicRogueProfession     = basicProfession { professionType = Clandestine }
    basicEconomicProfession  = basicProfession { professionType = Economic }
    basicMagicalProfession   = basicProfession { professionType = Arcane }

    hunter      = basicNaturalProfession { professionName = "hunter" } -- [ ] [ ] --Profession { professionalName = "hunter", professionType = Natural, professionalRarity = Basic, professionalRestrictions = [ ProfessionalRestrictionOnStats Wis 3 ], professionalSkills = [ tracking ] } 
    farmer      = basicNaturalProfession { professionName = "farmer" } -- [ ] [ ] --Profession { professionalName = "hunter", professionType = Natural, professionalRarity = Basic, professionalRestrictions = [ ProfessionalRestrictionOnStats Wis 3 ], professionalSkills = [ tracking ] } 

    thief = basicRogueProfession { professionName = "thief" } -- [ ] [] -- dexterityRestriction Good ]

    dancer = basicPerformerProfession { professionName = "dancer" } -- [ ] [] -- charismaRestrictrion Good ]

    trader      = basicEconomicProfession { professionName = "trader" } -- [ ] [] -- charismaRestriction Good ]

    illusionist = basicMagicalProfession { professionName = "illusionist" } -- [ ] []

    basicNatureProfessions = [ farmer, hunter ] --, tracker ]
    basicFighterProfessions = [ ] -- brawler, wrestler ]
    basicRogueProfessions = [ thief ] --, scout, hustler, trickster ]
    basicPerformerProfessions = [ dancer ] --, singer ]
    basicEconomicProfessions = [ trader ] --, beggar, gambler ]
    basicMagicalProfessions = [ illusionist ] -- chanter ]
    basicReligiousProfessions = [  ]

    basicProfessions = basicNatureProfessions ++ basicFighterProfessions ++ basicRogueProfessions ++ basicPerformerProfessions ++ basicEconomicProfessions ++ basicMagicalProfessions ++ basicReligiousProfessions


    --data ProfessionSubtype 

    --data NormalSubtype = Academic | Colonial | Criminal | Drifer | Forester | Nomad | Novitiate | Scavenger --| | | | |
    --data RareSubtype = Feral | Dilettante | Clandestine | Shadow | Herald | Outcast | Guerrilla
    --data EpicProfessionalSubtype = Telepathic | Celebrity | Eldritch | Royal

    data ProfessionalSubtype = ProfessionalSubtype ProfessionalRarity String -- NormalSubtype | RareSubtype | EpicSubtype
      deriving (Eq, Show, Read)

    humanizeProfessionalSubtype (ProfessionalSubtype _ s) = s

    basicProfessionalSubtype      = ProfessionalSubtype Basic
    adventurerProfessionalSubtype = ProfessionalSubtype Adventurer
    epicProfessionalSubtype       = ProfessionalSubtype Epic
    
    --basicProfessionalSubtypeNames = [ "academic", "colonial", "criminal", "drifter", "nomad", "novitiate", "scavenger" ]
    --basicProfessionalSubtypes = map basicProfessionalSubtype basicProfessionalSubtypeNames

    academic = basicProfessionalSubtype "academic" 
    colonial = basicProfessionalSubtype "colonial"
    clandestine = adventurerProfessionalSubtype "clandestine"
    telepathic = epicProfessionalSubtype "telepathic" -- [ foresight (+5) ]

    normalProfessionalSubtypes = [ academic, colonial ]
    rareProfessionalSubtypes = [ clandestine ]
    epicProfessionalSubtypes = [ telepathic ]
    professionalSubtypes = normalProfessionalSubtypes ++ rareProfessionalSubtypes ++ epicProfessionalSubtypes


    data Job = Job Profession ProfessionalSubtype Quality
      deriving (Eq, Show, Read)
    --data Job = Job Profession ProfessionalSkillLevel

    humanizedJob :: Job -> String
    humanizedJob (Job prof st q) = humanizeProfessionalSubtype st ++ " " ++ professionName prof --(profession job)


    genJob = do
      professionalSubtype <- pickFrom professionalSubtypes --randomIO :: IO ProfessionalSubtype
      prof                <- pickFrom basicProfessions     --randomIO :: IO Profession
      quality 		  <- genQuality
      return (Job prof professionalSubtype quality)  	   --  { profession = prof, subtype = professionalSubtype }
    ------------


    ----data ProfessionalClass = Economic | Military | Criminal
    ----data Professionalt = ProfessionType ProfessionalCompetence -- Level

    --data ProfessionType = ProfessionType ProfessionalRarity ProfessionalGroupType [ProfessionalClass]
    --data BasicProfessionType = ProfessionType Basic
    --data BasicNaturalProfessionType = BasicProfessionType Natural [
  
    --data BasicNatureProfessionType = Hunter | Tracker | Scout
    --data BasicFighterProfessionType = Brawler | Wrestler
    --data BasicRogueProfessionType = Thief
  
    --data BasicProfessionType = BasicNatureProfessionType | BasicFighterProfessionType -- Aristocrat | Commoner | Fighter | Trader | Pilgrim | Thief | Apothecary | Hunter | Missionary | Dancer | Acolyte | Tracker | Scout

    --data AdventurerNatureProfessionType = Ranger | Druid
    --data AdventurerFighterProfessionType = Warrior | Soldier | Archer
    --data AdventurerRogueProfessionType = Spy | Thief | Ninja | Pirate | Assassin | Diplomat
    --data AdventurerPerformerProfessionType = Bard
    --data AdventurerEconomicProfessionType = Gambler | Hustler | Merchant
    --data AdventurerMageProfessionType = Wizard | Mage | Warlock | Healer  -- | Adept
    --data AdventurerReligiousProfessionType = Monk | Cleric | Priest | WitchDoctor | Inquisitor

    --data BasicAdventurerProfessionType = BasicNatureProfessionType | BasicFighterProfessionType | BasicRogueProfessionType | BasicEconomicProfessionType | BasicMageProfessionType | BasicReligiousProfessionType

    --data EpicNatureProfessionType = Treespeaker
    --data EpicFighterProfessionType = Knight
    --data EpicRogueProfessionType = Diplomat | Samurai | Assassin | Swashbuckler
    --data EpicPerformerProfessionType = Caller
    --data EpicEconomicProfessionType = Gambler | Hustler | Merchant
    --data EpicMageProfessionType = Archmage 
    --data EpicReligiousProfessionType = Templar | Oracle | Prophet | Paladin

    --data EpicProfessionType = EpicNatureProfessionType | EpicFighterProfessionType | EpicRogueProfessionType | EpicPerformerProfessionType | EpicEconomicProfessionType | EpicMageProfessionType | EpicReligiousProfessionType

    --


    ----data EpicProfession = EpicProfession [ProfessionalRequirement]

    ----data DualClass = DualClass { parents :: [ AdventurerProfession ], AdventurerProfession
    --data RogueProfession = RogueProfession RogueProfessionType ProfessionalCompetence

    --data AdventurerProfession = RogueProfession | FighterProfession | MageProfession -- Warrior | Archer | Bard | Cleric | Ranger | Monk | Sorceror | Paladin | Rogue | Mage | Priest | Wizard | Knight

    --data ProfessionType = BasicProfessionType | AdventurerProfessionType | EpicProfessionType
    --    deriving (Eq, Show, Read, Enum, Bounded)

    --data Profession = Profession ProfessionType ProfessionSubtype ProfessionalCompetence --Aristocrat | Commoner | Warrior | Soldier | Archer | Bard | Trader | Pilgrim | Thief | Apothecary | Hunter | Cleric | Ranger | Monk | Sorceror | Paladin | Healer | Rogue | Merchant | Seeker | Archmage | Oracle | Priest | Wizard | Knight | Shadowmage | Commander | Diplomat | Ninja | Prophet | Pirate | Swashbuckler | Samurai | Warlord | Sage | Gambler | Scout | Assassin | WitchDoctor | Templar | Sniper | Trapper | Dancer | Dervish | Alchemist | Trickster | Inquisitor | Missionary | Tracker
        --deriving (Eq, Show, Read, Enum, Bounded)

    --data ProfessionalSubtype = Academic | Colonial | Rural | Telepathic | Drifter | Novitiate | Infernal | Celestial | Nomad | Forester | Herald | Criminal | Royal | Clandestine | Military | Guerrilla | Shadow | Dread | Scavenger | Feral | Primitive | Dilettante | Outcast | Celebrity | Eldritch
    --  deriving (Eq, Show, Read, Enum, Bounded)

    --data NormalSubtype = Academic | Colonial | Criminal | Drifer | Forester | Nomad | Novitiate | Scavenger --| | | | |
    --data RareSubtype = Feral | Dilettante | Clandestine | Shadow | Herald | Outcast | Guerrilla
    --data EpicProfessionalSubtype = Telepathic | Celebrity | Eldritch | Royal

    --data ProfessionalSubtype = NormalSubtype | RareSubtype | EpicSubtype

    --data ProfessionalRestriction = Profession StatisticValue -- Statistic Integer

    --data Job = Job ProfessionalSubtype Subtype -- { profession :: Profession, subtype :: ProfessionalSubtype }
    --  deriving (Eq, Show, Read)

