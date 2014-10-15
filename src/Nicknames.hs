module Nicknames where
    import CharacterSheet
    import FantasyStats
    import FantasyProfession
--    import FantasyRace
    import Alignment
    import Skill

    nicknameGuessesFromStrength :: Integer -> [String]
    nicknameGuessesFromStrength str
      | poor str  = ["weak", "puny", "measly"]
      | great str = ["strong", "bear", "titan", "powerful", "muscular", "giant"]
      | otherwise = [] 
      
    nicknameGuessesFromConstitution :: Integer -> [String]
    nicknameGuessesFromConstitution con
      | poor con  = ["dying", "doomed", "diseased", "ill", "sick"]
      | great con = ["healthy", "sturdy", "durable", "vigorous", "dragon"]
      | otherwise = []
      
    nicknameGuessesFromCharisma :: Integer -> [String]
    nicknameGuessesFromCharisma cha
      | poor cha  = ["unclean", "filthy", "vulgar", "ugly", "redolent"]
      | great cha = ["beautiful", "handsome", "glorious", "radiant", "aromatic", "charming", "fragrant"]
      | otherwise = []

    nicknameGuessesFromIntelligence :: Integer -> [String]
    nicknameGuessesFromIntelligence int
      | poor int  = ["slow-witted","dim"]
      | great int = ["bright", "quick-witted", "sharp-tongued"]
      | otherwise = []
      
    nicknameGuessesFromWisdom :: Integer -> [String]
    nicknameGuessesFromWisdom wis
      | poor wis  = ["confused", "dull"]
      | great wis = ["thoughtful", "clever", "wise", "brilliant"]
      | otherwise = [] 

    nicknameGuessesFromDexterity :: Integer -> [String]
    nicknameGuessesFromDexterity dex
      | poor dex  = ["slow"]
      | great dex = ["quick","fleet", "swift"]
      | otherwise = []

    nicknameGuessesFromStats :: Stats -> [String]
    nicknameGuessesFromStats sts = concat statGuessElements
      where fromStrength      = nicknameGuessesFromStrength     (strength sts)
            fromCha           = nicknameGuessesFromCharisma     (charisma sts)
            fromWis           = nicknameGuessesFromWisdom       (wisdom sts)
            fromInt           = nicknameGuessesFromIntelligence (intelligence sts)
            fromCon           = nicknameGuessesFromConstitution (constitution sts)
            fromDex           = nicknameGuessesFromDexterity    (dexterity sts)
            statGuessElements = [fromStrength, fromCha, fromWis, fromInt, fromCon, fromDex]

    nicknameGuessesFromJob :: Profession -> [String]
    nicknameGuessesFromJob j
      | j == Aristocrat = ["illustrious", "king", "queen", "prince", "haughty", "glorious", "eminent"]
      | j == Cleric = ["faithful"]
      | otherwise = []

    nicknameGuessesFromSkills :: [Skill] -> [String]
    nicknameGuessesFromSkills sks
      | Leatherworking `elem` sks = ["leatherman", "leathermaster"]
      | Blacksmithing `elem` sks  = ["mastersmith"]
      | Armory `elem` sks         = ["armorer"]
      | Will `elem` sks           = ["strong-willed", "fortitudinous"]
      | Patience `elem` sks       = ["cautious", "patient"]
      | otherwise = []

    nicknameGuessesFromMoralAlignment :: MoralAlignment -> [String]
    nicknameGuessesFromMoralAlignment morality
      | morality == Good    = ["kind", "sweet", "good", "valorous", "noble"]
      | morality == MoralNeutral = ["even-handed", "impartial", "neutral"] 
      | morality == Evil    = ["wicked", "devious", "cruel", "evil", "terrible"]
      | otherwise           = []

    nicknameGuessesFromEthicalAlignment :: EthicalAlignment -> [String]
    nicknameGuessesFromEthicalAlignment ethics
      | ethics == Lawful  = ["honorable", "true", "just"]
      | ethics == EthicalNeutral  = ["unconverted", "free-hearted", "independent"]
      | ethics == Chaotic = ["wild", "unhinged", "unpredictable"]
      | otherwise = []

    nicknameGuessesFromAlignment :: Alignment -> [String]
    nicknameGuessesFromAlignment a = fromEthics ++ fromMorals
      where fromMorals = nicknameGuessesFromMoralAlignment (moral a)
            fromEthics = nicknameGuessesFromEthicalAlignment (ethical a)

    nicknameGuesses :: CharacterSheet -> [String] 
    nicknameGuesses character = concat guessElements
      where fromStats     = nicknameGuessesFromStats        (stats character)  
            fromSkills    = nicknameGuessesFromSkills       (skills character)
            fromAlignment = nicknameGuessesFromAlignment    (alignment character)
            fromJob       = [] 
            -- nicknameGuessesFromJob       (job (profession character))
            guessElements = [fromStats, fromSkills, fromJob, fromAlignment]
