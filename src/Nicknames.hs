module Nicknames where
    import CharacterSheet
    import FantasyStats
    import FantasyProfession
--    import FantasyRace
    import Alignment
    import Skill

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
      where fromStats     = nicknameGuessesFromStats        (stats character)  
            fromSkills    = nicknameGuessesFromSkills       (skills character)
            fromAlignment = nicknameGuessesFromAlignment    (alignment character)
            fromJob       = [] 
            -- nicknameGuessesFromJob       (job (profession character))
            guessElements = [fromStats, fromSkills, fromJob, fromAlignment]
