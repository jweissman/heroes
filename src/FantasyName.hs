module FantasyName where
    import BasicNames
    import Probability
    import Nicknames
    import StringHelpers
    
    import CharacterSheet

    data FantasyName = FantasyName { forename :: String, surname :: String, nickname :: String } --, particles :: [String] }

    -- really should be part of a structure right?
    genName :: CharacterSheet -> IO String
    genName sheet = do
      forename <- genBasicName
      surname  <- genBasicName
      nickname <- pickFrom (nicknameGuesses sheet)
      return (let fullName = f ++ " " ++ s ++ " the " ++ n
                  f = capWord forename
                  s = capWord surname
                  n = capWord nickname
               in fullName)
