module FantasyName where
    import BasicNames
    import Probability
    import Nicknames
    import StringHelpers
    
    import CharacterSheet

    --data FantasyName = FantasyName { forename :: String, surname :: String, nickname :: String } --, particles :: [String] }

    -- really should be part of a structure right?
    genName :: CharacterSheet -> IO String
    genName sheet = do
      f <- genBasicName
      s <- genBasicName
      n <- pickFrom (nicknameGuesses sheet)
      return (capWord f ++ " " ++ capWord s ++ " the " ++ capWord n)
