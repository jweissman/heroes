module Hero where
    import CharacterSheet
    import FantasyName
    
    data Hero = Hero { name :: String
                     , sheet :: CharacterSheet }
      deriving (Eq, Show, Read)

    -- generate random named hero
    genHero = do
      characterSheet <- genCharacterSheet
      characterName <- genName characterSheet
      return Hero { name = characterName, sheet = characterSheet }