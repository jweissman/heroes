module Person where
    data Person = Person { name :: String, ethnicity :: Race, profession :: Job }
      deriving (Eq, Show, Read)

    genPerson = do
      n <- genBasicName
      eth <- genRace
      return (Person { name = n, ethnicity = eth })
