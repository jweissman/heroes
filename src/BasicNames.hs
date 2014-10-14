module BasicNames where

    import Probability

    adjectives :: [String]
    adjectives = [ "fine", "foul", "mad", "clean", "sweet", "cold", "hot", "green", "red", "blue", "black", "violet", "white", "gray", "dead", "easy", "broad", "shy", "odd", "fierce", "big", "little", "high", "low", "proud", "happy", "straight", "wide", "square", "round", "deep", "shallow", "bitter", "faint", "kind", "full", "cool", "warm", "heavy", "light", "dry", "wet", "crooked", "straight", "iron", "wood", "steel", "diamond", "indigo", "bright", "sour", "flat", "rough", "twist", "still", "quiet", "fell", "fey", "slow", "swift", "quick", "fleet" ]

    nouns :: [String]
    nouns = [ "baker", "shrine", "frog", "beast", "king", "ville", "field", "glen", "brook", "serpent", "river", "tree", "creek", "ton", "mount", "hill", "vale", "shire", "shrine", "hook", "brush", "twig", "berry", "shine", "winter", "bear", "hunt", "wick", "wax", "flax", "seed", "turtle", "pen", "fold", "sill", "tell", "tale", "satyr", "saw", "awn", "fall", "knife", "fork", "turn", "cross", "drone", "don", "dome", "hold", "throne", "chill", "breeze", "flay", "song", "shadow", "shout", "kill", "point", "home", "hint", "barge", "smith", "joiner", "star", "ash", "birch", "september", "ruby", "pearl", "crystal", "switch", "curve", "mash", "court", "spin", "sand", "slip", "wall", "bridge", "moat" ]

    genBasicName :: IO String
    genBasicName = do
      adj <- pickFrom adjectives
      noun <- pickFrom nouns
      return (adj ++ noun)

