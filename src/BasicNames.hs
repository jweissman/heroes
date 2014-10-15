module BasicNames where

    import Probability

    adjectives :: [String]
    adjectives = [ "able", "airy", "antic", "apt", "astral", "avid", "big", "bitter", "black", "blue", "bright", "broad", "clean", "cold", "cool", "crooked", "dead", "deep", "diamond", "dreary", "dry", "easy", "faint", "fell", "fey", "fierce", "fine", "flat", "fleet", "foul", "full", "gray", "green", "happy", "heavy", "high", "hot", "indigo", "iron", "kind", "light", "little", "low", "mad", "odd", "proud", "quick", "quiet", "red", "rough", "round", "shallow", "shy", "slow", "sour", "square", "steel", "still", "straight", "straight", "sweet", "swift", "twist", "violet", "warm", "wet", "white", "wide", "wood" ]

    nouns :: [String]
    nouns = [ "ark", "ash", "awn", "baker", "barge", "bear", "beast", "berry", "birch", "branch", "breeze", "bridge", "brook", "brush", "chill", "count", "court", "creek", "cross", "crystal", "curve", "dale", "dell", "dome", "don", "down", "drone", "fall", "field", "flax", "flay", "fog", "fold", "fork", "frog", "glen", "hill", "hint", "hold", "hole", "home", "hook", "hunch", "hunt", "isle", "joiner", "key", "kill", "king", "knife", "lake", "lock", "main", "maker", "mantle", "marsh", "mash", "merge", "mist", "moat", "mont", "moon", "most", "mount", "pearl", "pen", "point", "river", "ruby", "sail", "sand", "satyr", "saw", "seed", "september", "serpent", "shackle", "shadow", "shell", "shine", "shire", "shout", "shrine", "shrine", "sigh", "sill", "slip", "smith", "song", "spin", "star", "sun", "switch", "tale", "tell", "thicket", "thorn", "throne", "ton", "tree", "turn", "turtle", "twig", "vale", "ville", "wall", "wax", "wick", "winter" ]

    genBasicName :: IO String
    genBasicName = do
      adj <- pickFrom adjectives
      noun <- pickFrom nouns
      return (adj ++ noun)

