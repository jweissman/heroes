module BasicNames where

    import Probability

    adjectives :: [String]
    adjectives = [ "able", "adamant", "airy", "antic", "apt", "argent", "astral", "avid", "baleful", "big", "bitter", "black", "blue", "bright", "broad", "clean", "cold", "comely", "cool", "crooked", "dainty", "dank", "darkling", "dead", "deep", "diamond", "doughty", "dour", "dreary", "dry", "easy", "faint", "fell", "fey", "fierce", "fine", "flat", "fleet", "foul", "full", "garnet", "graven", "gray", "green", "happy", "heavy", "high", "hot", "indigo", "iron", "kind", "light", "little", "low", "mad", "misty", "more", "most", "odd", "oft", "outworn", "proud", "quick", "quiet", "red", "rough", "round", "shallow", "shy", "sleepy", "slow", "sour", "square", "steel", "still", "straight", "sweet", "swift", "twisted", "under", "violet", "wan", "worn", "warm", "wet", "white", "wide", "wood", "yonder" ]

    nouns :: [String]
    nouns = [ "adder", "ark", "ash", "attercop", "aught", "aumbry", "awn", "baker", "baldric", "bar", "barge", "barrel", "bay", "bear", "beast", "berry", "bide", "birch", "bole", "bond", "bower", "brace", "brake", "branch", "breeze", "bridge", "brood", "brook", "brush", "byre", "champ", "chill", "chime", "chine", "circle", "coffer", "count", "court", "creek", "cross", "crystal", "curve", "dale", "dastard", "defile", "dell", "device", "dingle", "dome", "don", "down", "dromund", "drone", "eaves", "ell", "essay", "eyot", "face", "fall", "fane", "fastness", "fief", "field", "flag", "flaw", "flax", "flay", "fog", "fold", "fork", "forth", "fosse", "frith", "frog", "girdle", "glen", "grass", "guest", "hill", "hint", "hold", "hole", "hollow", "home", "hook", "hue", "hunch", "hunt", "ire", "isle", "jaunt", "joiner", "key", "kill", "king", "knife", "lash", "lay", "lark", "lake", "lass", "land", "lea", "lock", "log", "maid", "main", "maker", "mantle", "mark", "marsh", "mash", "may", "mead", "meadow", "merge", "miss", "mist", "moat", "mont", "moon", "moot", "morn", "morning", "mount", "muster", "name", "need", "nick", "nock", "non", "norm", "north", "notch", "nugget", "pace", "pearl", "pen", "point", "river", "ruby", "sail", "sand", "satyr", "saw", "seed", "september", "serpent", "shackle", "shadow", "shell", "shine", "shire", "shout", "shrine", "shrine", "sigh", "sill", "slip", "smith", "song", "spin", "star", "sun", "switch", "tale", "tell", "thicket", "thorn", "throne", "ton", "tree", "trove", "turn", "turtle", "twig", "twist", "umbel", "urchin", "vale", "vigil", "ville", "wall", "waver", "ware", "wax", "web", "wick", "wile", "winter", "yoke", "yore", "zenith" ]

    genBasicName :: IO String
    genBasicName = do
      first  <- pickFrom (nouns ++ adjectives)
      second <- pickFrom nouns
      pickFrom [second, first ++ second]
