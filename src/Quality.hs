module Quality where
    import System.Random
    import Probability

    -- should 
    data Quality = Unknown | Terrible | Poor | Average | Good | Great | Superb | Deity
      deriving (Ord, Eq, Show, Read, Bounded, Enum)

    genQuality = randomIO :: IO Quality

    judge val 
      | val <  5  = Terrible
      | val <  8  = Poor
      | val <  12 = Average
      | val <  15 = Good
      | val <  18 = Great
      | val <  20 = Superb
      | val >= 20 = Deity
      | otherwise  = Unknown

    terrible val = judge val == Terrible
    poor     val = judge val <= Poor      -- || terrible val

    average  val = judge val >= Average 
    good     val = judge val >= Good      -- || great val
    great    val = judge val >= Great     -- || epic val
    epic     val = judge val >= Superb    -- || deity val
    deity    val = judge val == Deity

