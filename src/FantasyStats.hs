module FantasyStats where
    import Data.Maybe
    import Data.List
    import System.Random
    import StringHelpers
    import Probability
    import Quality

    data StatisticType = Strength | Constitution | Charisma | Intelligence | Wisdom | Dexterity | 
			 Perception | Patience |
		         Spirit | Pride | Energy | Will | Focus | Logic
      deriving (Eq, Show, Read, Bounded, Enum)

    allStats = [ Strength .. ]

    data Statistic = Statistic StatisticType Integer
      deriving (Eq, Show, Read)

    rollStat statType = do
      statValue <- 1 `d` 20
      return (Statistic statType (head statValue))

    genStat = do
      statType <- randomIO :: IO StatisticType
      return (rollStat statType)

    genStats = mapM rollStat allStats



    getStat ofType stats = fromMaybe (Statistic Strength 0) tryStat
      where tryStat = find statMatches stats where statMatches (Statistic t _) = t == ofType 


    getStatValue (Statistic _ val) = val

    -- mimic record accessors...
    strength     ss = getStatValue (getStat Strength ss)
    constitution ss = getStatValue (getStat Constitution ss)
    charisma 	 ss = getStatValue (getStat Charisma ss)
    intelligence ss = getStatValue (getStat Intelligence ss)
    wisdom 	 ss = getStatValue (getStat Wisdom ss)
    dexterity    ss = getStatValue (getStat Dexterity ss)

    perception   ss = getStatValue (getStat Perception ss)
    patience     ss = getStatValue (getStat Patience ss)

    spirit       ss = getStatValue (getStat Spirit ss)
    pride        ss = getStatValue (getStat Pride ss)
    energy       ss = getStatValue (getStat Energy ss)
    will         ss = getStatValue (getStat Will ss)
    focus        ss = getStatValue (getStat Focus ss)
    logic        ss = getStatValue (getStat Logic ss)


    --data StatisticRestriction = StatisticRestriction StatisticType Quality
    --  deriving (Eq, Show, Read)


    judgeStat (Statistic _ val) = judge val
    
    humanizeStat :: Statistic -> String
    humanizeStat (Statistic t v) = "\n    " ++ capWord (show t) ++ ": " ++ show v ++ " (" ++ (show (judge v)) ++ ")" 

    humanizedStats :: [Statistic] -> String
    humanizedStats stats  = concat (map humanizeStat stats)
    
