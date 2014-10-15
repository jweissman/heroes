module FantasyStats where
    import StringHelpers
    
    data Stats = Stats { strength :: Integer
                       , constitution :: Integer
                       , charisma :: Integer
                       , intelligence :: Integer
                       , wisdom :: Integer
                       , dexterity :: Integer } 
      deriving (Eq, Show, Read)

    buildStats :: [Integer] -> Stats              
    buildStats (str:con:cha:int:wis:dex:xs) =  Stats { strength = str
                                                     , constitution = con
                                                     , charisma = cha
                                                     , intelligence = int
                                                     , wisdom = wis
                                                     , dexterity = dex }

                                                
    --data StatQuality = Terrible | Poor | Average | ... ?

    judgeStat :: Integer -> String
    judgeStat stat
      | stat <= 5  = "terrible"
      | stat <= 8  = "poor"
      | stat <= 12 = "average"
      | stat <= 15 = "good"
      | stat <= 18 = "great"
      | stat <= 19 = "epic"
      | stat >  19 = "deity"

    terrible stat = judgeStat stat == "terrible"
    poor     stat = judgeStat stat == "poor"
    average  stat = judgeStat stat == "average" || good stat
    good     stat = judgeStat stat == "good" || great stat
    great    stat = judgeStat stat == "great" || epic stat
    epic     stat = judgeStat stat == "epic" || deity stat
    deity    stat = judgeStat stat == "deity"


    humanizeStat :: String -> Integer -> String
    humanizeStat name stat = "\n    " ++ capWord name ++ ": " ++ show stat ++ " (" ++ judgeStat stat ++ ")" 

    humanizeCon = humanizeStat "constitution"
    humanizeCha = humanizeStat "charisma"
    humanizeWis = humanizeStat "wisdom"
    humanizeDex = humanizeStat "dexterity"
    humanizeStr = humanizeStat "strength"
    humanizeInt = humanizeStat "intelligence"


    humanizedStats :: Stats -> String
    humanizedStats stats  = humanizeCon con ++    
                            humanizeCha cha ++    
                            humanizeWis wis ++    
                            humanizeInt int ++    
                            humanizeDex dex ++    
                            humanizeStr str
      where str = strength stats
            con = constitution stats
            cha = charisma stats
            wis = wisdom stats
            int = intelligence stats
            dex = dexterity stats
