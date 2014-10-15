module Alignment where  
    import StringHelpers
    import System.Random
    import Probability()
    
    data MoralAlignment = Good | MoralNeutral | Evil
      deriving (Eq, Show, Read, Enum, Bounded)

    data EthicalAlignment = Lawful | EthicalNeutral | Chaotic
      deriving (Eq, Show, Read, Enum, Bounded)

    data Alignment = Alignment { ethical :: EthicalAlignment
                               , moral :: MoralAlignment } deriving (Eq, Show, Read)

    aligned :: EthicalAlignment -> MoralAlignment -> Alignment
    aligned e m = Alignment { ethical = e, moral = m }

    genAlignment :: IO Alignment
    genAlignment = do
      morality            <- randomIO :: IO MoralAlignment
      ethics              <- randomIO :: IO EthicalAlignment
      return (aligned ethics morality)

    describeAlignment :: EthicalAlignment -> MoralAlignment -> String
    describeAlignment Lawful  Good                    = "crusader"
    describeAlignment EthicalNeutral Good             = "benefactor"
    describeAlignment Chaotic Good                    = "rebel"
    describeAlignment Lawful MoralNeutral             = "judge"
    describeAlignment EthicalNeutral MoralNeutral     = "undecided"
    describeAlignment Chaotic MoralNeutral            = "free spirit"
    describeAlignment Lawful Evil                     = "dominator"
    describeAlignment EthicalNeutral Evil             = "malefactor"
    describeAlignment Chaotic Evil                    = "destroyer"

    humanizedAlignment :: Alignment -> String
    humanizedAlignment alignment = d
      where d = capWord (describeAlignment (ethical alignment) (moral alignment))
