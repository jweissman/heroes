module CharacterSheet where
    import Control.Monad
    import Data.List
    import FantasyRace
    import FantasyProfession
    import FantasyStats
    import FantasyLanguage
    import Skill
    import Alignment
    import StringHelpers

    data CharacterSheet = CharacterSheet { race :: Race
                                         , job :: Job
                                         , alignment :: Alignment
                                         , stats :: [Statistic]
                                         , languages :: [LanguageType]
                                         , characterSkills :: [Skill] }
      deriving (Eq, Show, Read)

    describeSkillsAndLanguages :: [Skill] -> [LanguageType] -> String
    describeSkillsAndLanguages sks langs = 
      "\n  Skills" ++
      "\n  ------" ++ 
      "\n" ++
      "\n    * speaks " ++ l ++
      "\n    * " ++ sk where sk = humanizedSkills sks
                             l = humanizedLanguages langs

    describeStatistics :: [Statistic] -> String
    describeStatistics sts =
      "\n  Statistics" ++
      "\n  ----------" ++ 
      "\n" ++ humanizedStats sts

    characterDescription :: CharacterSheet -> String
    characterDescription character = st ++ "\n\n" ++ sl
      where sl = describeSkillsAndLanguages (characterSkills character) (languages character)
            st = describeStatistics (stats character)

    

    -- generate skeletal character sheet
    genCharacterSheet :: IO CharacterSheet
    genCharacterSheet = do
      r                   <- genRace
      a                   <- genAlignment
      j                   <- genJob
      --sks                 <- genSkills 5
      langs               <- genLangs 3 --(intelligence st)
      sks 		  <- replicateM 5 genSkill
      sts 		  <- genStats
      return (let l  = nub (CommonSpeech:langs)
                  in CharacterSheet { race = r, alignment = a, job = j, stats = sts, languages = l, characterSkills = sks }) -- nub (genSkills 5) })

