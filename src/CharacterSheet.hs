module CharacterSheet where
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
                                         , stats :: Stats
                                         , languages :: [LanguageType]
                                         , skills :: [Skill] }
      deriving (Eq, Show, Read)

    describeSkillsAndLanguages :: [Skill] -> [LanguageType] -> String
    describeSkillsAndLanguages sks langs = 
      "\n  Skills" ++
      "\n  ------" ++ 
      "\n" ++
      "\n    * speaks " ++ l ++
      "\n    * skilled in " ++ sk where sk = humanizedSkills sks
                                        l = humanizedLanguages langs

    describeStatistics :: Stats -> String
    describeStatistics sts =
      "\n  Statistics" ++
      "\n  ----------" ++ 
      "\n" ++ humanizedStats sts

    characterDescription :: CharacterSheet -> String
    characterDescription character = 
      "\n       Race: " ++ r ++ 
      "\n        Job: " ++ j ++ 
      "\n  Alignment: " ++ a ++ 
      "\n\n" ++ st ++ "\n\n" ++ sl
      where a  = humanizedAlignment (alignment character)
            r  = humanizedRace (race character)
            j  = humanizedJob (job character)
            sl = describeSkillsAndLanguages (skills character) (languages character)
            st = describeStatistics (stats character)

    

    -- generate skeletal character sheet
    genCharacterSheet :: IO CharacterSheet
    genCharacterSheet = do
      st                  <- genStats
      r                   <- genRace
      a                   <- genAlignment
      j                   <- genJob
      sks                 <- genSkills 5
      langs               <- genLangs 3 --(intelligence st)
      return (let l  = nub (CommonSpeech:langs)
                  sk = nub sks
                  in CharacterSheet { race = r, alignment = a, job = j, stats = st, languages = l, skills = sk })

