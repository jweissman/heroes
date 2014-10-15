module CharacterSheet where
    import Control.Monad
    import Data.List
    import System.Random
    import FantasyRace
    import FantasyProfession
    import FantasyStats
    import FantasyLanguage
    import Skill
    import Alignment

    data CharacterSheet = CharacterSheet { race :: Race
                                         , job :: Job
                                         , alignment :: Alignment
                                         , stats :: Stats
                                         , languages :: [Language]
                                         , skills :: [Skill] }
      deriving (Eq, Show, Read)
  
    describeSkillsAndLanguages skills langs = 
      "\n  Skills" ++
      "\n  ------" ++ 
      "\n" ++
      "\n    * speaks " ++ l ++
      "\n    * skilled in " ++ sk where sk = humanizedSkills skills
                                        l = humanizedLanguages langs

    describeStatistics stats =
      "\n  Statistics" ++
      "\n  ----------" ++ 
      "\n" ++ humanizedStats stats

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

    hr = "=====================================================================\n"

    displayCharacterSheet :: CharacterSheet -> String
    displayCharacterSheet character = hr ++ description ++ "\n\n"
      where description = characterDescription character


    

    -- generate skeletal character sheet
    genCharacterSheet :: IO CharacterSheet
    genCharacterSheet = do
      st                  <- genStats
      r                   <- genRace
      a                   <- genAlignment
      j                   <- genJob
      sks                 <- genSkills 8
      langs               <- genLangs 2
      return (let l  = nub (CommonSpeech:langs)
                  sk = nub sks
                  in CharacterSheet { race = r, alignment = a, job = j, stats = st, languages = l, skills = sk })

