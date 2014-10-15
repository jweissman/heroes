module CharacterSheet where
    import Control.Monad
    import Data.List
    import System.Random
    import Probability
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
      "\n\n" ++ st ++ "\n" ++ sl
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
      rolledValues        <- 6 `d` 20
      
      racialModifier      <- randomIO :: IO RacialModifier
      sp                  <- randomIO :: IO Species
      morality            <- randomIO :: IO MoralAlignment
      ethics              <- randomIO :: IO EthicalAlignment
      
      professionalSubtype <- randomIO :: IO ProfessionalSubtype
      prof                <- randomIO :: IO Profession

      langs               <- replicateM 2 (randomIO :: IO Language)                                                               
      sks                 <- replicateM 8 (randomIO :: IO Skill) 

      return (let r = Race { modifier = racialModifier, species = sp }
                  st = buildStats rolledValues
                  a  = aligned ethics morality
                  j  = Job { profession = prof, subtype = professionalSubtype }
                  l  = nub (CommonSpeech:langs)
                  sk = nub sks
                  in CharacterSheet { race = r, alignment = a, job = j, stats = st, languages = l, skills = sk })

