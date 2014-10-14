module CharacterSheet where
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

    characterDescription :: CharacterSheet -> String
    characterDescription character = 
      "\n  Race: " ++ r ++ 
      "\n  Job: " ++ j ++ 
      "\n  Alignment: " ++ a ++ 
      "\n" ++
      "\n  Skills" ++
      "\n  ------" ++ 
      "\n    * speaks " ++ l ++
      "\n    * skilled in " ++ sk ++
      "\n" ++
      "\n  Statistics" ++
      "\n  ----------" ++ 
      "\n" ++
      st

      where a = humanizedAlignment (alignment character)
            r = humanizedRace (race character)
            j = humanizedJob (job character)
            st = humanizedStats (stats character)
            l = humanizedLanguages (languages character)
            sk = humanizedSkills (skills character)

    displayCharacterSheet :: CharacterSheet -> String
    displayCharacterSheet character = "=====================================================================\n" ++ characterDescription character ++ "\n\n"


    -- generate skeletal character sheet
    genCharacterSheet :: IO CharacterSheet
    genCharacterSheet = do
      rolledValues <- 6 `d` 20
      racialModifier <- randomIO :: IO RacialModifier
      sp <- randomIO :: IO Species
      morality <- randomIO :: IO MoralAlignment
      ethics <- randomIO :: IO EthicalAlignment
      professionalSubtype <- randomIO :: IO ProfessionalSubtype
      prof <- randomIO :: IO Profession
      langs <- sequence $ replicate 3 (randomIO :: IO Language)
      skills    <- sequence $ replicate 5 (randomIO :: IO Skill)

      return (let r = Race { modifier = racialModifier, species = sp }
                  st = (buildStats rolledValues)
                  a = (aligned ethics morality)
                  j = Job { profession = prof, subtype = professionalSubtype }
                  l = (nub (CommonSpeech:langs))
                  sk = (nub skills)
                  in CharacterSheet { race = r, alignment = a, job = j, stats = st, languages = l, skills = sk })

