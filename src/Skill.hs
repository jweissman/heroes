module Skill where
    import Data.List
    
    
    data Skill   = Will | Athletics | Acrobatics | Gymnastics | Brawling | Swimming | Climbing | Shooting | Ride | Focus | Lore | Astrology | Botany | Mechanics | Logic | Mathematics | Philosophy | Theology | Law | FirstAid | Medicine | Bartering | Bluffing | Appraising | Pandering | Economics | Leatherworking | Blacksmithing | Armory | Brewing | Farming | Fletching | Energy | Alchemy | AnimalRapport | Spellcraft | Enchanting | Mindreading | Creativity | Aesthetics | Sculpture | Singing | Acting | Performance | Prestidigitation | Speech | Conversation | Rhetoric | Reading | Writing | Speaking | Storytelling | Observation | Persuasion | Provocation | Administration | Intimidation | Inspiration | Arbitrartion | Perception | Survival | Tactics | DisablingTraps | CreatureLore | Herbalism | Deception | Stealing | Surveillance | Sneaking | Cryptography | Spirit | Healing | Blessing | Prayer | OccultLore | Patience
      deriving (Eq, Show, Read, Bounded, Enum)

    --data SkillDegree = Novice | Adept | Adequate | Expert | Master
    --data Skill = Skill { kind :: SkillType, level :: SkillDegree }

    humanizedSkills :: [Skill] -> String
    humanizedSkills sks = intercalate ", " (map show sks)