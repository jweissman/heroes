module FantasyProfession where
    import System.Random
    import Probability

    data Profession = Aristocrat | Commoner | Warrior | Soldier | Archer | Bard | Trader | Pilgrim | Thief | Apothecary | Hunter | Cleric | Ranger | Monk | Sorceror | Paladin | Healer | Rogue | Merchant | Seeker | Archmage | Oracle | Priest | Wizard | Knight | Shadowmage | Commander | Diplomat | Ninja | Prophet | Pirate | Swashbuckler | Samurai | Warlord | Sage | Gambler | Scout | Assassin | WitchDoctor | Templar | Sniper | Trapper | Dancer | Dervish | Alchemist | Trickster | Inquisitor | Missionary | Trackerss
        deriving (Eq, Show, Read, Enum, Bounded)

    data ProfessionalSubtype = Academic | Colonial | Rural | Telepathic | Drifter | Novitiate | Infernal | Celestial | Nomad | Forester | Herald | Criminal | Royal | Clandestine | Military | Guerrilla | Shadow | Dread | Scavenger | Feral | Primitive | Dilettante | Outcast | Celebrity | Eldritch
      deriving (Eq, Show, Read, Enum, Bounded)

    data Job = Job { profession :: Profession, subtype :: ProfessionalSubtype }
      deriving (Eq, Show, Read)

    humanizedJob :: Job -> String
    humanizedJob job = show (subtype job) ++ " " ++ show (profession job)

    genJob = do
      professionalSubtype <- randomIO :: IO ProfessionalSubtype
      prof                <- randomIO :: IO Profession
      return Job { profession = prof, subtype = professionalSubtype }
