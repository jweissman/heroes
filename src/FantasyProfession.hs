module FantasyProfession where

    data Profession = Aristocrat | Commoner | Warrior | Soldier | Bard | Trader | Pilgrim | Thief | Apothecary | Hunter | Cleric | Ranger | Monk | Sorceror | Paladin | Healer | Rogue | Merchant | Seeker | Archmage | Oracle | Priest | Wizard | Knight | Shadowmage | Commander | Diplomat | Ninja | Prophet | Pirate | Swashbuckler | Samurai | Warlord | Sage | Gambler | Scout | Assassin | WitchDoctor | Templar | Sniper | Trapper | Dancer | Dervish | Alchemist | Trickster | Inquisitor | Missionary
        deriving (Eq, Show, Read, Enum, Bounded)

    data ProfessionalSubtype = Academic | Colonial | Rural | Telepathic | Drifter | Novitiate | Infernal | Celestial | Nomad | Forester | Herald | Criminal | Royal | Clandestine | Military | Shadow | Dread | Scavenger | Feral | Primitive | Dilettante | Outcast | Celebrity | Eldritch
      deriving (Eq, Show, Read, Enum, Bounded)

    data Job = Job { profession :: Profession, subtype :: ProfessionalSubtype }
      deriving (Eq, Show, Read)

    humanizedJob job = show (subtype job) ++ " " ++ show (profession job)
