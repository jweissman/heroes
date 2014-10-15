module FantasyLanguage where
    import Data.List

    data Language = CommonSpeech | ElvishSpeech | DwarvenSpeech | InfernalLanguage | CelestialLanguage | ReptilianSpeech | InsectSpeech | SacredLanguage | DeadLanguage | FaeSpeech | SilentLanguage | BlackSpeech | AvianSpeech | AstralSpeech | SecretLanguage | AncientSpeech | AlienSpeech | OutsiderSpeech | TrueSpeech
      deriving (Eq, Show, Read, Enum, Bounded)

    humanizedLanguages :: [Language] -> String
    humanizedLanguages langs = intercalate ", " (map show langs)
