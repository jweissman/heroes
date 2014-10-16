module FantasyLanguage where
    import Data.List
    import Control.Monad
    import System.Random
    import Probability()

    data LanguageType = CommonSpeech | ElvishSpeech | DwarvenSpeech | HalflingSpeech | OrcishSpeech | InfernalLanguage | CelestialLanguage | ReptilianSpeech | InsectSpeech | SacredLanguage | DeadLanguage | FaeSpeech | SilentLanguage | BlackSpeech | AvianSpeech | AstralSpeech | SecretLanguage | AncientSpeech | AlienSpeech | OutsiderSpeech | TrueSpeech
      deriving (Eq, Show, Read, Enum, Bounded)

    data Language = LanguageType [(String,String)]
      deriving (Eq, Show, Read)

    -- compose a mock language translating our adjectives and nouns (so we can translate/"encrypt" placenames etc)
    -- genLanguage

    humanizedLanguages :: [LanguageType] -> String
    humanizedLanguages langs = intercalate ", " (map show langs)

    genLangs n = replicateM n (randomIO :: IO LanguageType)
