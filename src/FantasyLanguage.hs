module FantasyLanguage where
    import Data.List
    import Control.Monad
    import System.Random
    import Probability()

    data Language = CommonSpeech | ElvishSpeech | DwarvenSpeech | HalflingSpeech | OrcishSpeech | InfernalLanguage | CelestialLanguage | ReptilianSpeech | InsectSpeech | SacredLanguage | DeadLanguage | FaeSpeech | SilentLanguage | BlackSpeech | AvianSpeech | AstralSpeech | SecretLanguage | AncientSpeech | AlienSpeech | OutsiderSpeech | TrueSpeech
      deriving (Eq, Show, Read, Enum, Bounded)

    humanizedLanguages :: [Language] -> String
    humanizedLanguages langs = intercalate ", " (map show langs)

    genLangs n = replicateM 2 (randomIO :: IO Language)