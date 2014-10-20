module Precondition where
    import Alignment
    import FantasyStats
    import Quality

    data Precondition = StatisticRestriction StatisticType Quality | MoralRestriction MoralAlignment | EthicalRestriction EthicalAlignment
      deriving (Eq, Show, Read)

    restrictStat statType qual = StatisticRestriction statType qual

    statRestrictionApplies (StatisticRestriction statType minQual) stats = qual >= minQual
      where qual = (judgeStat statValue)
	    statValue = getStat statType stats

    strengthRestriction q = StatisticRestriction Strength q
    wisdomRestriction q   = StatisticRestriction Wisdom q


    restrictMoralAlignment m = MoralRestriction m
    restrictEthicalAlignment e = EthicalRestriction e

    alignRestrictionApplies (EthicalRestriction e) a = (ethical a) == e
    alignRestrictionApplies (MoralRestriction m) a = (moral a) == m


        --applyRestriction sts sheet restriction = 
    --professionalRestrictionApplies (EthicalRestriction e) sheet = alignRestrictionApplies e (ethical (alignment sheet))
    --professionalRestrictionApplies (MoralRestriction m) sheet   = alignRestrictionApplies m (moral (alignment sheet))

    requiresGoodStrength = strengthRestriction Good
    requiresSuperbWisdom = wisdomRestriction Superb

    --applyProfessionalRestrictions professions sheet = 

    requiresLawful = restrictEthicalAlignment Lawful
    requiresEvil   = restrictMoralAlignment Evil
    requiresGood   = restrictMoralAlignment MoralGood


