ccdr.lexicon <- function() {

  #' Lexicon of economic crisis
  #' @description Provide the lexicon of economic crisis
  #' @return a list of categories with the corresponding words
  #'  associated
  #' @export
  #' @example lexicon()

  key_words <- list()


  #--------
  ## EXOGENEOUS SHOCKS: NON ECONOMIC
  #--------

  # State-based conflict, non-state conflict and one-sided violence (difficult to divide).
  key_words[["Wars"]] <- c(
    "war damage",
    "war-damaged",
    "insurgency crisis",
    "security crisis",
    "civil conflict",
    "civil war",
    "ensuing conflict",
    "armed conflict",
    "armed internal conflict",
    "armed domestic conflict",
    "oingoing conflict",
    "violent conflict",
    "atlantic conflict",
    "internal conflict",
    "regional conflict",
    "conflicts in the region",
    "conflict zone",
    "conflict regions",
    "military coup",
    "military take-over",
    "coup d'etat",
    "escalated attacks",
    "breakdown of cease-fire",
    "ethnic rivalries",
    "terrorist attacks",
    "terrorism",
    "guerilla offensive",
    "continuing external aggression",
    "incidence de la guerre"
  )

  key_words[["Wars_confusing"]] <- c(
    "since the end of the civil war"
  )


  # Stemming might improve this category.
  key_words[["Migration"]] <- c(
    "refugee",
    "migrant",
    "inward migration",
    "population inflow",
    "asylum",
    "immigrant",
    "immigration"
  )


  key_words[["Natural_disaster"]] <- c(
    "flood",
    "drought",
    "rainfall",
    "torrential rains",
    "natural calamities",
    "power shortage",
    "natural disaster",
    "earthquake",
    "hurricane",
    "typhoon",
    "cyclone",
    "calamity",
    "adverse weather conditions",
    "tsunami"
  )

  key_words[["Natural_disaster_confusing"]] <- c(
    "easing of drought"
  )

  # Problem of epidemic+virus double counting is avoided with tokenization by sentence.
  key_words[["Epidemics"]] <- c(
    "epidemic",
    "epidemia",
    "pandemia",
    "pandemic",
    "virus",
    "\\sflu\\s",
    "relapsing fever",
    "typhoid fever",
    "leishmaniasis",
    "dengue",
    "mumps",
    "meningitis",
    "poliomyelitis",
    "measles",
    "zika",
    "encephalitis",
    "\\ssars\\s",
    "\\smers\\s",
    "nipah",
    "vcjd",
    "\\shiv\\s",
    "hiv/aids",
    "typhus",
    "hepatitis",
    "h1n1",
    "h5n1",
    "ebola",
    "\\ssida\\s",
    "rotavirus",
    "\\slyme\\s",
    "hepatite",
    "chikungunya",
    "dysenteria",
    "avian influenza",
    "zoonoses",
    "infectious disease",
    "dysentery",
    "smallpox",
    "yellow fever",
    "cholera",
    "malaria",
    "coronavirus",
    "covid 19",
    "\\splague\\s"
  )

  key_words[["Commodity_crisis"]] <- c(
    "oil crisis",
    "rice crisis",
    "crop crisis",
    "crop failure",
    "commodity crisis",
    "energy crisis",
    "cotton crisis",
    "crisis in the cotton",
    "severe shortages{1} of rice",
    "fall in prices of raw materials",
    "price of copper continue to drop",
    "swing in copper price",
    "weakness in the copper price",
    "adverse movement in the price of copper",
    "decline in coffee prices",
    "decline in international coffee prices",
    "drop in world coffee price",
    "fell of agricultural prices",
    "tourism.*suffer",
    "terms-of-trade shock",
    "deterioration in the terms of trade",
    "deteriorating terms of trade",
    "adverse terms of trade",
    "terms of trade loss",
    "unfavorable terms of trade",
    "severe drop in terms of trade",
    "severe terms of trade drop",
    "severe terms of trade shock",
    "significant terms of trade loss",
    "sharp fall in its terms of trade",
    "large terms of trade loss",
    "adverse movement in the terms of trade",
    "terms of trade were adversely affected",
    "dependenceon oil-related revenue",
    "budgetary dependency on oil revenue",
    "increase in world oil prices",
    "drop in world coffee price",
    "oil price increase",
    "fluctuations in oil prices",
    "increase in petroleum price"
  )

  key_words[["Political_crisis"]] <- c(
    "political turmoil",
    "internal security situation",
    "political atmosphere",
    "political crisis",
    "political uncertainty",
    "political instability",
    "political transition spillovers ",
    "political turn-over",
    "policies risks",
    "political turmoil",
    "political risk",
    "unstable political",
    "political instability",
    "poor governance",
    "disturbed political conditions",
    "political and economic developments",
    "political and security situation",
    "economic and political situation",
    "political crisis",
    "unsettled political  situation",
    "political tensions",
    "geopolitical events",
    "policy-related uncertainty",
    "policy related uncertainty",
    "geopolitical risk",
    "election related uncertainty",
    "election related uncertainties",
    "governance issues",
    "complex geopolitical situation",
    "geopolitical tensions",
    "geopolitical turmoil",
    "weak governance",
    "adverse geopolitical events",
    "adverse geopolitical",
    "unexpected political events",
    "revolution",
    "uncertain policies",
    "uncertainty about policy",
    "political contagion",
    "euro exit",
    "exit of the eurozone",
    "uncertain national election",
    "political transition",
    "political pressures",
    "change of administration",
    "risks linked to the electoral calendar ",
    "uncertainty surrounding the outcome of the presidential election",
    "uncertainty regarding the political transition",
    "domestic political developments",
    "politial risk",
    "facilitate an orderly transition to a new administration",
    "uncertainty about the continuity of policies",
    "uncertainty regarding future policies",
    "unstable political environment",
    "military coup",
    "coup d'etat",
    "annulment of the election",
    "parliamentary upheavals",
    "critical political.*juncture"
  ) # political and economical

  key_words[["Social_crisis"]] <- c(
    "social risk",
    "social.*turmoil", # originally, social and political turmoil
    "social disruption",
    "social climate as deteriorate",
    "social tension",
    "protest",
    "railroad-transport strike",
    "deteriorating social climate",
    "blockade",
    "social unrest",
    "walkouts",
    "events of may-june 1968" # removed strike: misleading and in any case accompanied by other words.
  )


  key_words[["Labormarket_boom"]] <- c(
    "low unemployment",
    "decline in unemployment",
    "wage pressure",
    "rising employment"
  )

  key_words[["Labormarket_crisis"]] <- c(
    "unemployment rate jumped",
    "increased layoffs"
  )

  #--------
  ## EXOGENEOUS SHOCKS; ECONOMIC
  #--------



  key_words[["Nuclear_accident"]] <- c(
    "nuclear disaster",
    "nuclear accident",
    "nuclear damage"
  )

  key_words[["Cyber_attack"]] <- c(
    "cyber attack",
    "hacking",
    "hackers"
  )


  key_words[["Credit_boom"]] <- c(
    "strong credit growth",
    "credit expansion",
    "rapid credit growth",
    "credit-driven domestic boom",
    "vigorous credit growth",
    "rapid accumulation of household debt"
  )


  key_words[["Housing_boom"]] <- c(
    "real estate boom",
    "rising housing price",
    "increase in house price",
    "increase in real estate price"
  )

  key_words[["Housing_crisis"]] <- c(
    "home prices have been declining",
    "drops in real estate prices",
    "house price trends",
    "home-price overvaluation",
    "real house prices declining",
    "cooled the property market",
    "foreclosures",
    "house price inflation",
    "house-price inflation",
    "foreclosures",
    "bust in housing",
    "home-price declines",
    "house-price declines",
    "house prices fall",
    "housing market correction",
    "stalling house prices",
    "slower house price",
    "slowing housing wealth",
    "Declines in house prices",
    "headwinds from housing",
    "problems in housing",
    "housing downturn",
    "cooling housing market",
    "cooling in the housing market",
    "change in housing wealth",
    "deceleration in house prices",
    "slowdown in the housing market",
    "housing slowdown",
    "house prices seemed overvalued",
    "housing boom",
    "Falling house prices",
    "spillovers from the housing market",
    "spillovers from housing",
    "housing market weakness",
    "slowdown in the housing market",
    "subprime",
    "residential investment has declined rapidly"
  )

  key_words[["Banking_crisis"]] <- c(
    "bank resolution",
    "bank crisis",
    "Banking sector restructuring",
    "restructuringof nonperforming loans",
    "undercapitalized banking system",
    "weak bank capitalization",
    "reorganization of the banking sector",
    "restructuring of the banking",
    "fragility of the banking sector",
    "fragile banking sector",
    "banking crisis",
    "banking system restructuring",
    "insolvent banks",
    "insovlvent banking sector",
    "bailout",
    "crisis in the banking sector",
    "take over of private banks",
    "private banks taken over",
    "recapitalize private banks",
    "collapse of the banking sector",
    "increase in nonperforming loans",
    "recapitalization of the banks",
    "recapitalizing the banking system",
    "recapitalizing the banking sector",
    "banking system collapsed",
    "additional nonperforming loans",
    "collapsed in the banking system",
    "banking system stability",
    "pressure on the banking",
    "bankrun",
    "bank recapitalization",
    "deteriorating credit quality",
    "recapitalization",
    "bank restructuring",
    "recapitalize private financial institutions",
    "confidence in the domestic banking system",
    "strengthen bank supervision",
    "financial support package"
  )

  key_words[["Banking_crisis_severe"]] <- c(
    # Significant distress in the banking system: bank runs, losses in the banking system and/or bank liquiditations.
    "banking crisis",
    "bank failures",
    "run on*.deposits",
    "capital of the banking system insurance scheme.*exhausted",
    "crisis in the banking sector",
    "insovlvent banking sector",
    "insolvent banking sector",
    "banking system collapsed",
    "collapse of the banking system",
    "collapse of the banking system",
    "sizable outflow of deposits",
    "fears of deposits confiscations",
    "sustained deposit outflows",
    "continued withdrawals of.*deposits",
    "sharp decline in deposits",
    "problems of illiquidity and insolvency in the banking system",
    "decrease in confidence in the banking system",
    "paralysis of the banking system",
    "margin calls",
    "liquidity crunch in the banking system",
    # Significant banking policy intervention: deposit freezes, bank holidays, significant bank nationalization,
    # significant guarantees put in place, significant asset purchases, bank restructuring fiscal cost. We do not include bank supervision.
    "bank holiday",
    "liquidated a number of privately owned",
    "immediate takeover",
    "liquidating.*insolvent banks",
    "recapitalization program",
    "fiscal costs of the restructuring",
    "comprehensive restructuring",
    "bank recapitalization scheme",
    "emergency credits",
    "bailout",
    "lowered legal reserve requirements",
    "lowering the legal reserve requirements",
    "reduced.* minimum legal reserve requirement",
    "hold lower than required reserves",
    "operations of banks.*suspended",
    "suspend operation of banks",
    "support.*provided.*to banks of systemic importance",
    "backing of.*deposits",
    "withdraw the licenses of major.*banks"
  )

  # Are banking crisis all in the database? can't see 1981 banking crisis in Chile.

  key_words[["Financial_crisis"]] <- c(
    "financial stability crisis",
    "international monetary crisis",
    "crisis in financial market",
    "financial risks",
    "turmoil in financial markets",
    "turmoil in international financial markets",
    "volatility in financial markets",
    "restore the strength of the financial sector",
    "unfolding financial crisis",
    "global market sell-off",
    "global financial shock",
    "financial shock",
    "financial contagion",
    "financial crisis",
    "collapse of financial markets",
    "fire sells",
    "collapse of equity prices",
    "financial market panic",
    "global financial turbulence",
    "viability and health of the financial sector"
  )

  key_words[["Inflation_crisis"]] <- c(
    "high.{0,10}inflation",
    "high rate of inflation",
    "severe.{0,10}inflation",
    "large.{0,10}inflation",
    "virulence.{0,10}inflation",
    "unprecedented.{10}inflation", # adj. severity + inflation (quantifier to take into account headline) - check how they use rete of inflation
    "sharp.{0,2}increase in domestic prices",
    "large increase in.{0,10}prices",
    "high pressure on.{0,10}prices", # adj. severity + increase in domestic prices (quantifier 1 for sharp because of spelling mistakes sharpe)
    # (quantifier 10 for domestic)
    "inflation.*critical",
    "inflation.*unprecedented levels", # inflation + has reached +... special character to control for different patterns
    "the rate of inflation accelerated sharply", # acceleration - find pattern for this
    "inflation crisis",
    "hyperinflation", # extreme
    "large monetary creation" # involving monetary creation (also here to find pattern)
  )
  
  key_words[["Inflation_pressure"]] <- c(
    "inflation pressure",
    "inflationary pressure", # with pressure (plurals taken into account)
     # inflation + has reached +... special character to control for different patterns
    "acceleration of inflation",
    "price pressures",
    "pressure on domestic price",
    "pressure on price",
    "combat inflation", # measures to correct
    "halting inflation",
    "halt to inflation",
    "efforts against inflation",
    "quick reduction.*inflation",
    "lowering the rate of inflation",
    "entrenchment of inflationary behavior"
  )
  


  key_words[["Trade_crisis"]] <- c(
    "trade war",
    "trade policy tension",
    "trade tension",
    "trade conflict",
    "excalation of trade restrictions",
    "disruption of trade",
    "trade crisis",
    "trade restrictions",
    "trade volatility",
    "weak trade",
    "disruption to trade",
    "slowdown in trade",
    "trade restricting measure",
    "decline in FDI",
    "decline in foreign direct investment",
    "FDI flows declined",
    "FDI have declined",
    "trade issues",
    "slowdown in trade",
    "trade slowdown",
    "uncertainty about future trade policy",
    "uncertainty about trade policy",
    "trade restraints",
    "trade policy unpredictability",
    "slowdown in global trade",
    "stronger competition of countries"
  )


  key_words[["World_outcomes"]] <- c(
    "world-wide recession",
    "global economic crisis",
    "global crisis",
    "world recession",
    "worldwide recession",
    "international crisis",
    "global financial crisis",
    "deep international recession",
    "international downturn",
    "internation recession",
    "ongoing global downturn",
    "deterioration of external environment",
    "weakening of international economic activity",
    "turbulence in international markets",
    "External conditions deteriorated markedly",
    "unfavorable developments in the international economic environment",
    "deterioration of external environment",
    "recession in the world economy",
    "international monetary crisis",
    "worsening international environement",
    "difficult external environment",
    "downside risks in the international environment",
    "further deterioration in the international environment",
    "uncertain external environment",
    "slowdown in international economy",
    "fragile global outlook",
    "international financial turmoil",
    "sharply deteriorating external conditions"
  )

  key_words[["Contagion"]] <- c(
    "regional crisis",
    "crisis in the region",
    "spillovers from the global crisis",
    "systemic crisis",
    "crisis in emerging economies",
    "regional financial crisis",
    "spillovers from the global crisis",
    "vulnerable to external shocks",
    "crisis spillover",
    "regional economic situation turned adverse",
    "contagion from the crisis in neightboring",
    "external shocks",
    "external shock",
    "adverse exogenous events",
    "external vulnerability",
    "exogenous events",
    "contagion",
    "fears of contagion",
    "spillovers",
    # "systemic spillovers",
    "vulnerability to international",
    "Russian debt crisis",
    "asian currency crisis",
    "crisis in southeast asia",
    "southeast asia crisis",
    "crisis in russia",
    "crisis in libya",
    "libya crisis",
    "regional currency crisis",
    "kosovo crisis",
    "cyprus crisis",
    "crisis in ukraine",
    "regional dimension of the crisis",
    "mexican exchange crisis",
    "gulf crisis",
    "middle east crisis",
    "Mexican crisis",
    "crisis in Argentina",
    "crisis in Russia",
    "argentine crisis",
    "crisis in mexico",
    "the crisis of 1994",
    "the 1997 crisis",
    "the crisis in 2002",
    "2002 crisis",
    "euro area crisis",
    "eurozone contagion",
    "eurozone crisis",
    "crisis in europe",
    "world financial crisis",
    "Greek crisis",
    "brazil crisis",
    "asian and russian crisis",
    "asia crisis",
    "crisis in turkey",
    "Argentinan crisis",
    "crisis in argentina",
    "crisis in greece",
    "asian crisis",
    "global economic crisis",
    "global financial shock",
    "international systemic spillover",
    # "risk of system insolvency",
    # "global crisis",
    # "global economic crisis",
    "crisis in Brazil",
    "linkage with the US",
    "contagion effects of the Thai crisis"
  )


  key_words[["Positive_expectations"]] <- c(
    "positive expectations",
    "vibrant business environment",
    "favorable investment climate",
    "high level of market confidence"
  )


  key_words[["Expectations"]] <- c(
    "crisis risks",
    "market reversal",
    "economic sentiment remains poor",
    "market sentiment has collapsed",
    "increase uncertainty in the international environment",
    "heightened risk aversion",
    "high level of risk",
    "general uncertainty",
    "crisis of confidence",
    "risk of crisis",
    "confidence crisis",
    "panic",
    "potential risks",
    "upward risk",
    "market confidence",
    "high risk",
    "downside risks",
    "increase the risks",
    "self fulfilling crises",
    "potential risks",
    "restoring market confidence",
    "major risks",
    "heightening risks",
    "deterioration in market sentiment",
    "increase uncertainty in the international environment",
    "deterioration in market sentiment",
    "weakening of investor confidence",
    "market confidence",
    "uncertainty in international capital markets",
    "uncertainty among market participant",
    "change in expectations",
    "speculative capital movements",
    "speculative attack",
    "uncertainty among market participant",
    "a time of heightened global uncertainty",
    "change in investors sentiment",
    "reassure the markets",
    "extreme global risk aversion",
    "provide assurances to financial markets",
    "restore market confidence",
    "reduce market uncertainty",
    "bolster confidence",
    "economic credibility",
    "slump in confidence",
    "undermining confidence",
    "confidence crisis",
    "signals to markets",
    "market confidence sagged",
    "vulnerable to abrupt swings in market sentiment",
    "heightened risk aversion",
    "increase in global risk aversion",
    "weakening of market confidence",
    "vulnerable to changes in the international investment climate",
    "confidence in the liquidity of the foreign exchange market",
    "increase uncertainty in the international environment",
    "pressures on confidence",
    "self-fulfilling",
    "shifts in investor sentiment",
    "bolstering market confidence",
    "confidence crisis"
  )


  #------------
  ## CURRENCY AND EXCHANGE RATE ISSUES
  #------------


  key_words[["Balance_payment_crisis"]] <- c(
    "Shortage of foreign exchange",
    "bop crisis",
    "balance of payment crisis",
    "capital account crisis",
    "balance of payment crisis",
    "balance of payment problem",
    "balance of payment difficulties",
    "cessation of official foreign capital inflows",
    "decline in net international reserves",
    "pressures in the official foreign exchange market",
    "external account came under pressure",
    "external account came under severe pressure",
    "external account came under serious pressure",
    "balance of payments problems",
    "shortage of international reserves",
    "sharp reduction in international reserves",
    "strong decline in international reserves",
    "international reserves exhaused",
    "decline in reserves",
    "drop in reserves",
    "loss of official reserves",
    "decline in net capital inflows",
    "decline in international reserves",
    "decline in official reserves",
    "official international reserves exhausted",
    "major loss in net international reserves",
    "foreign exchange scarcity",
    "decline in receipts of official foreign loans",
    "exhaustion of the disposable official international reserves",
    "capital flight",
    "flight of capital",
    "pull-back of capital",
    "capital flow reverse",
    "capital flow reversal",
    "pressure on capital flows",
    "large capital outflows",
    "strong balance of payment pressures",
    "balance of payment assistance",
    "depleted international reserves",
    "large external financing needs",
    "substantial capital outflows",
    "unforseen balance of payments contingencies",
    "tail risks to the balance of payments",
    "large balance of payments imbalances",
    "exhausted official international reserves",
    "pressure on the capital account",
    "exceptional balance of payments need",
    "balance of payment sustainability",
    "reversal in the flow of private capital",
    "sharpe reduction in access to international capital markets",
    "sharp fall in private inflows",
    "decline in net inflow",
    "severe external imbalances",
    "severe internal and external imbalances"
  )

  key_words[["Reduction_reserves"]] <- c(
    "decline in net international reserves",
    "shortage of international reserves",
    "shortage of international reserves",
    "shortfall of international reserves",
    "sharp reduction in international reserves",
    "strong decline in international reserves",
    "international reserves exhaused",
    "decline in reserves",
    "drop in reserves",
    "drop in official reserves",
    "selling foreign exchange",
    "decline in official reserves",
    "loss of official reserves",
    "loss of international reserves",
    "decline in international reserves",
    "loss of official international reserves",
    "decline in official international reserves",
    "exhaustion of the disposable official international reserves",
    "exhausted official international reserves",
    "strong pressure in exchange rate reserves",
    "depleted international reserves",
    "exhaustion of all foreign exchange reserves", # not original (added on the second stage)
    "sizeable fall of interantional reserves",
    "substantial sales of foreign reserves"
  )


  key_words[["Currency_crisis"]] <- c(
    "currency weakness",
    "exchange rate crisis",
    "large real depreciation",
    "foreign exchange crisis",
    "disruption of exchange markets",
    "severe disruption of exchange markets",
    "major devaluation",
    "Currency crisis",
    "Currency crash",
    "pressure on the exchange system",
    "large devaluation",
    "large depreciation",
    "cumulative depreciation",
    "sharp depreciation",
    "magnitude of the depreciation",
    "pressure in the foreign exchange markets", # not original (added on the second stage)
    "sharply depreciating trend",
    "a depreciating trend",
    "significant overshooting.*of the exchange rate",
    "overshooting of the exchange rate",
    "on a depreciating path",
    "widespread flight from the currency",
    "unsettled foreign exchange markets",
    "more depreciated level of the exchange rate",
    "more depreciated exchange rate",
    "more depreciated real exchange rate",
    "due to currency weakness",
    "demand for foreign exchange hedge jumped sharply",
    "did not calm exchange.*markets",
    "continuing exchange market uncertainties",
    "sizeable real depreciation",
    "continuing depreciation",
    "considerable depreciation"
  )

  key_words[["Currency_crisis_severe"]] <- c(
    "exchange rate crisis",
    "large real depreciation",
    "foreign exchange crisis",
    "severe disruption of exchange markets",
    "major devaluation",
    "currency crisis",
    "currency crash",
    "large devaluation",
    "large depreciation",
    "sharp depreciation",
    "sharp depreclatlon",
    "currency attack",
    "exchange rate crisis",
    "unsuccessful attempt to.*maintain the exchange rate unchanged",
    "foreign currency turmoil"
  )

  key_words[["Currency_crisis_confusing"]] <- c(
    "sharp depreciation in the u.s. dollar",
    "sharp depreciation of the u.s. dollar",
    "large depreciation of the u.s.dollar",
    "yen.+s sharp depreciation"
  )

  key_words[["Floating_exchange_rate"]] <- c(
    "floating exchange rate regime",
    "floating currency",
    "flexible exchange rate",
    "dual exchange market"
  )

  key_words[["Fixed_exchange_rate"]] <- c(
    "pegged",
    "currency pegs",
    "exchange rate peg",
    "peg to the dollar",
    "fixed exchange rate",
    "adjustable exchange rate bands",
    "crawling band exchange rate",
    "exchange rate peg  within a band",
    "convertibility regime",
    "adjustable exchange rate band system",
    "currency board regime",
    "currency board",
    "common currency area",
    "fully dollarized"
  )

  key_words[["Losening_monetary_policy"]] <- c(
    "loosening monetary policy",
    "accommodative monetary policy",
    "support from monetary policy",
    "easing monetary policy",
    "monetary easing",
    "monetary policy stimulus",
    "ease monetary policy",
    "interest rate cut",
    "bring down interest rates"
  )

  key_words[["Tightening_monetary_policy"]] <- c(
    "monetary tightening",
    "restrictive monetary policy",
    "tighten monetary policy",
    "restrictive monetary policy",
    "control monetary aggregate",
    "tighten monetary conditions",
    "tightening of interest rate",
    "increase interest rate",
    "tighter lending conditions",
    "higher interest rates",
    "tightening of monetary policy",
    "increase in interest rates"
  )



  #----------
  ## OUTPUT SHOCKS
  #-----------


  key_words[["Severe_recession"]] <- c(
    "severe economic crisis",
    "very difficult economic circumstances",
    "Severe recession",
    "severe crisis",
    "economic crisis",
    "steep recession",
    "strong recessionary headwinds",
    "sharp slowdown",
    "sharp declines in output",
    "significant loss of output",
    "economic collapse",
    "deeper recession",
    "deepening recession",
    "painful recession",
    "prolonged recession",
    "lengthening recession",
    "severity of the recession",
    "economic recession",
    "sharp contraction of economic activity",
    "strong contraction of economic activity",
    "large contraction of economic activity",
    "deep recession",
    "large economic slowdown",
    "severe recession",
    "profond recession",
    "contraction in output",
    "deep recession",
    "severe contraction",
    "deep contraction",
    "profond contraction",
    "large decline in income per capita",
    "deep economic downturn",
    "severe economic downturn",
    "deep economic downturn"
  )

  key_words[["Soft_recession"]] <- c(
    "slowdown in the economic activity",
    "slowdown in economic growth",
    "slowdown of the economy",
    "slowdown of output",
    "economic decline",
    "activity remains weak",
    "the economy slowed down",
    "declining trend in economic activity",
    "decline in economic activity",
    "slowing down of business activity",
    "slow down",
    "low rates of economic growth",
    "low rate of economic growth",
    "economic activity on a downward trend",
    "depressed level of economic activity",
    "the economic situation worsen",
    "slowing the pace of economic recovery",
    "decline in economic activity",
    "weakening of economic fundamental",
    "recession",
    "contraction of output",
    "sluggish recovery",
    "contraction of economic activity",
    "economic downturn",
    "output is estimated to have contracted",
    "slowdown in the economic activity",
    "slowdown of output",
    "slow economic activity"
  )

  key_words[["Expansion"]] <- c(
    "exceptionally high rate of economic growth",
    "very strong fundamentals",
    "high rate of economic growth",
    "high GDP growth",
    "economic activity remains robust",
    "strong economic growth",
    "economy performed well",
    "economic recovery",
    "output was growing",
    "economic expansion",
    "economic activity accelerated",
    "GDP increasing",
    "acceleration in economic activity",
    "exceeded potential",
    "recovery of economic activity",
    "overheating",
    "achievement of macroeconomic stability",
    "expansion of economic activity",
    "strong economic activity",
    "overheating of domestic demand",
    "favorable economic developments",
    "overheating of the economy",
    "domestic demand expansion",
    "The economy continued to perform strongly",
    "strong economic performance",
    "high competitivitness",
    "rapid real convergence",
    "economy is currently running over its potential",
    "economy that is growing above potential",
    "buyant investments",
    "optimistic outlook",
    "competitivitness remain solid",
    "stable growth path",
    "economic overheating",
    "rapid economic growth",
    "output above potential",
    "rapid economic expansion",
    "overheating pressures",
    "economic success",
    "GDP growth above potential",
    "remarkable economic performance",
    "overheated economy",
    "over-heated economy",
    "strong growth performance",
    "economy is overheating",
    "contain demand pressures"
  )


  key_words[["Poverty_crisis"]] <- c(
    "poverty",
    "the poor",
    "basic access",
    "pro-poor",
    "social hardship",
    "food security",
    "low income familly",
    "living condition"
  )


  #---------
  ## PUBLIC DEBT ISSUES
  #---------


  key_words[["Fiscal_outcomes"]] <- c(
    "large debt servicing burden",
    "large debt service obligation",
    "large borrowing needs",
    "public finance were adversely affected",
    "high debt service",
    "severe fiscal imbalances",
    "extremely difficult budgetary situation",
    "heavy debt service burden",
    "high external debt service payments",
    "stock of domestic debt increased",
    "weakening of the public finances",
    "borrowing increased substantially",
    "growing deficits",
    "unsustainable public debt",
    "unsustainable financial position",
    "unsustainable debt",
    "stock of external debt increased",
    "budgetary imbalances",
    "fragile fiscal framework",
    "lack of financial ressources",
    "increase in interest payments",
    "servicing external debt",
    "fiscal account vulnerable",
    "difficult budgetary situation",
    "rising levels of government spendings",
    "indebtedness of the public sector",
    "large budgetary gap",
    "adverse effect on government revenue",
    "weaken the fiscal position",
    "decline in government revenues",
    "public sector position deteriorate",
    "high debt service",
    "increase in government spending",
    "shortfall in expenditure",
    "debt service obligations",
    "shortfall of revenue",
    "limit new borrowing",
    "drop in revenue",
    "expenditure restraint",
    "debt service burden",
    "a gap in financing",
    "financing gap",
    "weakened fiscal revenue",
    "government deficit widened",
    "accrued deficit",
    "concerns about debt sustainability",
    "pressure on public finance",
    "market concerns about debt sustainability",
    "fiscal instability",
    "large external financing needs",
    # "Debt",
    "severe deterioration in the fiscal situation",
    "short-term financing need",
    "external debt",
    "contingent fiscal liability",
    "deficits",
    "policy stimulus",
    "external sovereign bond buybacks"
  )


  key_words[["Low_public_debt"]] <- c(
    "low level of public debt",
    "low public debt",
    "negligible public debt",
    "strong fiscal position",
    "low gross government debt",
    "fiscal position is strong",
    "outstanding record of fiscal policy",
    "strong fiscal outcomes"
  )


  key_words[["Fiscal_stimulus"]] <- c(
    "fiscal_stimulus",
    "fiscal boost",
    "fiscal package",
    "fiscal support",
    "fiscal loosening",
    "positive fiscal impulse",
    "expansionary fiscal stance"
  )



  key_words[["Fiscal_consolidation"]] <- c(
    "overall deficit of the public sector is to be reduced",
    "Maintaining tight financialpolicies",
    "fiscal consolidation",
    "fiscal adjustment effort",
    "tightening of fiscal policy",
    "tight fiscal stance",
    "budget surplus",
    "reduction in public debt",
    "reduce public sector deficit",
    "containing expenditures",
    "strengthening annual fiscal targets",
    "reduce substantially the budgetary deficit",
    "contain the rise in expenditure",
    "substantial adjustments with respect to the budget",
    "freeze wages",
    "fiscal restraint",
    "wage moderation",
    "rationalization of public sector expenditure",
    "improve the financial position",
    "control over noninterest expenditures",
    "nonpriority expenditure",
    "long term financial strength",
    "expenditure discipline",
    "tax collection effort",
    "reduce operational deficits",
    "consolidate pulbic sector revenue",
    "fiscal performance",
    "reduction of the deficit",
    "actions on the public finance",
    "measures in the fiscal field",
    "fiscal performance criteria",
    "revenue measures",
    "expenditure cuts",
    "limiting borrowing",
    "performance criteria in the fiscal area",
    "cash budget deficit",
    "limit budget deficits",
    "cutting the overall central government deficit",
    "consolidation of the fiscal position",
    "strengthening of the fiscal position",
    "strengthening of the financial position",
    "intend to improve budgetary performances",
    "achieving a current budgetary surplus",
    "fiscal action",
    "limit public sector borrowing requirement",
    "fiscal discipline",
    "fiscal restraint",
    "fiscal targets",
    "fiscal reform",
    "fiscal measures",
    "fiscal discipline",
    "fiscal objectives",
    "fiscal adjustement",
    "fiscal tightening",
    "reduce the deficit",
    "asjustement effort",
    "fiscal adjustment",
    "bolster the public finance",
    "redress their fiscal imbalances",
    "tightening fiscal policy",
    "reduction in the overall deficit",
    "tax reform package",
    "tight wage policy",
    "controlling expenditure",
    "reduce sharply the overall deficit",
    "reduce sharply the deficit",
    "fiscal policy",
    "spending controls",
    "measures to increase budgetary revenue",
    "limit the rise in government outlays",
    "limit the budgetary",
    "cutback in expenditures",
    "revenues measures",
    "toward budgetary equilibrium",
    "limits on the borrowing requirement",
    "ceilings on external public",
    "substantial adjustment",
    "restrained domestic financial policies",
    "fiscal outcome may fall short of the target",
    "balance in the finances of the public sector",
    "tight fiscal stance",
    "firm control over public finance",
    "fiscal adjustment",
    "package of fiscal measures",
    "additional fiscal measures",
    "primary fiscal balance",
    "monitoring of the fiscal performance",
    "firm fiscal policy"
  )

  key_words[["Concessional_lending"]] <- c(
    "concessional terms"
  )
  key_words[["Short_term_debt"]] <- c(
    "short term borrowing",
    "short-term maturities",
    "short term public debt",
    "short term external financing",
    "short maturity",
    "short maturities",
    "debt maturing in less than 12 months",
    "shortening of public debt maturity",
    "shortening of the average maturity",
    "securities due within less than 12 months",
    "short term component of the external debt",
    "maturity shortening"
  )


  key_words[["floating_rate_debt"]] <- c(
    "floating rate securities",
    "floating rate notes"
  )


  key_words[["foreign_debt"]] <- c(
    "de facto dollarization",
    "denominated in foreign currency",
    "currency mismatch",
    "denominated in U.S. dollar",
    "denominated in US dollar",
    "foreign exchange-linked bonds",
    "foreign exchange linked bonds",
    "debt indexed to the dollar",
    "debt denominated in foreign exchange",
    "foreign exchange index debt",
    "denominated in foreign exchange"
  )


  key_words[["Sovereign_default"]] <- c(
    "rescheduled debt", #
    "external payments crisis",
    "difficulties in servicing its external debt", #
    "difficult time in rolling over its debt", #
    "rescheduling of external debt", #
    "rescheduling agreement", ##
    "suspend service payments", #
    "fiscal crisis", #
    "debt relief", #
    "failure to roll over debt", #
    "government bonds crisis", #
    "government bonds crisis", #
    "bond crisis", #
    "debt reprofiling", #
    "sovereign debt crisis", #
    "public debt crisis", #
    "default risks", #
    "self fulfilling crises",
    "debt restructuring program", #
    "governement default", #
    "restructuring of debt", #
    "suspension of payments", #
    "debt swap", #
    "debt restructuring", #
    "debt rescheduling", #
    "debt service reduction", #
    "debt restructuring program", #
    "rescheduling of the debt", #
    "arrears", #
    "rescheduling of arrears", #
    "arrears in the payment", #
    "restructuring of its external debt", #
    "restructuring agreements", #
    "external payment arrears", #
    "debt service reduction", #
    "no debt service payments", #
    "relation with external creditors", #
    "Paris Club", #
    "Club of Paris", #
    "debt relief", #
    "debt exchange" #
  )


  #---------
  ## PROBLEMATIC DOCUMENTS TAG
  #---------
  
  key_words[["IMF_verbose"]] <- c(
    "document",
    "international monetary fund",
    "public agenda",
    "confidential",
    "information",
    "executive board secretary",
    "consultations",
    "consideration",
    "executive board staff report consultations",
    "recommendations ",
    "staff",
    "pages",
    "background",
    "information",
    "memorandum",
    "staff report",
    "representatives consultations",
    "committee",
    "article xiv",
    "consultations",
    "staff mission", 
    "discussions", 
    "membership",
    "minutes",
    "executive board meeting",
    "executive board attendance",
    "article iv consultation",
    "review",
    "extended arrangement",
    "article iv consultation",
    "enhanced structural adjustment facility", 
    "arrangement",
    "review",
    "waiver",
    "performance criteria",
    "structural adjustment arrangement",
    "minutes of executive board meeting", # Minutes of meeting
    "minutes of executive board minutes",
    "executive board attendance",
    "final minutes of executive board meeting",
    "this is a working paper", # Working Papers
    "working paper",
    "a working paper of the international monetary fund",
    "background paper", # Background papers: similar to working papers
    "provides background to the paper",
    "attached paper provides background information",
    "background documentation for",
    "draft issues paper", # Issue papers: similar to working paper
    "(?<!the documents listed below have been or will be separately released. )selected issues", # Selected issues
    "(?<!the documents listed below have been or will be separately released. selected issues )financial system stability assessment", # Similar to selected issues for financial system
    "poverty reduction strategy paper", # Similar to selected issues for poverty
    "individual economy assessments", # Assessments on multiple countries for a single issue
    "global financial stability report", # found looking through epidemics India
    "debt sustainability analysis", # ?
    "triennial surveillance review", # Documents on internal IMF functioning
    "interim surveillance review",
    "report on the observance of standard and codes"
  )
  
  # Informational annexes still included because part of recent articles IV.
  key_words[["Problematic_documents"]] <- c(
    "minutes of executive board meeting", # Minutes of meeting
    "minutes of executive board minutes",
    "executive board attendance",
    "final minutes of executive board meeting",
    "this is a working paper", # Working Papers
    "working paper",
    "a working paper of the international monetary fund",
    "background paper", # Background papers: similar to working papers
    "provides background to the paper",
    "attached paper provides background information",
    "background documentation for",
    "draft issues paper", # Issue papers: similar to working paper
    "(?<!the documents listed below have been or will be separately released. )selected issues", # Selected issues
    "(?<!the documents listed below have been or will be separately released. selected issues )financial system stability assessment", # Similar to selected issues for financial system
    "poverty reduction strategy paper", # Similar to selected issues for poverty
    "individual economy assessments", # Assessments on multiple countries for a single issue
    "global financial stability report", # found looking through epidemics India
    "debt sustainability analysis", # ?
    "triennial surveillance review", # Documents on internal IMF functioning
    "interim surveillance review",
    "report on the observance of standard and codes"
  )
  key_words
}

ccdr.lexicon_details <- function(keyword) {
  #' Provide the keywords associate to the name of the category
  #' @description usefull know what are the words behind each category
  #' @param keyword the name of the category of which you want to
  #' know the lexicon
  #' @author Manuel Betin
  #' @return a vector of words
  #' @example lexicon_details("Severe_recession")
  #' @export
  detect <- names(ccdr.lexicon())[stringr::str_detect(
    tolower(names(ccdr.lexicon())),
    tolower(keyword)
  )]

  return(ccdr.lexicon()[detect])
}

ccdr.lexicon.exportcsv=function(myvars=NULL,path){
  #'export dictionary of words in csv
  #'@description export the dictionary 
  #'@param myvars a vector with the names of the categories to export
  #'@param path the path to the folder where the dicitonary is exported
  #'@author Manuel Betin
  #'@export
  if(is.null(myvars)){
    myvars=ccdr.lexicon()%>%names()
  }
  
  results=lapply(myvars,function(x){
    res=cbind(x,ccdr.lexicon_details(x)[[1]])
    colnames(res)=c("category","keywords")
    res
  })
  results=do.call(rbind,results) %>% data.frame()%>%tibble()
  
  rio::export(results,paste0(path,"/ccdr.lexicon_",Sys.Date(),".csv"))
  return(results)
}
