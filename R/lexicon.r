lexicon <- function(){
  
  #' Lexicon of economic crisis
  #' @description Provide the lexicon of economic crisis  
  #' @return a list of categories with the corresponding words
  #'  associated
  #' @export
  #' @example key_word_crisis()
  
  key_words <- list()
  
  #--------
  ## REFORMS AND CONDITIONALITY
  #--------
  
  key_words[["Deregulation"]]=c(
    "privatization",
    "liberalization",
    "liberalize",
    "privatize",
    "deregulate",
    "demonopolization",
    "deregulation"
  )
  
  key_words[["Reform_agenda"]]=c(
    "reform",
    "reforming",
    "adjustment program",
    "structural adjustments",
    "package",
    "structural program"
  )
  
  key_words[["Trade_reforms"]]=c(
    "trade and tariff programme",
    "trade and tariff program",
    "trade and tariff reform",
    "trade liberalization",
    "liberalization of trade",
    "liberalize trade",
    "opening to international trade",
    "liberalize trade policy",
    "liberalization of tariffs",
    "remove tariffs",
    "lift tariffs",
    "lifting tariffs",
    "open trade policy",
    "comply with WTO",
    "WTO",
    "removing tariffs"
  )
  key_words[["Financial_reforms"]]=c(
    "financial reform",
    "financial sector reform",
    "reforms on the financial system",
    "reforms to the financial sector",
    "liberalize the financial sector",
    "liberalization of the financial sector",
    "reforms to the financial sector",
    "reforming the financial sector",
    "regulation of the financial sector",
    "deregulation of the financial sector",
    "deregulating the financial sector",
    "financial market regulation",
    "financial market deregulation",
    "regulation financial markets",
    "deregulation of financial markets",
    "regulating the financial sector"
  )
  
  
  key_words[["Labor_market_reforms"]]=c(
    "labor market reforms",
    "reforms to the labor market",
    "labor market flexibility",
    "liberalization of the labor market",
    "liberalize the labor market",
    "flexibility of the labor market",
    "flexibility of the labour market",
    "deregulation labor market",
    "labor market framework",
    "labor reform"
  )
  
  key_words[["Tax_reforms"]]=c(
    "tax reform",
    "fiscal reform",
    "reform the tax system",
    "reform of indirect taxation",
    "tax package",
    "tax administration",
    "modifications to the tax regime"
  )
  
  
  key_words[["Banking_reforms"]]=c(
    "banking reform",
    "banking sector reform",
    "reforms to the banking sector",
    "reforming the banking sector",
    "reforming bank",
    "regulation of the banking sector",
    "liberalization of the banking sector",
    "liberalize the banking sector",
    "banking supervision reform",
    "banking system consolidation",
    "bank restructuring reform",
    "desintermediation of the banking system",
    "regulating the financial sector"
  )
  
  
  
  #--------
  ## OFFICIAL CREDITORS INTERVENTION
  #--------
  
  
  key_words[["Peformance_criterion"]]=c(
    "performance criteria",
    "non observance of performance criterion",
    "performance criteria were met",
    "non observance",
    "delay in the review",
    "performance criteria and reviews",
    "completion of the second review",
    "completion of the first review",
    "completion of the third review",
    "completion of the second and third review",
    "status of implementation of prior actions",
    "record of policy implementation as been very slow",
    "policy commitments",
    "keep the program on track",
    "program reviews",
    "waivers of the relevant performance criteria",
    "request for a waiver",
    "reviews of the arrangement",
    "rephasing of purchases",
    "waiver for the non observance",
    "review of economic performance",
    "succesful implementation of the program",
    "fiscal performance criteria",
    "modification of the fiscal performance criteria",
    "second review under the arrangement",
    "quantitative targets",
    "were all met",
    "request waivers of the performance criteria",
    "policy slippage"
  )
  
  key_words[["Program_extension"]]=c(
    "augmentation of SDR",
    "emergency financial procedure",
    "extension of the stand-by arrangement",
    "extension of the arrangement",
    "supplemental reserve facility",
    "augmentation of access under",
    "augmentation of the SBA"
  )
  
  key_words[["Official_support"]]=c(
    "ESFS",
    "European system of financial supervision",
    "ESM",
    "IRDB",
    "ECB",
    "european commission",
    "development banks",
    "official lending",
    "official support",
    "World Bank",
    "Official bilateral creditors",
    "Inter-American Development Bank",
    "Inter American Development Bank",
    "European stability mechanism",
    "Fed swap lines")
  
  key_words[["Technical_assistance"]]=c(
    "technical assistance"
  )
  
  key_words[["Precautionary_programs"]]=c(
    #"precaution",
    "make purchases under the requested stand-by arrangement as needed",
    "make purchases under the requested stand-by arrangement only as needed",
    "intent to make any purchases",
    "do not intent to make purchases",
    "make no purchases",
    "not to make purchases",
    "no purchases are intended",
    "requested stand-by arrangement only as a last resort",
    "only as a last resort",
    "arrangement as precautionary",
    "treat the proposed arrangement as a precautionary",
    "to treat as precautionary",
    "to treat the arrangement as precautionary",
    "treated the arrangement as a precautionary",
    "arrangement as a precautionary",
    "arrangement as precautionary",
    "the arrangement will be precautionary",
    "no purchases are expected",
    "intent to make no purchases",
    "intentions not to make purchases",
    "do not intent to make purchases under the arrangement",
    "do not intent to make any purchases under this stand",
    "intention not to make purchases under the arrangement",
    "they intent to make no purchase  under the arrangement",
    "arrangementas precautionary",
    "arrangement is precautionary",
    "precautionary arrangement",
    "the program is precautionary",
    "precautionary 13 month stand",
    "14 month precautionary stand by arrangement",
    "arrangement remained precautionary",
    "approved a precautionary",
    "precautionary support",
    "precautionary nature",
    "arrangement was precautionary",
    "approved a precautionary",
    "treat the sba as precautionary",
    "arrangement will be precautionary",
    "request for a precautionary",
    "precautionary arrangement",
    "precautionary sba",
    "month precautionary",
    "precautionary stand by",
    "precautonary program",
    "approved a precautionary",
    "treated as precautionary",
    "precautionary stand by arrangement",
    "precautionary stand",
    "the program is precautionary",
    "is intented to be precautionary",
    "would treat the arrangementas precautionary",
    "does not propose to make any purchase",
    "do not intend at this time to make a drawing",
    "do not intend to make any purcharse under this stand",
    "do not intend to make purchase under the arrangement",
    "do not intend to draw on the resources")
  
  
  key_words[["Risk_for_IMF"]]=c(
    "Risks to the program",
    "risks to the Fund",
    "repayment capacity",
    "repayment capacity to the Fund",
    "outstanding use of Fund resources",
    "Fund cumulative exposure",
    "Fund exposure",
    "debt service to the Fund",
    "Capacity to Repay the Fund",
    "service its obligations to the Fund",
    "repay the Fund",
    "debt service due to the Fund",
    "payments to the fund"
  )
  
  key_words[["Uncertainty_reforms"]]=c(
    "implementation risk",
    "inconsistent record in policy reform",
    "non compliance with agenda",
    "non observance",
    "program implementation"
  )
  
  key_words[["Disbursement"]]=c(
    "frontloaded disbursement",
    "exceptional circumstances provision",
    "Exceptional Access Criteria",
    "Funds access policy"
  )
  
  
  #--------
  ## EXOGENEOUS SHOCKS: NON ECONOMIC
  #--------
  
  
  key_words[["Wars"]]=c(
    "war damage",
    "insurgency crisis",
    "security crisis",
    "civil war",
    "war claims",
    "war reconstruction",
    "war recovery",
    "war strategy",
    "post conflict assistance",
    "armed conflict",
    "oingoing conflict",
    "atlantic conflict",
    "military conflict",
    "armed internal conflict",
    "ethnic rivalries",
    "internal conflict",
    "armed domestic conflict",
    "israeli conflict",
    "sahara conflict",
    "violent conflict",
    "military conflict",
    "conflict regions",
    "conflict zone",
    "regional conflict",
    "civil conflict",
    "conflicts in the region",
    "armed conflicts",
    "terrorism",
    "terrorist attacks",
    "guerilla offensive",
    "military take-over"
  )
  
  key_words[["Migration"]]=c(
    "refugee",
    "migrants",
    "populatio inflow",
    "asylum",
    "migration flows",
    "immigrant"
  )
  
  key_words[["Natural_disaster"]]=c(
    "flooding",
    "drought",
    "record-low levels of rainfall",
    "natural calamities",
    "virus",
    "natural disaster",
    "earthquake",
    "hurricane")
  
  key_words[["Epidemics"]]=c(
    "epidemics",
    "pandemia",
    "leishmaniasis",
    "dengue fever",
    "chikungunya",
    "mumps",
    "meningitis",
    "poliomyelitis",
    "measles",
    "zika",
    "encephalitis",
    "sars",
    "nipah",
    "vCJD",
    "HIV",
    "smallpox",
    "Relapsing fever",
    "typhoid fever",
    "typhus",
    "hepatitis",
    "SRAS",
    "H1N1",
    "virus",
    "epidemy",
    "epidemic",
    "epidemia",
    "avian flu",
    "ebola",
    "^sida$",
    "rotavirus",
    "Lyme",
    "hepatite",
    "H5N1",
    "chikungunya",
    "dysenteria",
    "dysentery",
    "smallpox",
    "yellow fever",
    "cholera",
    "malaria",
    "influenza",
    "coronavirus",
    "Covid-19",
    "^flu$",
    "^plague$"
  )
  
  key_words[["Commodity_crisis"]]=c(
    "oil crisis",
    "rice crisis",
    "food crisis",
    "deterioration in the terms of trade",
    "decline in terms of trade",
    "adverse terms of trade",
    "terms of trade loss",
    "unfavorable terms of trade",
    "severe drop in terms of trade",
    "severe terms of trade shocks",
    "vulnerability to terms of trade shocks",
    "collapse in international prices",
    "significant terms of trade loss",
    "terms of trade declined",
    "terms of trade drop",
    'large terms of trade loss',
    "dependenceon oil-related revenue",
    "budgetary dependency on oil revenue",
    "negative terms of trade",
    "terms of trade worsen",
    "crisis in the cotton",
    "sharp fall in its terms of trade",
    "increase in world oil prices",
    "decline in international coffee prices",
    "terms of trade were adversely affected",
    "drop in world coffee price",
    "oil price increase",
    "fluctuations in oil prices",
    "decline in coffee prices",
    "decline in the terms of trade",
    "increase in petroleum price",
    "Terms of trade shock",
    "adverse movement in the terms of trade",
    "adverse movement in the price of copper",
    "weakness in the copper price",
    "decline in the copper price",
    "swing in copper price",
    "price of copper continue to drop",
    "fluctuations in the terms of trade",
    "deteriorating terms of trade",
    "commodity crisis",
    "movements in commodity prices",
    "domestic energy crisis",
    "fall in prices of raw materials",
    "energy crisis",
    "fell of agricultural prices"
  )
  
  key_words[["Political_crisis"]]=c(
    "political turmoil",
    "internal security situation",
    "political atmosphere",
    "political crisis",
    "political uncertainty",
    "Political instability",
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
    "uncertainty regarding future policies")
  
  key_words[["Social_crisis"]]=c(
    "social risk",
    "social and political turmoil",
    "social disruption",
    "social climate as deteriorate",
    "social tension",
    "protest",
    "strike",
    "deteriorating social climate"
  )
  
  
  #--------
  ## EXOGENEOUS SHOCKS; ECONOMIC
  #--------
  
  key_words[["Housing_crisis"]]=c(
    "home prices have been declining",
    "drops in real estate prices",
    "house price trends",
    "home-price overvaluation",
    "real house prices declining",
    "foreclosures",
    "house price inflation",
    "house-price inflation",
    "foreclosures",
    "bust in housing",
    "home-price declines",
    "house-price declines",
    "house prices fall",
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
  
  key_words[["Banking_crisis"]]=c(
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
    "financial support package")
  
  key_words[["Banking_crisis_severe"]]=c(
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
  
  key_words[["Financial_crisis"]]=c(
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
  
  key_words[["Inflation_crisis"]]=c(
    "inflation pressure",
    "hyperinflation",
    "high rate of inflation",
    "contain inflationary pressure",
    "the rate of inflation accelerate",
    "halting inflation",
    "inflationary pressures",
    "halt to inflation",
    "limit inflationary pressures",
    "efforts against inflation",
    "reduction of inflation",
    "issuing paper money",
    "inflation has now reached a critical",
    #"infaltion has reached",
    "inflation at unprecedented levels",
    "large monetary creation",
    "lowering the rate of inflation",
    "pressure on prices",
    "high inflation",
    "high headline inflation",
    "severe inflation",
    "combat inflation",
    "cronic inflation",
    "sharpe increase in domestic prices",
    "large increase in domestic prices",
    #"inflationary pressures",
    "inflationary pressure",
    "inflation crisis")
  
  key_words[["Trade_crisis"]]=c(
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
  
  
  key_words[["World_outcomes"]]=c(
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
    "fragile global outlook"
  )
  
  key_words[["Contagion"]]=c(
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
    #"systemic spillovers",
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
    #"risk of system insolvency",
    #"global crisis",
    #"global economic crisis",
    "crisis in Brazil",
    "linkage with the US",
    "contagion effects of the Thai crisis")
  
  key_words[["Expectations"]]=c(
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
    "bolstering market confidence"
  )
  
  
  #------------
  ## CURRENCY AND EXCHANGE RATE ISSUES
  #------------
  
  
  key_words[["Balance_payment_crisis"]]=c(
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
  
  key_words[["Reduction_reserves"]]=c(
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
  
  
  key_words[["Currency_crisis"]]=c(
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
  
  key_words[["Currency_crisis_severe"]]=c(
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
    'unsuccessful attempt to.*maintain the exchange rate unchanged'
  )
  
  key_words[["Currency_crisis_confusing"]]=c(
    "sharp depreciation in the u.s. dollar",
    "sharp depreciation of the u.s. dollar",
    "large depreciation of the u.s.dollar",
    "yen.+s sharp depreciation"
  )
  
  key_words[["Floating_exchange_rate"]]=c(
    "floating exchange rate regime",
    "floating currency",
    "flexible exchange rate",
    "dual exchange market"
  )
  
  key_words[["Fixed_exchange_rate"]]=c(
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
  
  key_words[["Losening_monetary_policy"]]=c(
    "loosening monetary policy",
    "accommodative monetary policy",
    "support from monetary policy",
    "easing monetary policy",
    "monetary easing",
    'monetary policy stimulus',
    "ease monetary policy",
    "interest rate cut",
    "bring down interest rates"
  )
  
  key_words[["Tightening_monetary_policy"]]=c(
    "monetary tightening",
    "restrictive monetary policy",
    "tighten monetary policy",
    "control monetary aggregate",
    "tighten monetary conditions",
    "tightening of interest rate",
    "increase interest rate",
    "tightening of monetary policy",
    "increase in interest rates"
  )
  
  
  
  #----------
  ## OUTPUT SHOCKS
  #-----------
  
  
  key_words[["Severe_recession"]]=c(
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
  
  key_words[["Soft_recession"]]=c(
    "slowdown in the economic activity",
    "slowdown in economic growth",
    "slowdown of the economy",
    "slowdown of output",
    "economic decline",
    'activity remains weak',
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
    'sluggish recovery',
    "contraction of economic activity",
    "economic downturn",
    "output is estimated to have contracted",
    "slowdown in the economic activity",
    "slowdown of output",
    "slow economic activity")
  
  key_words[["Expansion"]]=c(
    "exceptionally high rate of economic growth",
    "very strong fundamentals",
    "high rate of economic growth",
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
    "strong economic performance")
  
  
  
  #---------
  ## PUBLIC DEBT ISSUES
  #---------
  
  
  key_words[["Fiscal_outcomes"]]=c(
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
    "external sovereign bond buybacks")
  
  key_words[["Fiscal_consolidation"]]=c(
    "overall deficit of the public sector is to be reduced",
    "Maintaining tight financialpolicies",
    "fiscal consolidation",
    "fiscal adjustment effort",
    "tightening of fiscal policy",
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
    "monitoring of the fiscal performance"
  )
  
  key_words[["Concessional_lending"]]=c(
    "concessional terms"
  )
  key_words[["Short_term_debt"]]=c(
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
    "maturity shortening")
  
  
  key_words[["floating_rate_debt"]]=c(
    "floating rate securities",
    "floating rate notes"
  )
  
  
  key_words[["foreign_debt"]]=c(
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
    "denominated in foreign exchange")
  
  
  key_words[["Sovereign_default"]]=c(
    "rescheduled debt", #
    "external payments crisis",
    "difficulties in servicing its external debt",#
    "difficult time in rolling over its debt",#
    "rescheduling of external debt",#
    "rescheduling agreement",##
    "suspend service payments",#
    "fiscal crisis",#
    "debt relief",#
    "failure to roll over debt",#
    "government bonds crisis",#
    "government bonds crisis",#
    "bond crisis",#
    "debt reprofiling",#
    "sovereign debt crisis",#
    "public debt crisis",#
    "default risks",#
    "self fulfilling crises",
    "debt restructuring program",#
    "governement default",#
    "restructuring of debt",#
    "suspension of payments",#
    "debt swap",#
    "debt restructuring",#
    "debt rescheduling",#
    "debt service reduction",#
    "debt restructuring program",#
    "rescheduling of the debt",#
    "arrears",#
    "rescheduling of arrears",#
    "arrears in the payment",#
    "restructuring of its external debt",#
    "restructuring agreements",#
    "external payment arrears",#
    "debt service reduction",#
    "no debt service payments",#
    "relation with external creditors",#
    "Paris Club",#
    "Club of Paris",#
    "debt relief",#
    "debt exchange"#
  )
  
  
  key_words[["Track_record"]]=c(
    "impeccable record of payments to the fund under previous facilities",
    "sound policies",
    "solid policy framework",
    "strong repurchase track record",
    "sustained track records",
    "high level of credibility in the institutional framework",
    "high level of policy credibility",
    "solid track record"
  )
  
  #---------
  ## PROBLEMATIC DOCUMENTS TAG
  #---------
  key_words[["Problematic_documents"]] = c(
    "minutes of executive board meeting", # Minutes of meeting
    "minutes of executive board minutes",
    "executive board attendance",
    "this is a working paper", # Working Papers
    "working paper",
    "a working paper of the international monetary fund",
    "draft issues paper", # Issue papers
    "background paper", #Background papers
    "provides background to the paper",
    "attached paper provides background information",
    "individual economy assessments", #Assessments on multiple countries
    "april 2012 global financial stability report", # found looking through epidemics India
    "poverty reduction strategy paper"
  )
  
  
  key_words
}

lexicon_details = function(keyword) {
  #' Provide the keywords associate to the name of the category
  #' @description usefull know what are the words behind each category
  #' @param keyword the name of the category of which you want to 
  #' know the lexicon
  #' @author Manuel Betin
  #' @return a vector of words 
  #' @export
  detect = names(lexicon())[stringr::str_detect(tolower(names(lexicon())), 
                                                         tolower(keyword))]
  
  return(lexicon()[detect])
  
}

lexicon_typology = function() {
  
  #' provide the typology of each category
  #' @description Provide a typology for each category in the lexicon
  #' @return a dataframe classifying each category of the lexicon
  #' @author Manuel Betin
  #' @example typology_categories()
  #' @export
  
  #include new categories in the proper classification
  
  adjustment_program = c("Deregulation", "Reform_agenda", "Trade_reforms", 
                         "Financial_reforms", "Labor_market_reforms", "Tax_reforms", "Banking_reforms", 
                         "Fiscal_consolidation", "Success_of_reforms")
  
  characteristics_program = c("Performance_criterion", "Program_extension", 
                              "Official_support", "Technical_assistance", "Precautionary_programs")
  
  economic_shock = c("Banking_crisis","Banking_crisis_severe", "Financial_crisis", "Inflation_crisis", 
                     "Trade_crisis", "World_outcomes", "Contagion", "Expectations", 
                     "Balance_payment_crisis", "Reduction_reserves", "Currency_crisis","Currency_crisis_severe", "Severe_recession", 
                     "Soft_recession", "Expansion","Housing_crisis")
  
  non_economic_shock = c("Wars", "Natural_disaster", "Commodity_crisis", "Political_crisis", 
                         "Social_crisis","Epidemics")
  
  debt_outcomes = c("Fiscal_outcomes", "Sovereign_default")
  
  debt_structure = c("Concessional_lending", "Short_term_debt", "Floating_rate_debt", 
                     "Foreign_debt", "Track_record")
  
  type = unlist(list(adjustment_program = adjustment_program, characteristics_program = characteristics_program, 
                     economic_shock = economic_shock, non_economic_shock = non_economic_shock, 
                     debt_outcomes = debt_outcomes, debt_structure = debt_structure))
  
  type = data.frame(type)
  type$Type = rownames(type)
  names(type) = c("variable", "type")
  type = type %>% mutate(type = str_remove_all(type, "\\d"))
  
  # typology in terms of Economic domain
  
  Fiscal_policy = c("Banking_crisis","Banking_crisis_severe", "Financial_crisis","Trade_crisis", 
                    "Natural_disaster", "Commodity_crisis", "Wars","Severe_recession", "Soft_recession", 
                    "Expansion", "Fiscal_outcomes", "Sovereign_default","Political_crisis", "Social_crisis",
                    "Disbursement","Fiscal_consolidation","Short_term_debt","Floating_rate_debt",
                    "Concessional_lending","Foreign_debt")
  
  Monetary_policy = c("Inflation_crisis", "Balance_payment_crisis", 
                      "Reduction_reserves", "Currency_crisis","Currency_crisis_severe","Global_depreciation",
                      "Floating_exchange_rate","Fixed_exchange_rate","Losening_monetary_policy","Tightening_monetary_policy")
  
  Structural_policy = c("Trade_crisis","Housing_crisis","Performance_criterion","Program_extension",
                        "Deregulation", "Reform_agenda", "Trade_reforms", "Financial_reforms", 
                        "Labor_market_reforms", "Tax_reforms", "Banking_reforms", "Fiscal_consolidation", 
                        "Success_of_reforms","Official_support","Technical_assistance","uncertainty_reforms")
  
  forward_guidance=c("Expectations","Contagion","World_outcomes","Precautionary_programs")
  
  domain = unlist(list(Fiscal_policy = Fiscal_policy, Monetary_policy = Monetary_policy, 
                       Structural_policy = Structural_policy,forward_guidance=forward_guidance))
  
  domain = data.frame(domain)
  domain$Type = rownames(domain)
  names(domain) = c("variable", "domain")
  domain = domain %>% mutate(domain = str_remove_all(domain, "\\d"))
  
  # typology in terms of Nature of the shocks
  
  Other_Financial_shock = c("Banking_crisis","Banking_crisis_severe", "Financial_crisis", "Balance_payment_crisis", 
                            "Reduction_reserves", "Currency_crisis","Currency_crisis_severe","World_outcomes", "Contagion",
                            "Expectations","Housing_crisis")
  
  Fiscal_financial_shock = c( "Fiscal_outcomes", "Sovereign_default","Short_term_debt","Floating_rate_debt",
                              "Concessional_lending","Foreign_debt")
  
  Real_shock = c("Severe_recession", "Soft_recession", "Epidemics","Expansion","Trade_crisis","Inflation_crisis",
                 "Political_crisis", "Social_crisis","Natural_disaster","Commodity_crisis", "Wars")
  
  Economic_ajustement=c("Deregulation", "Reform_agenda", "Trade_reforms", "Financial_reforms", 
                        "Labor_market_reforms", "Tax_reforms", "Banking_reforms", "Fiscal_consolidation", 
                        "Success_of_reforms","Performance_criterion","Program_extension","Official_Support",
                        "Technical_assistance","Precautionary_programs","Official_support")
  
  nature = unlist(list(Other_Financial_shock = Other_Financial_shock, Fiscal_financial_shock = Fiscal_financial_shock, 
                       Real_shock = Real_shock,Economic_ajustement=Economic_ajustement))
  
  nature = data.frame(nature)
  nature$Type = rownames(nature)
  names(nature) = c("variable", "nature_shock")
  nature = nature %>% mutate(nature_shock = str_remove_all(nature_shock, "\\d"))
  
  
  # Transmission to public finance
  
  Expenditure_shock = c("Banking_crisis","Banking_crisis_severe", "Financial_crisis","World_outcomes", "Contagion",
                        "Expectations","Epidemics","Housing_crisis")
  
  Revenue_shock = c("Balance_payment_crisis", "Natural_disaster","Commodity_crisis", "Wars",
                    "Reduction_reserves", "Currency_crisis","Currency_crisis_severe","Trade_crisis","Inflation_crisis",
                    "Official_support")
  
  Debt_shock = c("Fiscal_outcomes", "Sovereign_default","Short_term_debt","Floating_rate_debt",
                 "Concessional_lending","Foreign_debt")
  
  
  Adjustment_shock=c("Deregulation", "Reform_agenda", "Trade_reforms", "Financial_reforms", 
                     "Labor_market_reforms", "Tax_reforms", "Banking_reforms", "Fiscal_consolidation", 
                     "Success_of_reforms","Performance_criterion","Program_extension","Official_Support",
                     "Technical_assistance","Precautionary_programs","Political_crisis", "Social_crisis")
  
  Output_shock=c("Severe_recession","Soft_recession", "Severe_recession", "Soft_recession", "Expansion")
  
  Spread_shock=c("Expectations","Contagion","Precautionary_program")
  
  risk_free_rate=c("World_outcomes")
  
  channel = unlist(list(Expenditure_shock = Expenditure_shock, Revenue_shock = Revenue_shock, 
                        Debt_shock = Debt_shock,Adjustment_shock=Adjustment_shock,Output_shock=Output_shock,
                        Spread_shock=Spread_shock,risk_free=risk_free_rate))
  
  channel = data.frame(channel)
  channel$Type = rownames(channel)
  names(channel) = c("variable", "channel")
  channel = channel %>% mutate(channel = str_remove_all(channel, "\\d"))
  
  
  
  res = type %>% mutate(variable=as.character(variable))%>% left_join(nature %>% mutate(variable=as.character(variable)), by = c("variable"))
  res = res %>% left_join(domain%>% mutate(variable=as.character(variable)), by = c("variable"))
  res = res %>% left_join(channel%>% mutate(variable=as.character(variable)), by = c("variable"))
  
  res
}
