
#necesary packages and dependencies
# 
# library(pdftools)
# library(xml2)
# library(rvest)
# library(stringr)
# library(tidytext)
# library(dplyr)
# library(tidyr)
# library(tictoc)
# library(crayon)

#-----------------------------------------------------------------------
# lexicon of economic crisis

# supervised lexicon 
key_words_crisis=function(){
  
  #key words by type of crisis/distress, capital letter do not matter
  key_words=list()
  
  #--------
  ## REFORMS AND CONDITIONALITY
  #--------
  
  key_words[["Deregulation"]]=c(
    "privatization",
    "liberalization",
    "liberalize",
    "privatize",
    "deregulate",
    "deregulation"
  )
  
  key_words[["Reform_agenda"]]=c(
    "reform",
    "reforming",
    "adjustment program",
    "structural adjustments",
    "packages",
    "structural program"
  )
  
  key_words[["Trade_reforms"]]=c(
    "trade and tariff programme",
    "trade and tariff program",
    "trade and tariff reform",
    "trade liberalization",
    "liberalization of trade",
    "liberalization of tariffs",
    "remove tariffs",
    "removing tariffs"
  )
  key_words[["Financial_reforms"]]=c(
    "financial reform",
    "financial sector reform",
    "reforms to the financial sector",
    "reforming the financial sector",
    "regulation of the financial sector",
    "regulating the financial sector"
  )
  
  key_words[["Banking_reforms"]]=c(
    "banking reform",
    "banking sector reform",
    "reforms to the banking sector",
    "reforming the banking sector",
    "reforming bank",
    "regulation of the banking sector",
    "regulating the financial sector"
  )
  
  
  #--------
  ## OFFICIAL CREDITORS INTERVENTION
  #--------
  
  key_words[["Official_support"]]=c(
    "ESFS",
    "European system of financial supervision",
    "ESM",
    "IRDB",
    "development banks",
    "official lending",
    "official support",
    "World Bank",
    "Official bilateral creditors",
    "Inter-American Development Bank",
    "Inter American Development Bank",
    "European stability mechanism",
    "Fed swap lines")
  
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
    "internal conflict",
    "armed domestic conflict",
    "israeli conflict",
    "sahara conflict",
    "violent conflict",
    "military conflict",
    "conflict regions",
    "conflict zone",
    "civil conflict",
    "armed conflicts",
    "guerilla offensive",
    "military take-over"
  )
  
  key_words[["Natural_disaster"]]=c(
    "flooding",
    "drought",
    "record-low levels of rainfall",
    "natural calamities",
    "natural disaster",
    "earthquake",
    "hurricane")
  
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
    "disturbed political conditions",
    "political and economic developments",
    "political and security situation",
    "economic and political situation",
    "political crisis",
    "unsettled political  situation",
    "political tensions",
    "geopolitical events",
    "geopolitical risk",
    "complex geopolitical situation",
    "geopolitical tensions",
    "geopolitical turmoil",
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
  
  
  #--------
  ## EXOGENEOUS SHOCKS; ECONOMIC
  #--------
  
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
    "bailout",
    "crisis in the banking sector",
    "collapse of the banking sector",
    "increase in nonperforming loans",
    "recapitalization of the banks",
    "recapitalizing the banking system",
    "recapitalizing the banking sector",
    "banking system collapsed",
    "collapsed in the banking system",
    "banking system stability",
    "pressure on the banking",
    "bankrun",
    "bank recapitalization",
    "recapitalization",
    "bank restructuring",
    "recapitalize private financial institutions",
    "confidence in the domestic banking system",
    "strengthen bank supervision",
    "financial support package")
  
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
    "infaltion has reached",
    "inflation at unprecedented levels",
    "large monetary creation",
    "lowering the rate of inflation",
    "pressure on prices",
    "high inflation",
    "severe inflation",
    "combat inflation",
    "cronic inflation",
    "sharpe increase in domestic prices",
    "large increase in domestic prices",
    "inflationary pressures",
    "inflationary pressure",
    "inflation_crisis")
  
  key_words[["Crisis_contagion"]]=c(
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
    "risk of system insolvency",
    "global crisis",
    "global economic crisis",
    "crisis in Brazil",
    "linkage with the US")
  
  
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
    "global financial shock",
    "recession in the world economy",
    "international monetary crisis",
    "worsening international environement",
    "difficult external environment",
    "downside risks in the international environment",
    "further deterioration in the international environment",
    "uncertain external environment",
    "slowdown in international economy"
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
    "contagion from the crisis in neightboring",
    "external shocks",
    "external shock",
    "adverse exogenous events",
    "external vulnerability",
    "exogenous events",
    "contagion",
    "fears of contagion",
    "spillovers",
    "systemic spillovers",
    "vulnerability to international")
  
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
    "decline in official reserves",
    "loss of official reserves",
    "loss of international reserves",
    "decline in international reserves",
    "loss of official international reserves",
    "decline in official international reserves",
    "exhaustion of the disposable official international reserves",
    "depleted international reserves",
    "exhausted official international reserves"
  )
  
  key_words[["Currency_crisis"]]=c(
    "currency weakness",
    "exchange rate crisis",
    "large real depreciation",
    "foreign exchange crisis",
    "disruption of exchange  markets",
    "major devaluation",
    "Currency crisis",
    "Currency crash",
    "pressure on the exchange system",
    "large devaluation",
    "large depreciation",
    "cumulative depreciation",
    "sharpe depreciation",
    "sharp depreciation of the exchange rate",
    "magnitude of the depreciation",
    "pressure in the foreign exchange markets")
  
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
    "adjustable exchange rate band system")
  
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
    "slowdown of output",
    "economic decline",
    "declining trend in economic activity",
    "slowing down of business activity",
    "slow down",
    "low rates of economic growth",
    "low rate of economic growth",
    "economic activity on a downward trend",
    "depressed level of economic activity",
    "the economic situation worsen",
    "decline in economic activity",
    "recession",
    "contraction of output",
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
    "recovery",
    "expansion of economic activity",
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
    "tightening of fiscal policy",
    "reduction in public debt",
    "reduce public sector deficit",
    "containing expenditures",
    "strengthening annual fiscal targets",
    "reduce substantially the budgetary deficit",
    "contain the rise in expenditure",
    "substantial adjustments with respect to the budget",
    "freeze wages",
    "rationalization of public sector expenditure",
    "improve the financial position",
    "control over noninterest expenditures",
    "nonpriority expenditure",
    "long term financial strength",
    "reduce operational deficits",
    "fiscal performance",
    "reduction of the deficit",
    "actions on the public finance",
    "measures in the fiscal field",
    "fiscal performance criteria",
    "revenue measures",
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
    "primary fiscal balance"
  )
  
  key_words[["Concessional_lending"]]=c(
    "concessional terms"
  )
  key_words[["Short_term_debt"]]=c(
    "short term borrowing",
    "short-term maturities",
    "short term public debt",
    "debt maturing in less than 12 months",
    "shortening of public debt maturity",
    "maturity shortening")
  
  
  key_words[["Sovereign_default"]]=c(
    "rescheduled debt",
    "external payments crisis",
    "liquidity crisis",
    "difficulties in servicing its external debt",
    "rescheduling of external debt",
    "suspend service payments",
    "fiscal crisis",
    "debt relief",
    "government bonds crisis",
    "government bonds crisis",
    "bond crisis",
    "debt reprofiling",
    "sovereign debt crisis",
    "public debt crisis",
    "default risks",
    "self fulfilling crises",
    "debt restructuring program",
    "governement default",
    "restructuring of debt",
    "suspension of payments",
    "debt restructuring",
    "difficult time in rolling over its debt",
    "debt rescheduling",
    "debt service reduction operation",
    "debt restructuring program",
    "rescheduling of the debt",
    "arrears",
    "rescheduling of arrears",
    "arrears in the payment",
    "restructuring of its external debt",
    "restructuring agreements",
    "external payment arrears",
    "external arrears",
    "interest arrears",
    "arrears on payments",
    "arrears on debt service",
    "external payment arrears",
    "debt service reduction",
    "no debt service payments",
    "restructure these arrears",
    "relation with external creditors",
    "Paris Club",
    "Club of Paris",
    "debt relief",
    "debt exchange"
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
  
  #"floating interest rates",
  #"floating rate",
  #"foreign exchange-indexed",
  #"exchange rate-indexed bonds",
  #"exchange-indexed securities",
  #"foreign exchange swaps",
  #"dollar-indexed securities",
  
  
  #----
  key_words
}

key_words_categories=function(){
  
  cat_1=c("Political_crisis",
          "Wars",
          "Natural desaster",
          "World_outcomes",
          "Currency_crisis",
          "Severe_recession",
          "Soft_Recession",
          "Commodity_crisis",
          "Banking_crisis",
          "Inflation_crisis")
  
  cat_1=unlist(key_words_crisis()[cat_1],recursive = T)
  
  cat_2=c("Crisis_contagion",
          "Balance_payment_crisis",
          "Contagion",
          "Expectations",
          "Financial_crisis",
          "Sovereign_default",
          "Precautionary_programs")
  cat_2=unlist(key_words_crisis()[cat_2],recursive = T)
  
  cat_3=c("Official_support",
          "Reform_agenda",
          "constraining",
          "Fiscal_Outcomes")
  cat_3=unlist(key_words_crisis()[cat_3],recursive = F)
  
  return(list("Exogenous"=cat_1,
              "Manifestations"=cat_2,
              "Instruments"=cat_3)
  )
}

key_words_countries=function(){
  #table of adjectival and names of countries 
  if(curl::has_internet()){
    path="https://en.wikipedia.org/wiki/List_of_adjectival_and_demonymic_forms_for_countries_and_nations"
    html_file <- try(xml2::read_html(path), silent=T)
    ctry_table=html_file %>% rvest::html_nodes(xpath='//*[@id="mw-content-text"]/div/table[1]') %>% rvest::html_table(fill=T)
    ctry_table=ctry_table[[1]] 
    
    ctry_table=data.frame(ctry_table)[-1,]
    colnames(ctry_table)=c("ctries","adjectival","Demonyms","Demonyms_2","X")
    
    #currencies 
    path="https://en.wikipedia.org/wiki/List_of_circulating_currencies"
    html_file <- try(xml2::read_html(path), silent=T)
    currency_table=html_file %>% rvest::html_nodes(xpath='//*[@id="mw-content-text"]/div/table[1]') %>% rvest::html_table(fill=T)
    currency_table=currency_table[[1]]
    names(currency_table)=c("ctries","Currency","Symbol","ISO2_Code","fractional_unit","basis")
    table=dplyr::left_join(x=ctry_table,y=currency_table,by="ctries")
    
  }else{
    ctry_table=rio::import("../3. Data/List countries/wikipedia_country_names.RData")
  }
  return(table)
}

# unsupervised lexicon

remove_stop=function(word){
  #remove stop words
  ifelse(word %in% stop_words$word,"",word)
}

clean_ngram=function(text,length_ngram=4){
  #generate tibble with ngrams of specified length in rows and dummies 
  #for  a set of typologies, the function clean the text by removing 
  #unnecessary characters and removing common stop words.
  
  #parameters:
  # text: a character string containing the text to analyse
  # length_ngram the number of word to consider has the unit of observation
  
  #output:
  # a tibble with ngrams in rows, number of occurence and dummies for the typologies 
  text= text %>% clean_text() %>% tibble() 
  text=text %>% unnest_tokens(ngram, text, token = "ngrams", n = length_ngram)
  
  #remove stop words
  
  list_nwords=paste("word",1:length_ngram,sep="")
  ngrams_sep=text %>% 
    dplyr::select(ngram) %>% 
    tidyr::separate(ngram, list_nwords, sep = " ")
  ngrams_sep=ngrams_sep %>% dplyr::mutate_at(vars(matches("word")),remove_stop) %>%
    tidyr::unite("word",list_nwords,sep=" ") %>%
    dplyr::select(word) %>%
    dplyr::mutate(word=stringr::str_trim(word),
           word=stringr::str_replace(word,"  "," ")) %>% dplyr::filter(word!="")


  #remove figures and numeric variables 
  ngrams_sep=ngrams_sep %>% dplyr::filter(!stringr::str_detect(word,"\\d") | !stringr::str_detect(word,"perce") )
  
  ngrams_sep=ngrams_sep %>%
    dplyr::count(word,sort=T)
  
  #filter type of words
  ngrams_sep=ngrams_sep %>% dplyr::mutate(crisis=ifelse(str_detect(word,"crisis") | 
                                                   stringr::str_detect(word,"distress") |
                                                   stringr::str_detect(word,"downturn"),1,0),
                                   law=ifelse(stringr::str_detect(word,"law") | 
                                                stringr::str_detect(word,"legal") |
                                                stringr::str_detect(word,"regulation")|
                                                stringr::str_detect(word,"supervision"),1,0),
                                   finance=ifelse(stringr::str_detect(word,"finance"),1,0),
                                   supervision=ifelse(stringr::str_detect(word,"supervis"),1,0),
                                   solvency=ifelse(stringr::str_detect(word,"solven"),1,0),
                                   liquidity=ifelse(stringr::str_detect(word,"liquid"),1,0),
                                   budget=ifelse(stringr::str_detect(word,"budget"),1,0),
                                   guarantee=ifelse(stringr::str_detect(word,"guarantee"),1,0),
                                   nationalization=ifelse(stringr::str_detect(word,"nationalization"),1,0),
                                   overdue=ifelse(stringr::str_detect(word,"overdue"),1,0),
                                   accomodative=ifelse(stringr::str_detect(word,"accomodative"),1,0),
                                   administrative=ifelse(stringr::str_detect(word,"administrative"),1,0),
                                   economic=ifelse(stringr::str_detect(word,"economic"),1,0),
                                   money=ifelse(stringr::str_detect(word,"money"),1,0),
                                   arrears=ifelse(stringr::str_detect(word,"arrears"),1,0),
                                   exchange=ifelse(stringr::str_detect(word,"exchange"),1,0),
                                   depreciation=ifelse(stringr::str_detect(word,"depreciation"),1,0),
                                   banks=ifelse(stringr::str_detect(word,"bank"),1,0),
                                   capital=ifelse(stringr::str_detect(word,"capital"),1,0),
                                   asset=ifelse(stringr::str_detect(word,"asset"),1,0),
                                   reform=ifelse(stringr::str_detect(word,"reform"),1,0),
                                   debt=ifelse(stringr::str_detect(word,"debt"),1,0),
                                   loans=ifelse(stringr::str_detect(word,"loan")|
                                                  stringr::str_detect(word,"portfolio")|
                                                  stringr::str_detect(word,"lend"),1,0)) 
  
  
  #filter according to 
  ngrams_sep=ngrams_sep %>% dplyr::filter(crisis==1 | law==1 |finance==1 |
                                     supervision==1 |solvency==1 |liquidity==1 |
                                     budget==1 |guarantee==1 |nationalization==1 |
                                     overdue==1 |accomodative==1 |administrative==1 |
                                     economic==1 |money==1 |arrears==1 |exchange==1 |
                                     depreciation==1 |banks==1 |capital==1 |
                                     asset==1 |reform==1 |debt==1 |loans==1)
  
  return(ngrams_sep)
}

assoc_words=function(cleaned_ngram,type_word,min_occurence=5){
  # from the table of ngrams generated by clean_ngram() find the most common associated words by 
  # typology:
  
  #parameters:
  # clean_ngram: output from clean_ngram()
  # type_word: the name of the category to analyse (check outptu of clean_ngram() to see the available list)
  # min_occurence: only display the words with at least this specified number of occurence
  dt= cleaned_ngram %>% 
    dplyr::filter(get(type_word)==1) %>% grou
    tidyr::separate(word, "list_nwords", sep = " ") %>% tidyr::gather() %>% dplyr::filter(value!="") %>% dplyr::group_by(value) %>% 
    dplyr::summarize(n=n()) %>% dplyr::filter(!stringr::str_detect(value,"\\d"))%>% dplyr::arrange(-n) %>% dplyr::rename(word=value)
  
  dt=dt %>% dplyr::filter(!is.na(word) & word!="" & n>min_occurence)
  return(dt)
}

unsupervized_corpus=function(corpus,length_ngram=2,min_occurence=10){
  mycorpus=lapply(1:length(corpus),function(x){
    text=corpus[[x]]
    tictoc::tic()
    cleaned_ngram=clean_ngram(text,length_ngram = length_ngram)  
    cleaned_ngram=cleaned_ngram %>% dplyr::mutate(file=names(corpus)[x])
    cleaned_ngram=cleaned_ngram %>% dplyr::select(word,file,n,everything())
    tictoc::toc()
    cleaned_ngram
  })
  mycorpus=do.call(rbind,mycorpus)
  mycorpus=mycorpus %>% dplyr::filter(!grepl("\\d",word))
  assoc_words=list()
  assoc_words[["crisis"]]=assoc_words(mycorpus,"crisis",min_occurence = min_occurence)
  assoc_words[["banks"]]=assoc_words(mycorpus,"banks",min_occurence = min_occurence)
  assoc_words[["debt"]]=assoc_words(mycorpus,"debt",min_occurence = min_occurence)
  assoc_words[["reform"]]=assoc_words(mycorpus,"reform",min_occurence = min_occurence)
  assoc_words[["budget"]]=assoc_words(mycorpus,"budget",min_occurence =min_occurence )
  return(list(corpus=mycorpus,associated_words=associated_words))
}

#--------------------------------------------------------------
#clean text and find target words

clean_text=function(file){
  #first step cleaning of the file by removing preselected character
  #file=stringr::str_replace_all(file,"[^a-zA-Z\\s]", " ")
  file=stringr::str_replace_all(file,"[\\s]+", " ")
  file=gsub("\r","",file)
  file=gsub('\"',"",file)
  file=gsub("\n","",file)
  file=file[stringr::str_count(file)>50]
  file=gsub("<U+23AF>","",file)
  file=tolower(file)
  file
}

find_pages=function(file,targetword){
  #provide files either pdf of html and return the paragraphs matching the targetted word
  #parameters
  #file:a character string correspond to the text to analysis
  # targetword: a vector of characters corresponding to word to search and count for 
  # in the text
  
  require(stringi)
  require(stringr)
  
  page_locations=list()
  target_pages=list()
  i=0
  Tot.occurence=0
  file=clean_text(file)
  n.chars=sum(nchar(file)) #total number of characters in the file after cleaning
  page_location=sapply(1:length(file),function(x){
    page=file[[x]] 
    page=tibble(page) %>%  tidytext::unnest_tokens(word,page,token="ngrams",n=3) %>% dplyr::mutate(word=tolower(word))
    n.occurence=(page %>% dplyr::filter(grepl(paste(tolower(targetword),collapse="|"),word)) %>% dplyr::summarize(count=n()))$count
    condition= n.occurence>0
    if(any(condition)){
      i<<-i+1
      page_locations[[i]]<<-paste0("Found in page ",x," :" ,n.occurence," times")
      target_pages[[i]]<<-file[[x]]
      Tot.occurence<<-Tot.occurence+n.occurence
      return("yes")
    }else {return("no")}
    
  })
  
  return(list(target=targetword,N.chars=n.chars,N.Occurence=page_locations,Tot.occurence=Tot.occurence,pages=target_pages))
}

eval_pages=function(files,targetword,brute_freq=F,parrallel=T){
  #provide list of files and return summary of counts of occurence of the target world 
  #parameters:
  #file:a character string correspond to the text to analysis
  # targetword: a vector of characters corresponding to word to search and count for 
  # in the text
  # brute_freq: T if you only want the frequency count of the word, if F the output is the 
  # it provides the term frequency for the set of targetwords provided
  # parrallel: if T the function use the mclapply function to use parrallele computing
  # from the package 'parrallele"
  
  if(parrallel){
    metric=parallel::mclapply(files,function(x){
      file=find_pages(x,targetword)
      if(brute_freq){
        file$Tot.occurence
      }else{file$Tot.occurence/file$N.chars}
      
    }, mc.cores = parallel::detectCores()-1, mc.allow.recursive = TRUE)
    N.Occurence=do.call(rbind,metric)
    if(length(targetword)==1){
      colnames(N.Occurence)=targetword
    }else colnames(N.Occurence)="Occurence"
  }else{
    metric=lapply(files,function(x){
      file=find_pages(x,targetword)
      if(brute_freq){
        file$Tot.occurence
      }else{file$Tot.occurence/file$N.chars}
      
    })
    N.Occurence=do.call(rbind,metric)
    if(length(targetword)==1){
      colnames(N.Occurence)=targetword
    }else colnames(N.Occurence)="Occurence"
  }
  # N.Occurence=data.frame(N.Occurence) %>% dplyr::mutate(ISO3_Code=substr(rownames(a),4,6),
  #                            year=substr(rownames(a),str_locate(rownames(a), "_2")+1,str_locate(rownames(a), "_2")+4),
  #                            path_file=substr(rownames(a),16,80),
  #                            Occurence=as.numeric(as.character(Occurence)))
  return(N.Occurence)
}

pdf_page_count=function(files){
  #count the number of pages in the pdf
  #parameters:
  # files: a list of character strings
  
  error_no_metadata=try({files[[1]]$info},silent = T)
  if("try-error" %in% class(error_no_metadata)){
    cat(crayon::red("No metadata of document available, please run aggregate_corpus() setting the argument only_files=F \n"))
    #return(NULL)
  }else{
  table_count=lapply(files,function(x){
    #n.words=count_words(x$file)# %>% arrange(n) #%>% dplyr::summarize(word=paste(paste(word,'(',n,')',sep=""),collapse=", "))
    n.pages=x$info$pages
    n.pages
    #list(n.words=n.words,n.pages=n.pages)
  })
  
  names(table_count)=names(files)
  table_count=do.call(rbind,table_count)
  table_count=data.frame(table_count)
  table_count$File=names(files)
  table_count %>% dplyr::select(File,N.pages=table_count)
  }
}

#--------------------------------------------------------------
#Compute frequencies for a corpus of texts and the lexicon of words for crisis 
tf=function(corpus,keywords,brute_freq=F,parrallel=T){
  #for each document in the corpus the function creates a table counting the occurence of
  #the words in the vector of keywords and provide a table with the number of occurence,
  #name of the file that is the name of each document in the corpus (from which we can extract: the country code,
  #period, type of program and year)
  
  #parameters:
  # corpus: a list of texts from pdf_text() with different names for each element
  # keywords: a vector of strings containing the targeted words to look for
  
  if(is.vector(keywords) & !is.null(keywords)){
    table=lapply(corpus,function(x){
      output=try(sum(eval_pages(x,keywords,brute_freq=brute_freq,parrallel = parrallel)[,1]))
      if("try-error" %in% class(output)){
        print(paste0("Warning: error when mining ",keywords))
        output=0
      }
      output
    })
    names(table)=names(corpus)
    table=do.call(rbind,table)
    table=dplyr::tibble(table)
    table$file=names(corpus)
    names(table)=c("var","file")
    #table=table %>% dplyr::mutate(#Recession=ifelse(Recession>0,1,0),
    #   ISO3_Code=substr(file,1,3),
    #   Period=as.Date(substr(file,5,14)),
    #   type_prog=substr(file,16,23),
    #   year=year(Period))
    
    table
  }else{print('please provide a vector of strings as argument for keywords')}
}

tf_vector=function(corpus,keyword_list,brute_freq=F,parrallel=T){
  #vectorize the function tf() to be able to pass a list of names of keywords
  #keyword_list is a list containing the names of different groups of keywords that have a vector
  #of words to look into.
  
  #parameteres:
  # corpus: a list of texts from pdf_text() with different names for each element
  # keyword_list: the names of the items in keyword_list_crisis to include in the computation
  
  #outptut:
  # a table with each row corresponding to a document and each colum providing the number of occurence for the given
  # item.
  
  #my_keywords=key_words_crisis()
  list_table_keyword_occurence=lapply(1:length(keyword_list),function(x){
    print(paste0("running: ",names(keyword_list)[x]))
    tictoc::tic()
    if(!"character" %in% class(keyword_list[[x]])){
      warning("please provide a valid vector of characters")
      dt=NULL
      tictoc::toc()
      dt
    }else{
      res=try({
        dt=tf(corpus,keyword_list[[x]],brute_freq=brute_freq,parrallel=parrallel)
        dt=dt%>% dplyr::rename(!!paste0(names(keyword_list)[x]):=var)
        dt
      })
      if("try-error" %in% class(dt)){
        res=NULL
      }
      tictoc::toc()
      res
    }
  })
  names(list_table_keyword_occurence)=names(keyword_list)
  
  dt=list_table_keyword_occurence[[1]]
  for(i in 2:length(list_table_keyword_occurence)){
    res=try({
      dt=dt %>% dplyr::left_join(list_table_keyword_occurence[[i]],by=c("file")) #"ISO3_Code","Period","type_prog","year",  
      dt    
    })
    if("try-error" %in% class(res)){
      dt=dt
    }else{
      dt=res
      dt=dt %>% dplyr::select(file,everything()) %>% dplyr::distinct() #ISO3_Code,Period,year,type_prog,
    }
  }

  return(dt)
}

#----------------------------------------------------------------
#functions to transform brut frequency into alternative metrics
binary_freq_trans=function(table_N_occurence){
  #transform table from Number of occurence to binary variables
  binary_trans=function(x){ifelse(x>0,1,0)}
  table_brut_frequency=table_N_occurence %>% dplyr::mutate_if(is.numeric,binary_trans)
  return(table_brut_frequency)
}

log_norm_trans=function(table_N_occurence){
  #log normal transformation of the table
  log_norm_trans=function(x){ifelse(x>0,1+log(x),0)}
  table_log_norm_trans=table_N_occurence %>% dplyr::mutate_if(is.numeric,log_norm_trans)
  return(table_log_norm_trans)
}

#function to finds the relative importance of words in a corpus
#THE FUNCTION IS NOT WORKING PROPERLY WHEN USING GROUP
idf=function(table_N_occurence,group=NULL){
  #compute the inverse document frequency as the logarithm of the inverse of the proportion. it
  #allows to give reduce weight of words with high frequency in the corpus
  
  N.doc.corpus=dim(table_N_occurence)[1]
  idf_trans=function(x){
    if(x==0){
      x
    }else{log(N.doc.corpus/sum(x))}
  }
  
  table_N_binary=binary_freq_trans(table_N_occurence)
  
  if(!is.null(group)){
    inverse_doc_freq=table_N_binary %>% dplyr::group_by(get(group)) %>%
      dplyr::summarize_if(is.numeric,sum) %>%
      dplyr::mutate_if(is.numeric,idf_trans) %>%
      gather("Crisis",value="idf",-"get(group)") #%>% dplyr::rename(get(group):="get(group)")
    colnames(inverse_doc_freq)[1]=group
  }else{
    inverse_doc_freq=table_N_binary %>%
      dplyr::summarize_if(is.numeric,sum) %>%
      dplyr::mutate_if(is.numeric,idf_trans)%>%
      gather("Crisis",value="idf")
  }
  return(inverse_doc_freq)
}

#function to compute the weights of each word by doing the product of the index of frequency and the inverse
#document frequency
tf_idf=function(table_N_occurence,weight_method="brut_frequency"){
  #function to compute the weights of each word by doing the product of the index of frequency and the inverse
  #document frequency
  dt_inv_doc_freq=try(idf(table_N_occurence))
  if("try_error" %in% dt_inv_doc_freq){
    print(paste0("Warning: error when using function dt_inv_doc_freq"))
    return(NULL)
  }
  #select_cols=names(table_N_occurence)[!names(table_N_occurence) %in% c("file","ISO3_Code","Period","year","variable","Review")] 
  select_cols=names(table_N_occurence %>% dplyr::select_if(is.numeric))
  
  if(weight_method=="brut_frequency"){
    dt_words_weight=table_N_occurence
    for(var in select_cols ){
      dt_words_weight[,var]=dt_words_weight[,var]*(dt_inv_doc_freq %>% dplyr::filter(Crisis==var) %>% dplyr::select(idf))[[1]]
    }
    return(dt_words_weight)
  }else if(weight_method=="binary_frequency"){
    dt_words_weight=binary_freq_trans(table_N_occurence)
    for(var in select_cols ){
      dt_words_weight[,var]=dt_words_weight[,var]*(dt_inv_doc_freq %>% dplyr::filter(Crisis==var) %>% dplyr::select(idf))[[1]]
    }
    return(dt_words_weight)
  }else if(weight_method=="log_norm_frequency"){
    dt_words_weight=log_norm_trans(table_N_occurence)
    for(var in select_cols ){
      dt_words_weight[,var]=dt_words_weight[,var]*(dt_inv_doc_freq %>% dplyr::filter(Crisis==var) %>% dplyr::select(idf))[[1]]
    }
    return(dt_words_weight)
  }else warning("please choose a proper method: brut_frequency,binary_frequency,log_norm_frequency")
}


#function to run the tf by providing the list of texts and the type of lexicon
run_tf=function(corpus_path=paste0(root_path,"/3. Data/IMF Letters of Intents/IMF_letofIntent_1960_2014_clean.RData"),
                type_lexicon="words", #category
                keyword_list=c(
                  "Commodity_crisis",
                  "Balance_payment_crisis",
                  "Inflation_crisis"), #c("Exogenous","Manifestations","Instruments")
                export_path=NULL,
                parrallel=T){
  
  # function that compute the tf matrix for a corpus given in a list format,
  # the type of lexicon to use and the sublist of keywords associated
  # The output is a matrix of tf with a row per document and a column for 
  # each element of the keyword list
  
  print(paste0("Loading corpus from ",corpus_path))
  #load(file=corpus_path)
  corpus=rio::import(corpus_path)
  #corpus=IMF_LoI
  if(type_lexicon=="words"){
    if(is.null(keyword_list)){
      keyword_list=c(
        "Commodity_crisis",
        "Balance_payment_crisis",
        "Inflation_crisis",
        "World_outcomes",
        "Floating_Exchange rate",
        "Fixed_Exchange rate",
        "Wars",
        "Soft_Recession",
        'Fiscal_Outcomes',
        "Financial_crisis",
        "Banking_crisis",
        "Currency_crisis",
        "Severe_recession",
        "Crisis_contagion",
        "Sovereign_default",
        "Expectations",
        "Precautionary_programs",
        "Natural_desaster",
        "Loss_Market_access",
        "Political_crisis",
        "Constraining",
        "Contagion")
      print(keyword_list)
    }
    #print("RUNNING: ")
    #print(keyword_list)
    keyword_list=key_words_crisis()[keyword_list]  
  }else if(type_lexicon=="category"){
    if(is.null(keyword_list)){
      keyword_list=c(
        "Exogenous",
        "Manifestations",
        "Instruments")
    }
    #print("RUNNING: ")
    #print(keyword_list)
    keyword_list=key_words_categories()[keyword_list]
  }else{
    print("Please provide a valid type_lexicon, either 'words' or 'category'")
    return(NULL)
  }
  
  tictoc::tic()
  dt=tf_vector(corpus,keyword_list,parrallel = parrallel)
  tictoc::toc()
  destination=paste0(export_path,"/data_LoI_N_Occ_crisis_",type_lexicon,".RData")
  print(paste0("export table in ",corpus_path))
  if(!is.null(export_path)){
    rio::export(dt,destination)
  }
  dt
}

run_tf_update=function(path_tf_to_update=paste0(root_path,"/3. Data/IMF Letters of Intents/data_LoI_N_Occ_crisis_words.RData"),
                       corpus_path=paste0(root_path,"/3. Data/IMF Letters of Intents/IMF_letofIntent_1960_2014_clean.RData"),
                       type_lexicon="words", #category
                       keyword_list=NULL, #c("Exogenous","Manifestations","Instruments")
                       export_path=paste0(root_path,"/3. Data/IMF Letters of Intents/data_LoI_N_Occ_crisis_words.RData"),
                       parrallel=T){
  
  # function that update the tf  output of run_tf with the new variables 
  # avoid having to rerun all categories
  
  if(is.null(keyword_list)){
    print("Updating all columns")
    new_tf=run_tf(corpus_path=corpus_path,
                  type_lexicon=type_lexicon,
                  keyword_list=key_words_crisis(),
                  export_path = paste0(root_path,"/3. Data/IMF Letters of Intents/data_LoI_N_Occ_crisis_words.RData"), 
                  parrallel=parrallel)
    return(new_tf)
  }else{
    
    print("updating selected columns")
    tf_to_update=rio::import(path_tf_to_update) 
    dim_tf_to_update=dim(tf_to_update)
    existing_cols=names(tf_to_update)
    
    if(any(existing_cols %in% keyword_list)){
      tf_to_update=tf_to_update %>% dplyr::select(-keyword_list)
    }
    
    corpus=rio::import(corpus_path)
    
    new_tf=run_tf(corpus_path=corpus_path,type_lexicon=type_lexicon,keyword_list=keyword_list,parrallel=parrallel)
    
    tf_to_update=dplyr::left_join(x=tf_to_update,y=new_tf,by="file")
    
    print(paste0("Non updated columns:\n
                 ",paste0(existing_cols,collapse=", ")))
    
    print(paste0("Updated columns:\n
                 ",paste0(keyword_list,collapse=", ")))
    rio::export(tf_to_update,export_path)
    
    return(tf_to_update)
  }
  
  
}

#function to aggregate corpus
aggregate_corpus=function(path_files,only_files=F){
  #Description:
  # function that takes the path of the directory and load all the pdfs of the directory 
  # into a list in order to further perform the text mining
  
  #parameters:
  # path_files: the path of the directory with the files 
  
  #output:
  # a list containing the content of each document
  
  docs=list.files(path_files,pattern=".pdf")
  docs=stringr::str_remove(docs,".PDF") 
  docs=stringr::str_remove(docs,".pdf")
  count=0
  start=1
  x=1 
  corpus=lapply(start:length(docs),function(x){
    tictoc::tic(docs[x])
    path=paste0(path_files,"/",docs[x],".PDF")
    file <- try({
      pdfinfo=pdf_info(path)
      pdf_text(path) %>% strsplit(split = "\n")
    } , silent=T)
    if("try-error" %in% class(file)) {
      warning(paste(docs[[x]],": Error in path file",sep=""))
      file=NA
    }else {
      file=clean_text(file)
    }
    count<-count+1
    print(path)
    print(count)
    tictoc::toc()
    if(only_files==T){
      file
    }else{
    list(info=pdfinfo,file=file)
    }
  })
  names(corpus)=docs
  return(corpus)
}

#function to download pdf files from urls

pdf_from_url=function(urls,export_path){
  #download from a a dataframe containing the url of the files
  ref_colnames=c("title","reference","hierarchy","date","keywords" ,"pdf") 
  
  if(!dir.exists(export_path)){
    dir.create(export_path,recursive = T)
  }

  if(any(names(urls) %in% ref_colnames)){
    count=0
    lapply(1:dim(urls)[1],function(i){
      count<<-count+1
      filename=urls[i,"hierarchy"]
      tictoc::tic(paste0(urls[i,"hierarchy"]," : ",count,"/",dim(urls)[1]))
      file <- try(download.file(urls[i,"pdf"], destfile=paste0(export_path,"/",stringr::str_replace_all(urls[i,"hierarchy"],"/","_"),".pdf")), silent=T)
      if("try-error" %in% class(file)) {
        cat(crayon::red(paste(urls[i,"hierarchy"],": Error in path file: ",urls[i,"pdf"],sep="")))
        file=NA
      }
      tictoc::toc()
    })
    cat(crayon::green(paste0("urls succesfully downloaded in '",export_path,"'")))
    #print("urls succesfully downloaded")
  }else{cat(crayon::green("Please provide a valid data.frame of url"))}
  
}

# ----------------------------------------------------------------

#formula for the cosinus similiarity that provides the distance between two vectors here
#two crisis
cosim=function(table_N_occurence,vec1,vec2){
  #formula for the cosinus similiarity that provides the distance/similarity between two vectors here
  #two crisis
  #pararmeters:
  # table_N_occurence: a tibble with documents in rows and number of occurence of each type of crisi
  # in columns
  # vec1: the name of the column 1 to analyse
  # vec2: the name of the column 2 to analyse
  #output:
  # a number providing the similarity between the two vectors
  res=table_N_occurence %>% dplyr::select(vec1,vec2) %>%
    dplyr::mutate(scalar_prod=get(vec1)*get(vec2),
           norm_1=(get(vec1))^2,
           norm_2=(get(vec2))^2) %>%
    dplyr::summarize(scalar_prod=sum(scalar_prod),
              norm_1=sum(norm_1),
              norm_2=sum(norm_2)) %>%
    dplyr::mutate(cosinus_similarity=scalar_prod/(sqrt(norm_1)*sqrt(norm_2))) %>%
    dplyr::select(cosinus_similarity)
  
  return(res$cosinus_similarity)
}

#compute the cosinus similarity matrix to how different crisis cluster each others

cosim_matrix=function(table_weights){
  #compute the matrix of cosinus simularity between all the type of crisis 
  #parameters:
  #table_weights: a table of tf-idf with documents in rows and type of crisis in 
  #columns
  
  #vects=names(table_weights)[!names(table_weights) %in% c("file","ISO3_Code","Period","year","variable","Review")] 
  vects=names(table_weights %>% dplyr::select_if(is.numeric))
  res=matrix(NA,length(vects),length(vects),dimnames = list(vects,vects))
  for(j in 1:length(vects)){
    #ind=c(1:(j-1),(j+1):length(vects))
    for(i in 1:length(vects)){#j
      res[i,j]=cosim(table_weights,vects[j],vects[i])
      #print(res)
    }
  }
  
  res[res=="Inf"]=0
  res[res=="NaN"]=0
  res=res %>% round(.,2)
  return(res)
}

#plot results of matric cosinus
plot_cos_sim=function(cos_sim,var){
  dt= cos_sim %>% dplyr::select(myvar=var) 
  dt$xaxis=rownames(dt)
  dt=dt%>% dplyr::mutate(xaxis=str_replace(xaxis,"_"," "))
  
  ggplot2::ggplot(data=dt)+
    ggplot2::geom_bar(stat="identity",aes(x=reorder(xaxis,-myvar),y=myvar))+
    ggplot2::theme_bw()+
    ggplot2::ylab("Cosinus similarity")+
    ggplot2::xlab("")+
    ggplot2::theme(legend.position="bottom",axis.text.x=ggplot2::element_text(angle=90,hjust=1),axis.text=ggplot2::element_text(size=8))
  
}

#--------------------------------------------------------------------

#compute the average importance of each crisis by groups

tf_by_group=function(table_N_occurence,weight_method="brut_frequency",group=NULL){
  if(is.null(group)){
    dt_weights_years=tf_idf(table_N_occurence,weight_method) %>% ungroup() #dplyr::summarize_if(is.numeric,mean)
    dt_weights_years=dt_weights_years %>% tidyr::gather("Crisis",value="word_weight")
    dt_weights_years=dt_weights_years[-1,]
    dt_weights_years=dt_weights_years %>% dplyr::mutate(word_weight=as.numeric(word_weight))
  }else{  
    dt_weights_years=tf_idf(table_N_occurence,weight_method) %>% dplyr::ungroup() %>% dplyr::group_by(get(group)) %>% dplyr::summarize_if(is.numeric,mean)
    dt_weights_years=dt_weights_years %>% tidyr::gather("Crisis",value="word_weight",-'get(group)')
    colnames(dt_weights_years)[1]=group
    dt_weights_years
  }
}

#Look at average importance of each crisis by country and display the more important elements
country_radar_dt=function(table_N_occurence,isoc,top_n=50,weight_method="brut_frequency",group="ISO3_Code"){
  table=tf_by_group(table_N_occurence,weight_method = weight_method,group=group)
  table %>% dplyr::filter(ISO3_Code %in% isoc) %>% dplyr::arrange(-word_weight) %>% top_n(top_n)
} 

country_radar_fig=function(country_radar_dt){
  
  endo_exo_order=c("Wars","Natural_disaster","Commodity_crisis","World_outcomes",
                   "Expectations","Contagion","Balance_payment_crisis","Reduction_reserves",
                   "Currency_crisis","Banking_crisis","Financial_crisis","Severe_recession","Soft_recession",
                   "Inflation_crisis","Political_crisis","Fiscal_outcomes","Fiscal_consolidation","Sovereign_default")
  
  
  country_radar_dt=country_radar_dt %>%
    dplyr::arrange(match(Crisis,endo_exo_order)) %>% dplyr::filter(Crisis %in% endo_exo_order)
  
  Weights=country_radar_dt[,"word_weight"]$word_weight*10000
  
  Weights[Weights==0]=0.0000001
  Crisis=stringr::str_replace_all(country_radar_dt[,"Crisis"]$Crisis,"_"," ")
  
  i=6
  j=6 
  s=5
  alpha_endo=0.8
  p <- plotly::plot_ly(type = 'scatterpolar',
               mode="lines") %>%
    plotly::add_trace(
      r = rep(2.5,length(Crisis)), #max(Weights)
      theta = Crisis,
      line=list(color="lightgrey")) %>%
    # add_trace(
    #   r = c(rep(0,i),rep(max(Weights,na.rm=T),j),rep(0,length(Crisis)-j-i)),
    #   theta = Crisis,
    #   name="Exogenous",
    #   line=list(color=toRGB("orange",alpha_endo)),
    #   fill = 'toself',fillcolor = toRGB("orange",alpha_endo)) %>%
    # add_trace(
    #   r = c(rep(max(Weights,na.rm=T),i),rep(0,length(Crisis)-i)),
    #   theta = Crisis,
    #   name="Pure Exogenous",
    #   line=list(color=toRGB("green",alpha_endo)),
  #   fill = 'toself',fillcolor = toRGB("green",alpha_endo)) %>%
  # add_trace(
  #   r = c(rep(0,length(Crisis)-s),rep(max(Weights,na.rm=T),s)),
  #   theta = Crisis,
  #   name="Endogenous",
  #   line=list(color=toRGB("red",alpha_endo)),
  #   fill = 'toself',fillcolor = toRGB("red",alpha_endo)) %>%
  plotly::add_trace(
    r = Weights,
    theta = Crisis,
    name="Profile of crisis",
    line=list(color="#709Bff"),
    fill = 'toself',fillcolor = '#709Bff',alpha_endo) %>%
    plotly::layout(xaxis = 3, yaxis = median(Weights,na.rm=T)) %>%
    
    plotly::layout(
   xaxis = list(title = "", showgrid = T, zeroline = F, showticklabels = T,
                       domain = c(0, 3)
   ),
    yaxis = list(title = country_radar_dt$ISO3_Code %>% unique(), showgrid = F, zeroline = F, showticklabels = F#,
                 #domain = c(0, 0.92)
    ),
    font = list(family = "serif", size = 20),
    legend = list(bgcolor = "transparent",title="test"),
    showlegend = FALSE
    )
  p
  
}

#-------------------------------------------------------------------------
#generate a search engine for files of crisis

find_associated_keyword=function(keyword){
  #find the keywords associate to the name of the category provide
  #usefull to know what are the words behind each category
  detect=names(key_words_crisis())[stringr::str_detect(tolower(names(key_words_crisis())),tolower(keyword))]
  key_words_crisis()[detect]
}

find_pertinance=function(table.N.occurrence,keyword){
  #rand the more pertinent category given the tf-idf
  words=find_associated_keyword(keyword)
  table=tf_idf(table.N.occurrence)
  table=table %>% tidyr::gather(Crisis,value="weight",-c(ISO3_Code,Period,year,type_prog,file))
  results=table %>% dplyr::filter(stringr::str_detect(tolower(Crisis),tolower(keyword)) & weight>0) %>% dplyr::arrange(-weight)
  return(list(keywords=words,results=results))
}

#---------------------------------------------------------------------------

search_contagion=function(file){
  tictoc::tic()
  ctry_table=key_words_countries()
  
  ctries=c(ctry_table$ctries,ctry_table$adjectival,ctry_table$Currency,ctry_table$Symbol,key_words_crisis()$Crisis_contagion)
  
  text= clean_text(file)
  text_df <- dplyr::tibble(text = text) %>%
    tidytext::unnest_tokens(word, text) %>%
    dplyr::anti_join(stop_words,by=c("word"))
  tictoc::toc()
  text_df %>% dplyr::filter(word %in% tolower(ctries)) %>% count(word) %>% dplyr::arrange(word) %>% dplyr::filter(n>1)
}



