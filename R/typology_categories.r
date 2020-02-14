
typology_categories = function() {
    
    # draw typology according to the type of variable
    
    adjustment_program = c("Deregulation", "Reform_agenda", "Trade_reforms", 
        "Financial_reforms", "Labor_market_reforms", "Tax_reforms", "Banking_reforms", 
        "Fiscal_consolidation", "Success_of_reforms")
    
    characteristics_program = c("Performance_criterion", "Program_extension", 
        "Official_support", "Technical_assistance", "Precautionary_programs")
    
    economic_shock = c("Banking_crisis", "Financial_crisis", "Inflation_crisis", 
        "Trade_crisis", "World_outcomes", "Contagion", "Expectations", 
        "Balance_payment_crisis", "Reduction_reserves", "Currency_crisis","Currency_crisis_severe", "Severe_recession", 
        "Soft_recession", "Expansion")
    
    non_economic_shock = c("Wars", "Natural_disaster", "Commodity_crisis", "Political_crisis", 
        "Social_crisis")
    
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
    
    Fiscal_policy = c("Banking_crisis", "Financial_crisis","Trade_crisis", 
        "Natural_disaster", "Commodity_crisis", "Wars","Severe_recession", "Soft_recession", 
        "Expansion", "Fiscal_outcomes", "Sovereign_default","Political_crisis", "Social_crisis",
        "Disbursement","Fiscal_consolidation","Short_term_debt","Floating_rate_debt",
        "Concessional_lending","Foreign_debt")
    
    Monetary_policy = c("Inflation_crisis", "Balance_payment_crisis", 
        "Reduction_reserves", "Currency_crisis","Currency_crisis_severe","Global_depreciation",
        "Floating_exchange_rate","Fixed_exchange_rate","Losening_monetary_policy","Tightening_monetary_policy")
    
    Structural_policy = c("Trade_crisis","Performance_criterion","Program_extension",
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
    
    Other_Financial_shock = c("Banking_crisis", "Financial_crisis", "Balance_payment_crisis", 
                              "Reduction_reserves", "Currency_crisis","Currency_crisis_severe","World_outcomes", "Contagion",
                              "Expectations")
    
    Fiscal_financial_shock = c( "Fiscal_outcomes", "Sovereign_default","Short_term_debt","Floating_rate_debt",
                                "Concessional_lending","Foreign_debt")
    
    Real_shock = c("Severe_recession", "Soft_recession", "Expansion","Trade_crisis","Inflation_crisis",
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
    
    Expenditure_shock = c("Banking_crisis", "Financial_crisis","World_outcomes", "Contagion",
                              "Expectations")
    
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
                          Spread_shock=Spread_shock,risk_free=risk_free))
    
    channel = data.frame(channel)
    channel$Type = rownames(channel)
    names(channel) = c("variable", "channel")
    channel = channel %>% mutate(channel = str_remove_all(channel, "\\d"))
    
    
    
    res = type %>% left_join(nature, by = c("variable"))
    res = res %>% left_join(domain, by = c("variable"))
    res = res %>% left_join(channel, by = c("variable"))
    
    res
}