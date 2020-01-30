
typology_categories = function() {
    
    # draw typology according to the type of variable
    
    adjustment_program = c("Deregulation", "Reform_agenda", "Trade_reforms", 
        "Financial_reforms", "Labor_market_reforms", "Tax_reforms", "Banking_reforms", 
        "Fiscal_consolidation", "Success_of_reforms")
    
    characteristics_program = c("Peformance_criterion", "Program_extension", 
        "Official_support", "Technical_assistance", "Precautionary_programs")
    
    economic_shock = c("Banking_crisis", "Financial_crisis", "Inflation_crisis", 
        "trade_crisis", "Crisis_contagion", "World_outcomes", "Contagion", "Expectations", 
        "Balance_payment_crisis", "Reduction_reserves", "Currency_crisis", "Severe_recession", 
        "Soft_recession", "Expansion")
    
    non_economic_shock = c("Wars", "Natural_disaster", "Commodity_crisis", "Political_crisis", 
        "Social_crisis")
    
    debt_outcomes = c("Fiscal_outcomes", "Sovereign_default")
    
    debt_structure = c("Concessional_lending", "Short_term_debt", "floating_rate_debt", 
        "foreign_debt", "Track_record")
    
    type = unlist(list(adjustment_program = adjustment_program, characteristics_program = characteristics_program, 
        economic_shock = economic_shock, non_economic_shock = non_economic_shock, 
        debt_outcomes = debt_outcomes, debt_structure = debt_structure))
    
    type = data.frame(type)
    type$Type = rownames(type)
    names(type) = c("variable", "type")
    type = type %>% mutate(type = str_remove_all(type, "\\d"))
    
    # typology in terms of endogeneity/exogeneity of shocks
    
    exogeneous = c("trade_crisis", "World_outcomes", "Contagion", "Expectations", 
        "Natural_disaster", "Commodity_crisis", "Wars")
    
    endogeneous = c("Banking_crisis", "Financial_crisis", "Balance_payment_crisis", 
        "Reduction_reserves", "Currency_crisis", "Severe_recession", "Soft_recession", 
        "Expansion", "Fiscal_outcomes", "Sovereign_default")
    
    pure_endogeneous = c("Inflation_crisis", "Political_crisis", "Social_crisis", 
        "Deregulation", "Reform_agenda", "Trade_reforms", "Financial_reforms", 
        "Labor_market_reforms", "Tax_reforms", "Banking_reforms", "Fiscal_consolidation", 
        "Success_of_reforms")
    
    nature = unlist(list(exogeneous = exogeneous, endogeneous = endogeneous, 
        pure_endogeneous = pure_endogeneous))
    nature = data.frame(nature)
    nature$Type = rownames(nature)
    names(nature) = c("variable", "nature_shock")
    nature = nature %>% mutate(nature_shock = str_remove_all(nature_shock, "\\d"))
    
    res = type %>% left_join(nature, by = c("variable"))
    
    res
}
