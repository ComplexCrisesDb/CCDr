key_words_categories=function(){
  
  cat_1=c('Deregulation','Reform_agenda','Trade_reforms','Financial_reforms','Labor_market_reforms','Tax_reforms','Banking_reforms',
          'Fiscal_consolidation','Success_of_reforms')
  
  cat_1=unlist(key_words_crisis()[cat_1],recursive = T)
  names(cat_1)=str_remove_all(names(cat_1),"\\d")
  
  cat_2=c('Peformance_criterion','Program_extension','Official_support','Technical_assistance','Precautionary_programs')
  
  cat_2=unlist(key_words_crisis()[cat_2],recursive = T)
  names(cat_2)=str_remove_all(names(cat_2),"\\d")
  
  cat_3=c('Banking_crisis','Financial_crisis','Inflation_crisis','trade_crisis','Crisis_contagion','World_outcomes',
          'Contagion','Expectations','Balance_payment_crisis','Reduction_reserves','Currency_crisis','Severe_recession',
          'Soft_recession','Expansion')
  
  cat_3=unlist(key_words_crisis()[cat_3],recursive = F)
  names(cat_3)=str_remove_all(names(cat_3),"\\d")
  
  cat_4=c('Wars','Natural_disaster','Commodity_crisis','Political_crisis','Social_crisis')
  
  cat_4=unlist(key_words_crisis()[cat_3],recursive = F)
  names(cat_3)=str_remove_all(names(cat_3),"\\d")
  
  
  cat_5=c('Fiscal_outcomes','Sovereign_default')
  
  cat_5=unlist(key_words_crisis()[cat_5],recursive = F)
  names(cat_5)=str_remove_all(names(cat_5),"\\d")
  
  cat_6=c('Concessional_lending','Short_term_debt','floating_rate_debt','foreign_debt','Track_record')
  
  cat_6=unlist(key_words_crisis()[cat_6],recursive = F)
  names(cat_6)=str_remove_all(names(cat_6),"\\d")
  
  return(list("adjustment_program"=cat_1,
              "characteristics_program"=cat_2,
              "economic_shock"=cat_3,
              "non_economic_shock"=cat_4,
              "debt_outcomes"=cat_5,
              "debt_structure"=cat_6)
  )
}
