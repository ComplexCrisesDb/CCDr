# function to plot the idf for each category and group by typology and
# nature
idf_barplot = function(idf, vars_type = c("economic_shock", "non_economic_shock", 
    "debt_outcomes", "debt_structure", "characteristics_program", "adustment_program"), 
    vars_nature = NULL, idf_trans = F) {
    
    idf = LoI_idf
    res = list()
    
    if (idf_trans == T) {
        idf$idf = 1/exp(idf$idf)
    }
    idf = idf %>% rename(variable = Crisis)
    
    idf = idf %>% left_join(typology_categories(), by = c("variable"))
    
    res[["table"]] = idf
    
    if (!is.null(vars_nature)) {
        idf = idf %>% filter(type %in% vars_type & nature_shock %in% vars_nature)
    } else {
        idf = idf %>% filter(type %in% vars_type)
    }
    
    res[["fig"]] = ggplot(idf) + geom_bar(stat = "identity", aes(x = reorder(variable, 
        idf), y = idf, fill = type)) + scale_fill_manual(values = c("darkred", 
        "blue", "darkgreen", "lightgrey", "orange", "brown")) + theme_bw() + 
        lims(y = c(0, 1)) + labs(title = "Probability of the events", x = NULL, 
        y = NULL) + theme(legend.title = element_blank(), legend.position = "bottom", 
        axis.text.x = element_text(angle = 90, hjust = 1), axis.text = element_text(size = 8))
    
    return(res)
    
}
