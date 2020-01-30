
# function to run the tf by providing the list of texts and the type of
# lexicon
run_tf = function(corpus_path = "IMF_letofIntent_1960_2014_clean.RData", type_lexicon = "words", 
    keyword_list = c("Commodity_crisis", "Balance_payment_crisis", "Inflation_crisis"), 
    export_path = NULL, parrallel = T) {
    
    # function that compute the tf matrix for a corpus given in a list format,
    # the type of lexicon to use and the sublist of keywords associated The
    # output is a matrix of tf with a row per document and a column for each
    # element of the keyword list
    
    cat(crayon::bgBlue(paste0("Loading corpus from ", corpus_path)))
    corpus = rio::import(corpus_path)
    if (type_lexicon == "words") {
        if (is.null(keyword_list)) {
            keyword_list = c("Commodity_crisis", "Balance_payment_crisis", "Inflation_crisis", 
                "World_outcomes", "Floating_Exchange rate", "Fixed_Exchange rate", 
                "Wars", "Soft_Recession", "Fiscal_Outcomes", "Financial_crisis", 
                "Banking_crisis", "Currency_crisis", "Severe_recession", "Crisis_contagion", 
                "Sovereign_default", "Expectations", "Precautionary_programs", 
                "Natural_desaster", "Loss_Market_access", "Political_crisis", 
                "Constraining", "Contagion")
            print(keyword_list)
        }
        keyword_list = key_words_crisis()[keyword_list]
    } else if (type_lexicon == "category") {
        if (is.null(keyword_list)) {
            keyword_list = c("Exogenous", "Manifestations", "Instruments")
        }
        keyword_list = key_words_categories()[keyword_list]
    } else {
        cat(crayon::red("Please provide a valid type_lexicon, either 'words' or 'category'"))
        return(NULL)
    }
    
    tictoc::tic()
    dt = tf_vector(corpus, keyword_list, parrallel = parrallel)
    tictoc::toc()
    destination = paste0(export_path, "/tf_crisis_", type_lexicon, ".RData")
    print(paste0("export table in ", corpus_path))
    if (!is.null(export_path)) {
        rio::export(dt, destination)
    }
    return(dt)
}
