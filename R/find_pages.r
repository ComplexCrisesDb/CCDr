find_pages = function(file, targetword) {
    #' Find the page where words a located
    #' Provide files either pdf of html and return the paragraphs matching the
    #' targetted word 
    #' @param file a character string correspond to the text
    #' to analysis
    #' @param targetword a vector of characters corresponding to word to
    #' search and count for in the text
    #' @author Manuel Betin
    #' @return a list of strings of characters where words have been found.
    #' @export
    #' 
  if (!is.null(file)) {
        page_locations = list()
        target_pages = list()
        i = 0
        Tot.occurence = 0
        file = clean_text(file)
        n.chars = sum(nchar(file))  #total number of characters in the file after cleaning
        page_location = sapply(1:length(file), function(x) {
            page = file[[x]]
            page = tibble(page) %>% tidytext::unnest_tokens(word, page, token = "sentences") %>%
              dplyr::mutate(word = tolower(word))
            n.occurence = (page %>% dplyr::filter(grepl(paste(tolower(targetword), 
                collapse = "|"), word)) %>% dplyr::summarize(count = n()))$count
            condition = n.occurence > 0
            if (any(condition)) {
                i <<- i + 1
                page_locations[[i]] <<- paste0("Found in page ", x, " :", n.occurence, 
                  " times")
                target_pages[[i]] <<- file[[x]]
                Tot.occurence <<- Tot.occurence + n.occurence
                return("yes")
            } else {
                return("no")
            }
            
        })
        
        return(list(target = targetword, N.chars = n.chars, N.Occurence = page_locations, 
            Tot.occurence = Tot.occurence, pages = target_pages))
    } else return(list(target = targetword, N.chars = 0, N.Occurence = 0, Tot.occurence = 0, 
        pages = 0, error_message = "File is null no mining provided"))
}
