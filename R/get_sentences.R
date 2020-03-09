#'From a corpus (collection documents), returns the sentences where keyword detected
#'and respective keyword
#'
#'Function that simplifies checking validity indexes and potential problems in keywords
#'
#' @param corpus: list with collection documents
#' @param keyword_list: character vector with categories from key_words_crisis function
#'
#' @return Tibble with two columns: sentences containing keyword and respective keyword. 
#'
#' @author Manuel Bétin, Umberto Collodel
#'
#' @examples
#'
#'
#' @export


get_sentences <- function(corpus, keyword_list){
  corpus %>% 
  map(~ tibble(document = .x)) %>% 
  map(~ .x %>% tidytext::unnest_tokens(sentence, document, token = "sentences")) %>% 
  map(~ .x %>% filter(str_detect(sentence, paste(key_words_crisis()[[keyword_list]], collapse = "|")))) %>% 
  discard(~ nrow(.x) == 0) %>% 
  map(~ .x %>% mutate(keyword_detected = str_extract(sentence, paste(key_words_crisis()[[keyword_list]], collapse = "|"))))
}