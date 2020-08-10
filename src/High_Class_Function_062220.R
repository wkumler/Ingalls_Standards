

#functions for classifying a large number of compounds using the classyFire website

#Joshua Sacks
#Marine Metabolomics Research Center
#School of Oceanography, University of Washington
#06/22/20

# The function is called high_class() where the input is a tibble of 1 column where each observation 
# is the InChI Key of a compound you are interested in classifying (the InChI Keys for a compound
# can be found on sites like pubchem). The output is a tibble of 4 columns (key,Level,Classification,CHEMONT)
# where each observation corresponds to a compound (identified by its InChI key), a level within the ClassyFire
# classification system (ex. subclass), the classification of the compound at that level (ex. Fatty Acid Esters)
# and the ChemOnt-ID # for that classification. The other functions present are adapted from the ClassyFire_R package
# and have been modified and included in the high_class function.

#Required packages

library(clisymbols)

# parse_json_output -------------------------------------------------------

parse_json_output <- function(json_res)
{
  list_output <-
    list(
      kingdom = json_res[['kingdom']],
      superclass = json_res[['superclass']],
      class = json_res[['class']],
      subclass = json_res[['subclass']],
      intermediate_nodes = json_res[['intermediate_nodes']],
      direct_parent = json_res[['direct_parent']]
    )
  
  if (length(list_output$intermediate_nodes) == 0) {
    list_output$intermediate_nodes <- NULL
  }
  
  list_output <- list_output[!sapply(list_output, is.null)]
  
  if (length(list_output) > 0) {
    class_tibble <- purrr::map(1:length(list_output),  ~ {
      l <- list_output[[.]]
      tibble::tibble(
        Level = names(list_output)[.],
        Classification = l$name,
        CHEMONT = l$chemont_id
      )
    }) %>%
      dplyr::bind_rows() %>%
      dplyr::filter(!duplicated(Classification))
    
    nIntermediate <- class_tibble %>%
      dplyr::filter(Level == 'intermediate_nodes') %>%
      nrow()
    
    class_tibble$Level[class_tibble$Level == 'intermediate_nodes'] <-
      purrr::map_chr(5:(5 + (nIntermediate - 1)),  ~ {
        stringr::str_c('level ', .)
      })
    class_tibble$Level[class_tibble$Level == 'direct_parent'] <-
      stringr::str_c('level ', 5 + nIntermediate)
    
  } else {
    class_tibble <- tibble()
  }
  return(class_tibble)
}


# get_classification_2 ----------------------------------------------------

get_classification_2 <- function(inchi_key)
  
{
  entity_url <- 'http://classyfire.wishartlab.com/entities/'
  
  entity_query <- paste0(entity_url, inchi_key, '.json')
  
  response <- httr::GET(entity_query)
  
  if (response$status_code == 429) {
    stop('Request rate limit exceeded!')
  }
  
  if (response$status_code == 404) {
    message(crayon::red(clisymbols::symbol$cross, inchi_key))
  }
  
  if (response$status_code == 200) {
    text_content <- httr::content(response, 'text')
    
    if (text_content == '{}') {
      message(crayon::red(clisymbols::symbol$cross, inchi_key))
      return(invisible(NULL))
    } else{
      message(crayon::green(clisymbols::symbol$tick, inchi_key))
    }
    
    
    json_res <- jsonlite::fromJSON(text_content)
    
    classification <- parse_json_output(json_res) 
    
    labeled_classification <- classification %>%
      mutate(key = inchi_key) %>%
      select(key, everything())
    
    bs <- labeled_classification
    
    return(bs)
  }
  
}


# high_class --------------------------------------------------------------

high_class <- function(input)
{
  plop <- vector("character", nrow(input))
  for (p in seq_along(input)) {
    plop <- print(input[[p]])
  }
  input <- plop
  output <- vector("list", length(input))
  for (i in seq_along(input)) {
    output[[i]] <- get_classification_2(input[i])
    Sys.sleep(15)
  }
  classy_dat <- bind_rows(output)
  print(classy_dat)
}



