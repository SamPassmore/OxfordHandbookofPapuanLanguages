## This script contains helper functions for calculating structual vectors

## Term comparison function
get_dist = function(x, y, max_char, method = "osa"){
  if(method == "binary"){
    ifelse(x == y, 1, 0)
  } else {
    stringdist::stringdist(x, y, method = method) / max_char 
  }
}

get_vector = function(x, method){
  max_char = sapply(x, nchar) %>% max(.)
  ## compare all values to each other, then take the lower trianble of the matrix
  ## i.e. we don't need every comparison twice.
  m = outer(x, x, get_dist, method = method, max_char = max_char) %>% 
    .[lower.tri(.)] %>% 
    c(.)
  
  new_colnames = outer(names(x), names(x), paste0)
  names(m) = new_colnames[lower.tri(new_colnames)]
  m
}
