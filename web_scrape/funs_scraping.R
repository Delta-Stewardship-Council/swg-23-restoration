library(polite)
library(rvest)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Takes a character vector, and a key element in this vector, and returns a list split up by the key. This aids in the process of dividing objects in created by html_children.
#' @param chr_vec character vector
#' @param chr key single term
#' @return list
fn_wordseq <- function(chr_vec, chr){
  locs <- c(which(chr_vec == chr),
            length(chr_vec) + 1
  )
  
  map(1:(length(locs)-1), function(i){
    vec <- locs[i]:(locs[i+1] - 1)
    names(vec) <- chr_vec[vec]
    vec
  })
}

fn_1tag_extract <- function(item){
  nm <- item %>%
    html_name()
  
  text <- item %>%
    html_text2()
  
  attrs <- item %>%
    html_attrs()
  
  if(length(attrs) >= 1){
    tibble(
      tag = nm,
      text = text,
      attr = names(attrs),
      attr_value = attrs
    )
  } else {
    tibble(
      tag = nm,
      text = text,
      attr = as.character(NA),
      attr_value = as.character(NA)
    )
  }
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fn_strong_text <- function(
    html, 
    starting_element,
    recursive = T
){
  print(starting_element)
  # Get children
  ch <- html %>%
    html_element(starting_element) %>%
    html_children()
  
  if(length(ch) == 0){
    return(NULL)
  }
  
  # Get names
  nm <- ch %>%
    html_name()
  
  if("strong" %in% nm){
    l_ <- fn_wordseq(nm, "strong")
    
    tb <- imap_dfr(l_, function(vec, idx){
      # get names of elements
      vec_nm <- names(vec)
      
      # remove tags we don't want/need
      # br
      vec <- vec[vec_nm != "br"]
      
      l_tibbles <- map(vec, fn_1tag_extract)
      
      # return
      l_tibbles %>%
        bind_rows() %>%
        mutate(item = idx) %>%
        select(item, everything())
    })
    
  } else if(recursive == T) {
    tmp <<- list(ch = ch, nm = nm)
    
    # map(nm, function(item){
    #   fn_strong_text(
    #     html = html %>%
    #       html_element
    #   )
    # })
    out <- map2(ch, nm, function(child, name){
      browser()
      fn_strong_text(
        html = child, 
        starting_element = name)
    })
    
    tb <- out[!is.null(out)] %>%
      bind_rows()
    
  } else {
    return(NULL)
  }
  
  # Return
  tb
}


