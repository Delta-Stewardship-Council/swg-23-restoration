library(polite)
library(rvest)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Takes a character vector, and a key element in this vector, and returns a list split up by the key (eg strong). This aids in the process of dividing objects created by html_children.
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

#' Takes html tag, and returns a tibble.
fn_1tag_extract <- function(item){
  nm <- item %>%
    html_name()
  
  text <- item %>%
    html_text2()
  
  attrs <- item %>%
    html_attrs() 
  
  if(length(attrs) >= 1){
    
    attrs <- attrs %>%
      `[[`(1)
    
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
#' Extracts key-value data stored in the form of <strong> + text.
fn_strong_text <- function(
    html, 
    starting_element,
    recursive = T
){
  
  # Get children
  ch <- html %>%
    html_elements(starting_element) %>%
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
      
      l_tibbles <- map(ch[vec], fn_1tag_extract)
      
      # return
      l_tibbles %>%
        bind_rows() %>%
        mutate(item = idx) %>%
        select(item, everything())
    })
    
  } else if(recursive == T) {
    # # TODO rm
    # tmp <<- list(ch = ch, nm = nm)
    
    out <- map(nm, function(nm2){
      fn_strong_text(
        html = html %>%
          html_elements(starting_element), 
        starting_element = nm2)
    })
    
    tb <- out[!is.null(out)] %>%
      bind_rows()
    
  } else {
    return(NULL)
  }
  
  # Return
  tb
}

#' Obtain information about the children (and the parent) of an html element.
fn_children_info <- function(
    html
){
  if(length(html) > 1){
    map(seq_along(html), ~fn_children_info(html[.x])) %>%
      bind_rows %>%
      return()
    
  } else if(length(html) == 0) {
    return(NULL)
    
  } else {
    ch <- html %>%
      html_children()
    
    nm <- ch %>%
      html_name()
    
    ids <- ch %>%
      html_attr("id")
    
    cls <- ch %>%
      html_attr("class")
    
    tb <- tibble(
      parent_element = html %>% html_name(),
      parent_id = html %>% html_attr("id"),
      parent_class = html %>% html_attr("class"),
      child_element = nm,
      child_id = ids,
      child_class = cls
    )
    
    md5 <- digest(tb)
    
    tb <- tb %>%
      mutate(md5 = md5) %>%
      select(md5, everything())
    
    return(
      tb
    )
  }
  
}

#' Extract information from program or project page of CNRA.
fn_html_main <- function(
    html, 
    root_element = ".container_style_a"
){
  root <- html %>%
    html_elements(root_element)
  
  root_info <- fn_children_info(root)
  
  if(T %in% str_detect(root_info$child_id, 
                regex("program", ignore_case = T))){
    # PROGRAM (not project)
    # Where does the content live? Which ID?
    element_ <- root_info$child_id[
      str_which(root_info$child_id,
              regex("content$", ignore_case = T))
    ] %>%
      paste0("#", .)
    
    content <- root %>%
      html_elements(element_)
    
    
  } else {
    # PROJECT (not program)
  }
}