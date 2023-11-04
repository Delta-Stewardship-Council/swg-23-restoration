# R version 4.3.1 (2023-06-16)
# Author: KT
# Purpose: To help process data downloaded from CNRA Bond Accountability sites.

# Load libraries
lapply(
  X = c(
    "rvest",
    "polite"
  ),
  FUN = function(x) {
    if(!(x %in% .packages())){
      print(paste0("Loading {", x, "}"))
      library(x, character.only = T)
    }
  }
) |>
  invisible()

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
  
  if(length(attrs) > 1){
    # This should not run, so keep browser() in here.
    browser()
  }
  
  attrs <- attrs %>%
    `[[`(1)
  
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

#' Take a nested html and flatten it. This is an issue when we run through fn_strong_text and find a div on the same level as a strong.
fn_flatten_strong <- function(html){
  # get names
  nm <- html %>%
    html_name()
  
  # find div in names
  div_locs <- which(nm == "div")
  
  # flatten the div tag only
  divs_ <- map(div_locs, function(div_loc){
    ch <- html[div_loc] %>%
      html_children()
    
    chnm <- ch %>%
      html_name()
    
    if("div" %in% chnm){
      # do this recursively
      ch <- fn_flatten_strong(ch)
    }
    
    # Return
    ch
  })
  
  # Now we're going to insert the flattened divs
  out <- list()
  st <- 1
  for(i in 1:length(div_locs)){
    loc <- div_locs[i]
    end <- loc - 1
    if(st < loc){
      # add stuff before div
      out <- append(out, html[st:end])
    }
    # now add flattened div
    out <- append(out, unlist(divs_[i], recursive = F))
    # Set the new start location
    st <- st + loc
    # If this is the last insert, add rest of html
    if(i == length(div_locs)){
      out <- append(out, html[st:length(html)])
    }
  }
  
  # RETURN
  # convert node to character string
  map(out, as.character) %>% 
    # unlist and combine into vector of one character element.
    unlist() %>% 
    paste0(collapse = "") %>% 
    # convert back into html
    read_html() %>%
    # get children as a single node
    html_children() %>%
    html_children()
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
    
    if("div" %in% nm){
      # Sometimes there are divs at the same level
      # as strong tags.
      ch <- fn_flatten_strong(ch)
      nm <- ch %>%
        html_name()
    }
    
    l_ <- fn_wordseq(nm, "strong")
    
    tb <- imap_dfr(l_, function(vec, idx){
      
      # get names of elements
      vec_nm <- names(vec)
      
      # remove tags we don't want/need
      # br
      vec <- vec[vec_nm != "br"]
      
      l_tibbles <- map(seq_along(vec), function(j){
        fn_1tag_extract(ch[vec[j]])
        })
      
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

#' Extracts the most important information from the output of fn_strong_text. Note, this may need to be amended based on needed information.
fn_strong_format <- function(
    tb
){
  item_ids <- tb$item %>% 
    unique()
  
  map_dfr(item_ids, function(item_id){
    # get subset of strong-tibble
    item <- tb %>%
      filter(item == item_id)
    
    # extract Strong text value, removing the colon
    strong <- item$text[item$tag == "strong"] %>%
      str_remove(., "\\:$")
    
    # Remove strong row, leaving 1 to many rows.
    item <- item %>%
      filter(tag != "strong")
    
    
    if(nrow(item) == 1){
      # If there's just one row, we don't need to do much.
      # RETURN
      tibble(
        key = strong,
        value = item$text
      )
    } else if(all(item$tag == "a")){
      # If the tag is 'a' (ie. a link), we want the href only.
      href <- item %>%
        filter(attr == "href") %>%
        pull(attr_value)
      
      # RETURN
      tibble(
        key = strong,
        value = href
      )
      
    }else {
      # Leave this in here in case there's an unexpected tag.
      browser()
    }
    
  })
}

#' Formats data regarding Water Action Plan Target
fn_wapt <- function(wapt){
  if(length(wapt) == 0){
    return(
      tibble(
        wapt = NA,
        wapt_description_url = NA
      )
    )
  }
  
  tb <- map_dfr(wapt, function(row){
    as_tibble_row(row)
  })
  
  # clean up clutter
  clean <- tb %>%
    filter(text != "") %>%
    filter(attr != "target") %>%
    mutate(group = case_when(
      tag == "td" ~ row_number(),
      .default = NA
    )) %>%
    mutate(attr_value = case_when(
      tag == "td" ~ NA,
      .default = attr_value
    )) %>%
    fill(group, .direction = "down") %>%
    select(tag, text, attr_value, group)
  
  map_dfr(group_split(clean), function(row){
    plan <- row %>%
      filter(tag == "td") %>%
      pull(text)
    
    description_url <- row %>%
      filter(tag == "a") %>%
      pull(attr_value)
    
    tibble(
      wapt = plan,
      wapt_description_url = description_url
    )
  })
}



#' #' Extract information from program or project page of CNRA.
#' fn_html_main <- function(
#'     html, 
#'     root_element = ".container_style_a"
#' ){
#'   root <- html %>%
#'     html_elements(root_element)
#'   
#'   root_info <- fn_children_info(root)
#'   
#'   if(T %in% str_detect(root_info$child_id, 
#'                 regex("program", ignore_case = T))){
#'     # PROGRAM (not project)
#'     # Where does the content live? Which ID?
#'     element_ <- root_info$child_id[
#'       str_which(root_info$child_id,
#'               regex("content$", ignore_case = T))
#'     ] %>%
#'       paste0("#", .)
#'     
#'     content <- root %>%
#'       html_elements(element_)
#'     
#'     
#'   } else {
#'     # PROJECT (not program)
#'   }
#' }