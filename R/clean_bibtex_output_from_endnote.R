#' endnote_to_bibtex
#' 
#' @param filename the exported txt filename carried from Endnote via BibTeX Export utility that is dirty and must be cleaned
#' @param outname the clean(er) text file that is carried out by this process
#' 

clean_bibtex_output_from_endnote <- function(filename, outname = paste0("cleaned-", filename)){
raw <- readLines(filename)

startlines <- grep("\\@[a-z]+\\{\\s*$", raw)
raw[startlines] <- ifelse(grepl("^\\@", raw[startlines]), 
                          raw[startlines], 
                          gsub("^.*\\@", "@", raw[startlines]))
authorlines <- grep("^\\s*(author)\\s*=", raw, ignore.case = TRUE)
yearlines <- grep("^\\s*(year)\\s*=", raw, ignore.case = TRUE)
endlines <- grep("^}$", raw)

clean <- raw

key_chain <- character(length(startlines))
key_chain_prefixes <- key_chain

count <- 0

for (i in startlines){
  count <- count + 1
  next_endline <- min(endlines[endlines > i])
  next_authorline <- min(authorlines[authorlines > i & authorlines < next_endline])
  next_yearline   <- min(yearlines[yearlines > i & yearlines < next_endline])
  
  if (!is.na(next_authorline)){
    group_author <- tolower(raw[next_authorline])
    group_author <- gsub("^.*\\{([a-z]*).*\\}.*$", "\\1", group_author)
  } else {
    group_author <- "noauthor"
  }
  
  if (!is.na(next_yearline)){
    group_year <- gsub("^.*\\{([12][0-9]{1,3})\\}.*$", "\\1", raw[next_yearline])
  } else {
    group_year <- "nd"
  }
  
  plausible_key <- paste0(group_author, group_year)
  
  if(plausible_key %in% key_chain){
    # This only occurs when the first conflict occurs. It should be
    # chomsky1981a, chomsky1981b. Right now, it's chomsky1981, chomsky1981.
    # This is the only possible situation.
    key_siblings <- sum(key_chain == gsub("[0-9][a-z]$", "", plausible_key))
    plausible_key <- paste0(plausible_key, letters[key_siblings + 1])
    key_chain[count] <- paste0(plausible_key, "b")
    key_chain[count - 1] <- paste0(key_chain[count - 1], "a")
    key_chain_prefixes <- gsub("[a-z]$", "", key_chain)
  } else {
    # In this case, we probably have different author years, but we could also have
    # Chomsky1981a, Chomsky1981b, Chomsky1981.  We need to test before proceeding.
    
    # How many keys on the chain have the same prefix?
    key_chain_prefixes <- gsub("[a-z]$", "", key_chain)
    key_siblings <- sum(key_chain_prefixes == gsub("([0-9])[a-z]$", "\\1", plausible_key))
    if (key_siblings == 0){
      key_chain[count] <- plausible_key
    } else {
      plausible_key <- paste0(plausible_key, letters[key_siblings + 1])
    }
  }
  
  clean[i] <- gsub("(\\@[a-z]+\\{)\\s*$", "\\1", raw[i])
  clean[i] <- paste0(clean[i], plausible_key, ",")
}
writeLines(clean, con = outname)
}