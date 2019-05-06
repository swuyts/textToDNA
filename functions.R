# Inspired by https://github.com/allanino/DNA/blob/master/dna/

# Based on the technology described by Goldman et al (2013), Nature
# The terminology (S0, S1, S4, ...) is taken from the supplementary material (V2)
# https://www.ebi.ac.uk/sites/ebi.ac.uk/files/groups/goldman/file2features_2.0.pdf
# Important note: I will use a simplified version of the procedure.
# This means that the resulting DNA sequences can not be used for physical printing

# Main functions

## Convert S0 to S1
S0_to_S1 <- function(S0) {
  
  # Convert text to bytes and bytes to 256 number rep
  bytes <- bytes(as.character(S0)) %>% 
    strsplit(., " ") %>% 
    .[[1]] %>% # All steps above are parsing steps
    strtoi(base = 16) # Convert to the numerical representation of bytes
  
  # Convert bytes to base3 using the Huffman Dictionary
  S1 <- tibble(bytes) %>%
    left_join(huff) %>% # Find the corresponding base3 representation in the huffman dictionary
    pull(base3) %>% 
    paste(collapse = '') # Parse
  
  # Return output
  return(S1)
} 


## Convert S1 to S5 without the intermediate steps
S1_to_S5 <- function(S1) {
  
  S5 <- DNAcode %>% 
    filter(prevBase == "A") %>% 
    .[,str_sub(S1, 1, 1) %>% as.character()] %>%
    pull()

  for (char in 2:nchar(S1)){
    newBase <- DNAcode %>% 
      filter(prevBase == str_sub(S5, char - 1, char - 1)) %>% 
      .[,str_sub(S1, char, char) %>% as.character()] %>%
      pull()
    
    S5 <- str_c(S5, newBase, sep = "")
  }
  return(S5)
}

## Covert S5 to S1
S5_to_S1 <- function(S5) {
  
  S1 <- DNAcode %>% 
    filter(prevBase == "A") %>%
    .[ , which(. == str_sub(S5, 1, 1))] %>%
    names()
  
  for (char in 2:nchar(S5)){
    newTrit <- DNAcode %>%
      filter(prevBase == str_sub(S5, char - 1, char - 1)) %>%
      .[ , which(. == str_sub(S5, char, char))] %>%
      names()
    
    S1 <- str_c(S1, newTrit, sep = "")
  }
  return(S1)
}


## Convert S1 to S0
S1_to_S0 <- function(S1) {
  
  S0 <- ""
  i <- 1
  
  while (i < nchar(S1)) {
    if (str_sub(S1, i, i+4) %in% huff$base3){
      byte <- huff %>%
        filter(base3 == str_sub(S1, i, i+4)) %>%
        pull(bytes)
      
      S0 <- rawToChar(as.raw(byte)) %>%
        str_c(S0, ., sep = '')
      
      i <- i + 5
    } else {
      byte <- huff %>%
        filter(base3 == str_sub(S1, i, i+5)) %>%
        pull(bytes)
      
      S0 <- rawToChar(as.raw(byte)) %>%
        str_c(S0, ., sep = '')
      
      i <- i + 6
    }
  }
  
  return(S0)
} 

