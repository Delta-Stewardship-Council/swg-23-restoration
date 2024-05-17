# demo code that should work on windows machines.
library(stringr)
path1 <- "your\\path/here"

path1 <- str_split_1(path1, "\\/|\\\\{1,2}")

path1 <- file.path(
  paste(
    path1, 
    collapse = .Platform$file.sep
  )
)

# run readtext(path1)

