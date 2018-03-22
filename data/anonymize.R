# anonymize MGH data
rm(list=ls())
load("data-clean/mgh.RData")
str(hc)
str(voting)

### FUNCTIONS ########
# https://stackoverflow.com/questions/15871522/is-digest-function-in-r-suitable-for-anonymising-participant-identifiers
library(digest)
anonymize <- function(x, algo, salt="") {
    y <- paste(x, salt, sep="")
    y <- sapply(y, function(X) digest(X, algo=algo))
    ifelse(is.na(x), NA, as.character(y))
}


### MAIN PROGRAM ############

hc$employee_id <- NULL
hc$office <- NULL
hc$proposal_y <- NULL
hc$off <- NULL

# remove dimnames
for (v in colnames(hc)) {
     attr(hc[,deparse(as.name(v))], "dimnames") <- NULL
}


# Save curated file
if (file.exists("data-clean/mgh2.RData")) {
  stop("File exists already!")
}
save(hc, ratings, file="data-clean/mgh2.RData")
