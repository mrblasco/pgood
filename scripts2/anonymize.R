# anonymize MGH data
load("data-clean/mgh.RData")
str(hc)

### FUNCTIONS ########
# https://stackoverflow.com/questions/15871522/is-digest-function-in-r-suitable-for-anonymising-participant-identifiers
library(digest)
anonymize <- function(x, algo, salt="") {
    y <- paste(x, salt, sep="")
    y <- sapply(y, function(X) digest(X, algo=algo))
    ifelse(is.na(x), NA, as.character(y))
}


### MAIN PROGRAM ############
# remove employee names and office location
for (v in c("employee_id", "office")) {
  hc[,v] <- anonymize(hc[, v], algo="crc32")
}
str(hc)

# remove dimnames
for (v in colnames(hc)) {
     attr(hc[,deparse(as.name(v))], "dimnames") <- NULL
}
str(hc)