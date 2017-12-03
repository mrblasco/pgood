string.break <- function(x) {
  gsub('(.{1,12})(\\s|$)', '\\1\n', x)
}