decimal_digits <- function(x){
  if (x %% 1 == 0) 0 else nchar(gsub('^.+\\.(.+)$', '\\1', as.character(x)))
}
