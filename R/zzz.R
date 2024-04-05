.onAttach <- function(lib, pkg) {
  msg <- c(paste0("manyenviron ", utils::packageVersion("manyenviron")),
           "\nFor more information about the package please visit https://globalgov.github.io/manyenviron/",
           "\nType 'citation(\"manyenviron\")' for citing this R package in publications.")
  packageStartupMessage(msg)      
  invisible()
}
