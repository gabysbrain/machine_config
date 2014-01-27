
.First <- function() {
  options(
    repos = c(CRAN = 'http://cran.r-project.org/'),
    browserNLdisabled = TRUE,
    deparse.max.lines = 2)
}

if(interactive()) {
  require(devtools)
}

options(rstudio.markdownToHTML = 
  function(inputFile, outputFile) {
    system(paste("multimarkdown", shQuote(inputFile), "-o", shQuote(outputFile)))
  }
)

