options(
  htmltools.dir.version = FALSE, formatR.indent = 2,
  width = 55, digits = 4, warnPartialMatchAttr = FALSE, warnPartialMatchDollar = FALSE
)
local({
  r <- getOption("repos")
  if (!length(r) || identical(unname(r["CRAN"]), "@CRAN@")) {
    r["CRAN"] <- "https://cran.rstudio.com"
  }
  options(repos = r)
})
lapply(c("rmarkdown", "zoo", "reshape2", "bib2df"), function(pkg) { #' DT', 'citr', 'formatR', 'svglite'
  cat(paste(system.file(package = pkg)))
  if (system.file(package = pkg) == "") install.packages(pkg)
})
library(zoo)
library(reshape2)
