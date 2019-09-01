# Repodata1<-read.table(text="
# field english spanish           portuguese
# type Shiny-App
# title \"Cytogenetics database of Cerrado\" \"Base de datos citogenéticos del Cerrado\" \"Base de datos citogenéticos del Cerrado\"
# repo https://gitlab.com/ferroao/cerradocytopu
# webpage https://cyto.shinyapps.io/cerrado/
# badges [![](https://img.shields.io/badge/doi-10.3897/CompCytogen.v11i2.11395-green.svg)](https://doi.org/10.3897/CompCytogen.v11i2.11395)
# logo figures/CeCylogo.png
# publication https://compcytogen.pensoft.net/article/11395/",
#                        fill=T,header=T, stringsAsFactors=F)
# Repodata1[Repodata1==""]<-NA
# Repodata1<-data.frame(t(apply(Repodata1, 1, zoo::na.locf)))
# attr(Repodata1, 'dataType') <- "repoItem"

Repodata1 <- read.table(text = "
field english spanish           portuguese
type \"Python/R code; Shiny-App\"
title \"NLP for community feedback\"
repo https://gitlab.com/ferroao/nlpfeedback
logo figures/nlplogo.png
", fill = T, header = T, stringsAsFactors = F)
Repodata1[Repodata1 == ""] <- NA
Repodata1 <- data.frame(t(apply(Repodata1, 1, zoo::na.locf)))
attr(Repodata1, "dataType") <- "repoItem"

Repodata4 <- read.table(text = "
field english spanish           portuguese
type Shiny-App
title \"Cytogenetics characteristic evo modeling\" \"Modelación de evolución de características citogenéticas\" \"Modelagem da evo. de carac. citogenéticas\"
repo https://gitlab.com/ferroao/cytoevopri
webpage https://cyto.shinyapps.io/cytoevo/
logo figures/cytoevologo.png
publication - - -", fill = T, header = T, stringsAsFactors = F)
Repodata4[Repodata4 == ""] <- NA
Repodata4 <- data.frame(t(apply(Repodata4, 1, zoo::na.locf)))
attr(Repodata4, "dataType") <- "repoItem"

Repodata2 <- read.table(text = "
field english spanish           portuguese
type \"R-package; Shiny-App\"
title \"idiogramFISH: plot Idiograms and Karyotype indices\" \"idiogramFISH: plot de idiogramas e índices cariotípicos  \" \"idiogramFISH: plot de idiogramas e índices cariotípicos  \"
repo \"https://gitlab.com/ferroao/idiogramFISH, https://cran.r-project.org/web/packages/idiogramFISH\"
logo figures/idiogramFISHlogo.png
CRAN [![](http://cranlogs.r-pkg.org/badges/grand-total/idiogramFISH?color=green)](https://cran.r-project.org/package=idiogramFISH)
webpage https://ferroao.gitlab.io/manualidiogramfish/
webapp https://ferapps.shinyapps.io/iboard/", fill = T, header = T, stringsAsFactors = F)
Repodata2[Repodata2 == ""] <- NA
Repodata2 <- data.frame(t(apply(Repodata2, 1, zoo::na.locf)))
attr(Repodata2, "dataType") <- "repoItem"

Repodata5 <- read.table(text = "
field english spanish           portuguese
type \"R Scripts\"
title \"Reading databases\" \"Bajar info. de bases de datos\" \"Baixando info. de bases de dados\"
logo figures/databaseLogo.png
repo https://gitlab.com/ferroao/getdatabase", fill = T, header = T, stringsAsFactors = F)
Repodata5[Repodata5 == ""] <- NA
Repodata5 <- data.frame(t(apply(Repodata5, 1, zoo::na.locf)))
attr(Repodata5, "dataType") <- "repoItem"

Repodata6 <- read.table(text = "
field english spanish           portuguese
type Rmarkdown
title \"Curriculum\"
repo https://gitlab.com/ferroao/curriculumpu
logo figures/currLogo.png
webpage https://ferroao.gitlab.io/curriculumpu
", fill = T, header = T, stringsAsFactors = F)
Repodata6[Repodata6 == ""] <- NA
Repodata6 <- data.frame(t(apply(Repodata6, 1, zoo::na.locf)))
attr(Repodata6, "dataType") <- "repoItem"

Repodata3 <- read.table(text = "
field english spanish           portuguese
type \"Python/R scripts\"
title \"linkScraping: Creating a .bib library from journal html pages\" \"linkScraping: Creando biblioteca .bib de páginas html de journal\"
repo https://gitlab.com/ferroao/linkscraping
logo figures/linkScrapingLogo.png
", fill = T, header = T, stringsAsFactors = F)
Repodata3[Repodata3 == ""] <- NA
Repodata3 <- data.frame(t(apply(Repodata3, 1, zoo::na.locf)))
attr(Repodata3, "dataType") <- "repoItem"



# cp ../InveCien/figures/logo.png "$PWD/figures/linkScrapingLogo.png"
