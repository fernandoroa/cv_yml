#######################################################################################################
plinksdata <- read.table(
  text =
    " field english spanish portuguese
curriculum https://ferroao.gitlab.io/curriculumpu/
cvlattes http://lattes.cnpq.br/3239984208140922
gitlab https://gitlab.com/ferroao
github https://github.com/fernandoroa
orcid https://orcid.org/0000-0001-5940-4374
", fill = T, header = T, stringsAsFactors = F
)
plinksdata[plinksdata == ""] <- NA
plinksdata <- data.frame(t(apply(plinksdata, 1, zoo::na.locf)))
attr(plinksdata, "dataType") <- "linkItem"
# https://scienti.colciencias.gov.co/cvlac/visualizador/generarCurriculoCv.do?cod_rh=0000878243
#######################################################################################################
languagedata1 <- read.table(
  text =
    "field english spanish portuguese
language English Inglés Inglês
proof \"TOEFL-ITP; TOEFL\"
oripoints \"657/677; 103/120\"
classif \"C1; C1\"
year \"2018; 2012\" ",
  fill = T, header = T, stringsAsFactors = F
)
languagedata1[languagedata1 == ""] <- NA
languagedata1 <- data.frame(t(apply(languagedata1, 1, zoo::na.locf)))
attr(languagedata1, "dataType") <- "languageItem"

languagedata2 <- read.table(
  text =
    "field english spanish portuguese
language German Alemán Alemão
proof \"Sprachdiplom II\"
oripoints Passed Aprob. Aprov.
classif B2-C1
year 1995",
  fill = T, header = T, stringsAsFactors = F
)
languagedata2[languagedata2 == ""] <- NA
languagedata2 <- data.frame(t(apply(languagedata2, 1, zoo::na.locf)))
attr(languagedata2, "dataType") <- "languageItem"

languagedata3 <- read.table(
  text =
    "field english spanish portuguese
language Portuguese Portugués Português
proof CELPE-BRAS
oripoints \"Higher intermediate\" \"Intermedio superior\" \"Intermediário superior\"
classif B2
year 2011",
  fill = T, header = T, stringsAsFactors = F
)
languagedata3[languagedata3 == ""] <- NA
languagedata3 <- data.frame(t(apply(languagedata3, 1, zoo::na.locf)))
attr(languagedata3, "dataType") <- "languageItem"


Armydata1 <- read.table(
  text =
    "field english spanish portuguese
title Militar
institution \"Escuela de Ingenieros Militares, Bogotá, Colombia\"
period \"15-07-1995 - 21-07-1996\"
year 1996",
  fill = T, header = T, stringsAsFactors = F
)
Armydata1[Armydata1 == ""] <- NA
Armydata1 <- data.frame(t(apply(Armydata1, 1, zoo::na.locf)))
attr(Armydata1, "dataType") <- "armyItem"
