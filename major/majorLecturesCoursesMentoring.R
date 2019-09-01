Lecturesdata1 <- read.table(
  text =
    "field english spanish portuguese
institution \"Universidade Federal de Goiás\"
level College Pregrado Graduação
title Cytogenetics Citogenética Citogenética
term 64h
period 2014.I
year 2014",
  fill = T, header = T, stringsAsFactors = F
)
Lecturesdata1[Lecturesdata1 == ""] <- NA
Lecturesdata1 <- data.frame(t(apply(Lecturesdata1, 1, zoo::na.locf)))
attr(Lecturesdata1, "dataType") <- "lectureItem"

Lecturesdata2 <- read.table(
  text =
    "field english spanish portuguese
institution \"Universidade Federal de Goiás\"
level Graduate Posgrado Pós-graduação
title Cytogenetics Citogenética Citogenética
term 40h
period 2015.I
year 2015",
  fill = T, header = T, stringsAsFactors = F
)
Lecturesdata2[Lecturesdata2 == ""] <- NA
Lecturesdata2 <- data.frame(t(apply(Lecturesdata2, 1, zoo::na.locf)))
attr(Lecturesdata2, "dataType") <- "lectureItem"

################################################################################################################## 3
Coursesdata1 <- read.table(
  text =
    "field english spanish portuguese
title \"Molecular and classic Citogenenetics in plants\" \"Citogenética clásica y molecular en plantas\" \"Citogenética clássica e molecular em plantas\"
event \"XXV Semana do ICB, Universidade Federal de Goiás\"
term 6h
orilan portuguese portugués -
period \"18-20 11/2014\"
year 2014",
  fill = T, header = T, stringsAsFactors = F
)
Coursesdata1[Coursesdata1 == ""] <- NA
Coursesdata1 <- data.frame(t(apply(Coursesdata1, 1, zoo::na.locf)))
attr(Coursesdata1, "dataType") <- "courseItem"

Coursesdata2 <- read.table(
  text =
    "field english spanish portuguese
title \"Evolution of cytogenetic characters in the phylogenetic context \" \"Evolución de los caracteres citogenéticos en el contexto filogenético \" \"Evolução dos caracteres citogenéticos no contexto filogenético\"
event \"VI Reunião Brasileira de Citogenética e Citogenômica, Goiânia\"
term 5h
orilan portuguese portugués -
period \"21 05/2019\"
year 2019",
  fill = T, header = T, stringsAsFactors = F
)
Coursesdata2[Coursesdata2 == ""] <- NA
Coursesdata2 <- data.frame(t(apply(Coursesdata2, 1, zoo::na.locf)))
attr(Coursesdata2, "dataType") <- "courseItem"

#############################################################################################################################
Mentoringdata1 <- read.table(
  text =
    "field english spanish portuguese
title \"Undergraduate research \" \"Iniciación Científica\" \"Iniciação Científica\"
institution \"Universidade Federal de Goiás\"
student \"Daniella Rezende\"
period \"01-08-2014 - 31-07-2015\"
year 2015",
  fill = T, header = T, stringsAsFactors = F
)
Mentoringdata1[Mentoringdata1 == ""] <- NA
Mentoringdata1 <- data.frame(t(apply(Mentoringdata1, 1, zoo::na.locf)))
attr(Mentoringdata1, "dataType") <- "mentoringItem"
