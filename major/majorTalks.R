talkdata1 <- read.table(text = "
field english spanish portuguese
title \"Cytogenetics for resolving evolutionary questions\" \"Citogenética na resolução de questões evolutivas\"
event \"Encontro da liga acadêmica de genética\"
orilan portuguese portugués -
place \"UFG. Goiânia, Brasil\"
date 27-04-2018
year 2018", fill = T, header = T, stringsAsFactors = F)
talkdata1[talkdata1 == ""] <- NA
talkdata1 <- data.frame(t(apply(talkdata1, 1, zoo::na.locf)))
attr(talkdata1, "dataType") <- "talkItem"

talkdata2 <- read.table(
  text = "
field english spanish portuguese
title \"Reconstruction of ancestral states of cytogenetic characters in Leguminosae\" \"Reconstrução de estados ancestrais de caracteres citogenéticos em Leguminosae\"
event \"Reunião de citogenética do Brasil Central\"
orilan portuguese portugués -
place \"UCG. Goiânia, Brasil\"
date 20-10-2017
year 2017",
  fill = T, header = T, stringsAsFactors = F
)
talkdata2[talkdata2 == ""] <- NA
talkdata2 <- data.frame(t(apply(talkdata2, 1, zoo::na.locf)))
attr(talkdata2, "dataType") <- "talkItem"

talkdata3 <- read.table(
  text = "
field english spanish portuguese
title \"Database of Plant Cytogenetics. Current situation of the field\" \"Base de dados de citogenética vegetal do Cerrado. Um diagnóstico da área\"
event \"Reunião de citogenética do Brasil Central\"
orilan portuguese portugués NA
place \"UCG. Goiânia, Brasil\"
date 08-12-2016
year 2016",
  fill = T, header = T, stringsAsFactors = F
)
talkdata3[talkdata3 == ""] <- NA
talkdata3 <- data.frame(t(apply(talkdata3, 1, zoo::na.locf)))
attr(talkdata3, "dataType") <- "talkItem"

talkdata4 <- read.table(
  text = "
field english spanish portuguese
title \"Distribution of 5S and 45S rDNA sites and implications\" \"Distribución de los sitios de DNAr 5S y 45S y sus implicaciones\"
event \"II Simposio de Genética\"
orilan spanish - espanhol
place \"Universidad del Quindío. Armenia, Colombia\"
date  \"28-05-2012\"
year 2012",
  fill = T, header = T, stringsAsFactors = F
)
talkdata4[talkdata4 == ""] <- NA
talkdata4 <- data.frame(t(apply(talkdata4, 1, zoo::na.locf)))
attr(talkdata4, "dataType") <- "talkItem"

talkdata5 <- read.table(text = "
field english spanish portuguese
title \"Trends on the distribution of the 45S ribosomal DNA in plants\"
event \"Society for Experimental Biology Main Meeting\"
orilan - inglés inglês
place \"Prague, Czech Republic\" \"Praga, República Checa\" \"Praga, República Checa\"
date  \"02-07-2010\"
year 2010", fill = T, header = T, stringsAsFactors = F)
talkdata5[talkdata5 == ""] <- NA
talkdata5 <- data.frame(t(apply(talkdata5, 1, zoo::na.locf)))
attr(talkdata5, "dataType") <- "talkItem"

talkdata6 <- read.table(text = "
field english spanish portuguese
title \"Cytogenetics and Molecular cytotaxonomy of species of genus Callisia Loefl. (Commelinaceae)\" \"Citogenética y citotaxonomía molecular de algunas especies del género Callisia Loefl. (Commelinaceae)\"
event \"II simposio Latinoamericano de citogenética y evolución\"
orilan spanish - espanhol
place \"UN, Palmira, Colombia\" \"UN, Palmira, Colombia\" \"UN, Palmira, Colômbia\"
date \"17-08-2007\"
year 2007", fill = T, header = T, stringsAsFactors = F)
talkdata6[talkdata6 == ""] <- NA
talkdata6 <- data.frame(t(apply(talkdata6, 1, zoo::na.locf)))
attr(talkdata6, "dataType") <- "talkItem"


talkdata7 <- read.table(text = "
field english spanish portuguese
title \"Poliploidy and genome duplication Brazilian legumes\" \"Poliploidia e duplicação genômica nas Leguminosas Brasileiras\" \"Poliploidía y duplicación genómica en las Leguminosas Brasileñas\"
event \"70 Congresso Nacional de Botânica\"
orilan portuguese - portugués
place \"Maceió, Alagoas, Brazil\" \"Maceió, AL\" \"Maceió, Alagoas, Brasil\"
date \"25-10-2019\"
year 2019", fill = T, header = T, stringsAsFactors = F)
talkdata7[talkdata7 == ""] <- NA
talkdata7 <- data.frame(t(apply(talkdata7, 1, zoo::na.locf)))
attr(talkdata7, "dataType") <- "talkItem"
