abstractdata1 <- read.table(text = "
field english spanish portuguese
author \"ROA, F; Telles, MPC\"
title \"The Cerrado Plant cytogenetics database\" \"Base de datos de citogenética de plantas del Cerrado\" \"Base de dados de citogenética de plantas do Cerrado\"
event \"IV Congresso Brasileiro de Recursos Genéticos\"
mode Poster
book \"Anais do IV Congresso Brasileiro de Recursos Genéticos\"
pages -
orilan portuguese portugués -
place \"Curitiba\"
date \"08-11 11/2016\"
year 2016", fill = T, header = T, stringsAsFactors = F)
abstractdata1[abstractdata1 == ""] <- NA
abstractdata1 <- data.frame(t(apply(abstractdata1, 1, zoo::na.locf)))
attr(abstractdata1, "dataType") <- "abstractItem"

abstractdata2 <- read.table(
  text =
    "field english spanish portuguese
author \"ROA, F; Antunes, AM ; Souza, LGR ; Telles, MPC\"
title \"Genome size and chromosome counts in Brazilian Cerrado spermatophytes\" \"Tamaño genómico y conteos cromosómicos en espermatófitas del Cerrado Brasileño\" \"Tamanho genômico e contagens cromossômicas de espermatófitas do Cerrado Brasileiro\"
event \"4 Reunião Brasileira de Citogenética\"
mode Poster
book \"Trabalhos da 4ta RBC\"
pages -
orilan portuguese portugués -
place \"Atibaia\"
date \"26-29 05/2015\"
year 2015",
  fill = T, header = T, stringsAsFactors = F
)
abstractdata2[abstractdata2 == ""] <- NA
abstractdata2 <- data.frame(t(apply(abstractdata2, 1, zoo::na.locf)))
attr(abstractdata2, "dataType") <- "abstractItem"

abstractdata3 <- read.table(
  text =
    "field english spanish portuguese
author \"Guerra M, Roa F\"
title  \"Distribution of 5S and 45S rDNA sites in plant chromosomes\" \"Distribución de sitios de rDNA 5S y 45S en cromosomas de plantas\" \"Distribuição de sítios de rDNA 5S e 45S em cromossomos de plantas\"
event \"Gatersleben Research Conference\"
mode Talk \"Ponencia oral\" Palestra
book \"Annals of the Gatersleben Research Conference\"
pages 53
orilan - Inglés Inglês
place \"IPK Gatersleben, Germany\"
date \"23-25 04/2012\"
year 2012",
  fill = T, header = T, stringsAsFactors = F
)
abstractdata3[abstractdata3 == ""] <- NA
abstractdata3 <- data.frame(t(apply(abstractdata3, 1, zoo::na.locf)))
attr(abstractdata3, "dataType") <- "abstractItem"

abstractdata4 <- read.table(
  text =
    "field english spanish portuguese
author \"Roa F, Guerra M\"
title \"Trends on the distribution of the 45S ribosomal DNA in plants\" \"Tendencias de distribución del DNA ribosomal 45S en plantas\" \"Tendências na distribuição do DNA ribossômico 45S nas plantas\"
event \"Society for Experimental Biology Main Meeting\"
mode Talk \"Ponencia oral\" Palestra
book \"Annals of the SEB Meeting\"
pages 262
orilan - inglés inglês
place \"Prague, Czech Republic\" \"Praga, Rep. Checa\" \"Praga, Rep. Tcheca\"
date \"30-03 07/2010\"
year 2010", fill = T, header = T, stringsAsFactors = F
)
abstractdata4[abstractdata4 == ""] <- NA
abstractdata4 <- data.frame(t(apply(abstractdata4, 1, zoo::na.locf)))
attr(abstractdata4, "dataType") <- "abstractItem"

abstractdata5 <- read.table(
  text =
    "field english spanish portuguese
author \"Roa F, Guerra M\"
title \"Preferential position of 5S ribosomal RNA genes in plant genomes\" \"Distribución Preferential de genes de  RNA ribosomal 5S en genomas de plantas\" \"Distribuição preferencial dos genes do RNA ribossômico 5S nos genomas de vegetais\"
event \"II Brazilian Symposium of Molecular genetics\"
mode Poster
book \"Annals of the II Brazilian Symposium of Molecular genetics\"
pages 180
orilan portuguese portugués -
place \"Búzios, Brasil\"
date \"31-03 04/2009\"
year 2009
", fill = T, header = T, stringsAsFactors = F
)
abstractdata5[abstractdata5 == ""] <- NA
abstractdata5 <- data.frame(t(apply(abstractdata5, 1, zoo::na.locf)))
attr(abstractdata5, "dataType") <- "abstractItem"

abstractdata6 <- read.table(
  text =
    "field english spanish portuguese
author \"Roa F, Berjano R, Guerra M\"
title \"Chromosomal variation of heterochromatin and 5S and 45S rDNA in species of Aristolochia\" \"Variación cromosómica de la heterocromatina y del DNAr 5S y 45S en especies del género Aristolochia\" \"Variação cromossômica da heterocromatina e do DNAr 5S e 45S em espécies do gênero Aristolochia\"
event \"53 Brazilian congress of genetics\"
mode Poster
book \"Annals of the 53 Brazilian congress of genetics\"
pages 56
orilan portuguese portugués -
place \"Aguas de Lindóia, Brasil\"
date \"02-05 09/2007\"
year 2007
", fill = T, header = T, stringsAsFactors = F
)
abstractdata6[abstractdata6 == ""] <- NA
abstractdata6 <- data.frame(t(apply(abstractdata6, 1, zoo::na.locf)))
abstractdata6[abstractdata6 == "-"] <- NA
attr(abstractdata6, "dataType") <- "abstractItem"

abstractdata7 <- read.table(
  text =
    "field english spanish portuguese
author \"Roa F, Guerra M\"
title \"Cytogenetics and molecular cytotaxonomy of species of genus Callisia Loefl. (Commelinaceae)\" \"Citogenética y citotaxonomia molecular de algunas especies del gênero Callisia Loefl. (Commelinaceae)\" \"Citogenética e citotaxonomia molecular de algumas espécies do gênero Callisia Loefl. (Commelinaceae)\"
event \"II Latin-American symposium of cytogenetics and evolution\"
mode Talk \"Ponencia oral\" Palestra
book \"Annals of the II Latin-American symposium of cytogenetics and evolution\"
pages 217
orilan portuguese portugués -
place \"Palmira, Colombia\" \"Palmira, Colombia\" \"Palmira, Colômbia\"
date \"15-18 08/2007\"
year 2007
", fill = T, header = T, stringsAsFactors = F
)
abstractdata7[abstractdata7 == ""] <- NA
abstractdata7 <- data.frame(t(apply(abstractdata7, 1, zoo::na.locf)))
attr(abstractdata7, "dataType") <- "abstractItem"

abstractdata8 <- read.table(
  text =
    "field english spanish portuguese
author \"Roa F, Guerra M\"
title \"Cytogenetics of a spontaneous tetraploid of Nothoscordum pulchellum Kunth (Alliaceae)\" \"Citogenética y reproducción de un tetraploide espontáneo de Nothoscordum pulchellum Kunth (Alliaceae)\" \"Citogenética e Reprodução de um tetraplóide espontâneo de Nothoscordum pulchellum Kunth (Alliaceae)\"
event \"57 National congress of Botany\"
mode Poster
book -
pages -
orilan portuguese portugués -
place \"Gramado, Brasil\"
date \"06-10 11/2006\"
year 2006
", fill = T, header = T, stringsAsFactors = F
)
abstractdata8[abstractdata8 == ""] <- NA
abstractdata8 <- data.frame(t(apply(abstractdata8, 1, zoo::na.locf)))
attr(abstractdata8, "dataType") <- "abstractItem"

abstractdata9 <- read.table(
  text =
    "field english spanish portuguese
author \"Roa F, Guerra M\"
title \"Contrasting chromosome patterns in three species of genus Callisia\" \"Patrones cromosómicos contrastantes entre tres especies del género Callisia\" \"Padrões cromossômicos contrastantes entre três espécies do gênero Callisia\"
event \"52 Brazilian congress of genetics\"
mode Poster
book \"Annals of the 52 Brazilian congress of genetics\"
pages 1107
orilan portuguese portugués -
place \"Foz do Iguaçú, Brasil\"
date \"03-09 09/2006\"
year 2006
", fill = T, header = T, stringsAsFactors = F
)
abstractdata9[abstractdata9 == ""] <- NA
abstractdata9 <- data.frame(t(apply(abstractdata9, 1, zoo::na.locf)))
attr(abstractdata9, "dataType") <- "abstractItem"

abstractdata10 <- read.table(
  text =
    "field english spanish portuguese
author \"Velasco P, Roa F\"
title \"Transport of biomass by Atta laevigata Smith (Formicidae) in the eastern plains\" \"Transporte de biomasa por una colonia de Atta laevigata Smith 1958 (Hymenoptera: Formicidae) en los llanos orientales (Puerto López, Meta, Colombia)\" \"Transporte de biomassa por uma colônia de Atta laevigata Smith 1958 (Hymenoptera: Formicidae) nas planícies orientais (Puerto López, Meta, Colômbia)\"
event \"III Scientific meeting of biology students\"
mode Poster
book \"Acta Biológica Colombiana, Vol. 8 No. 2\"
pages 117
orilan  spanish - espanhol
place \"Bogotá, Colombia\" \"Bogotá, Colombia\" \"Bogotá, Colômbia\"
date \"06-10 10/2003\"
year 2003
", fill = T, header = T, stringsAsFactors = F
)
abstractdata10[abstractdata10 == ""] <- NA
abstractdata10 <- data.frame(t(apply(abstractdata10, 1, zoo::na.locf)))
attr(abstractdata10, "dataType") <- "abstractItem"

abstractdata11 <- read.table(
  text =
    "field english spanish portuguese
author \"Roa F, Pellegrini MOO, Vaio M, Guerra M\"
title \"Splitting is the answer. Cytogenetics and systematics analyses of Callisia Loefl. (Commelinaceae)\" \"Segregar es la respuesta. Análisis citogenéticos y sistemáticos de Callisia Loefl. (Commelinaceae)\" \"Segregar é a resposta. Citogenética e análises sistemáticas de Callisia Loefl. (Commelinaceae) \"
event \"VI Reunião Brasileira de Citogenética e Citogenômica\"
mode Poster
book -
pages -
orilan - inglés inglês
place Goiânia
date \"21-24 05/2019\"
year 2019",
  fill = T, header = T, stringsAsFactors = F
)
abstractdata11[abstractdata11 == ""] <- NA
abstractdata11 <- data.frame(t(apply(abstractdata11, 1, zoo::na.locf)))
attr(abstractdata11, "dataType") <- "abstractItem"

# listofabstractIt<-ls(pattern="abstractdata")
# listofabstractItr<-lapply(listofabstractIt, function(x) eval(parse(text=x)) )
# listofabstractItr
