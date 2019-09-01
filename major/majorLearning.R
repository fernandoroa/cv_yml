Learningdata1 <- read.table(
  text =
    "field english spanish portuguese
title \"Introducing python\" \"Introducción a python\" \"Introdução ao python\"
type \"Theoretical and Practical Course\" \"Curso teórico-práctico\"  \"Curso teórico-prático\"
orilan Portuguese Portugués -
institution \"Universidade Federal de Goiás, Goiânia, Brasil\"
term 80h
period \"28/07-09/08 2019\"
year 2019",
  fill = T, header = T, stringsAsFactors = F
)
Learningdata1[Learningdata1 == ""] <- NA
Learningdata1 <- data.frame(t(apply(Learningdata1, 1, zoo::na.locf)))
# Learningdata1[Learningdata1=="-"]<-NA
# Learningdata1
attr(Learningdata1, "dataType") <- "learningItem"

Learningdata2 <- read.table(
  text =
    "field english spanish portuguese
title \"Flow cytometry\" \"Citometria de flujo\" \"Citometria de fluxo\"
type \"Theoretical and Practical Course\" \"Curso teórico-práctico\"  \"Curso teórico-prático\"
orilan - Inglés Inglês
institution \"Universidade Federal de Pernambuco, Recife, Brasil\"
term 15h
period \"30/11-02/12 2015\"
year 2015",
  fill = T, header = T, stringsAsFactors = F
)

Learningdata2[Learningdata2 == ""] <- NA
Learningdata2 <- data.frame(t(apply(Learningdata2, 1, zoo::na.locf)))
attr(Learningdata2, "dataType") <- "learningItem"

Learningdata3b <- read.table(
  text = "field english spanish portuguese
title \"Engativa Wetlands 2013\" \"Humedales Engativá 2013\" \"Areas úmidas de Engativá 2013\"
type \"Course\" \"Curso\"  \"Curso\"
orilan Spanish - Espanhol
institution \"Jardín Botánico de Bogotá\"
term 60h
period \"30/09-30/11 2013\"
year 2013",
  fill = T, header = T, stringsAsFactors = F
)

Learningdata3b[Learningdata3b == ""] <- NA
Learningdata3b <- data.frame(t(apply(Learningdata3b, 1, zoo::na.locf)))
attr(Learningdata3b, "dataType") <- "learningItem"

Learningdata3 <- read.table(
  text = "field english spanish portuguese
title \"Isolation of cells by Splitt and acoustofluidics\" \"Separación de células por Splitt y acustofluídica\" \"Separação de células por Splitt e acustofluídica\"
type \"Theoretical Course\" \"Curso teórico\"  \"Curso teórico\"
orilan Spanish - Espanhol
institution \"Universidad Nacional de Colombia. Centro Internacional de Física\"
term 32h
period \"28/01-08/02 2013\"
year 2013",
  fill = T, header = T, stringsAsFactors = F
)
Learningdata3[Learningdata3 == ""] <- NA
Learningdata3 <- data.frame(t(apply(Learningdata3, 1, zoo::na.locf)))
attr(Learningdata3, "dataType") <- "learningItem"

Learningdata4 <- read.table(
  text = "field english spanish portuguese
title \"2nd Biological evolution workshop\" \"2do Taller de evolución\" \"2da oficina de evolução\"
type Workshop Taller Oficina
orilan - Inglés Inglês
institution \"Universidade Federal de Rio Grande do Sul, Porto Alegre, Brasil\"
term 24h
period \"16/11-18/11 2009\"
year 2009",
  fill = T, header = T, stringsAsFactors = F
)
Learningdata4[Learningdata4 == ""] <- NA
Learningdata4 <- data.frame(t(apply(Learningdata4, 1, zoo::na.locf)))
attr(Learningdata4, "dataType") <- "learningItem"

Learningdata5 <- read.table(
  text = "field english spanish portuguese
title \"Identification and characterization of gene function in plants and microorganisms\" \"Identificación y caracterización de la función genica en plantas y microorganismos\" \"Identificação e caracterização da função gênica en plantas e microorganismos\"
type \"Theoretical Course\" \"Curso teórico\"  \"Curso teórico\"
orilan Portuguese Portugués -
institution \"Universidade Federal Rural de Pernambuco, Recife, Brasil\"
term 20h
period \"28/07-01/08 2008\"
year 2008",
  fill = T, header = T, stringsAsFactors = F
)
Learningdata5[Learningdata5 == ""] <- NA
Learningdata5 <- data.frame(t(apply(Learningdata5, 1, zoo::na.locf)))
attr(Learningdata5, "dataType") <- "learningItem"

Learningdata6 <- read.table(
  text = "field english spanish portuguese
title \"Stem cells, facts, fiction and future\" \"Células madre, hechos, ficción, futuro\" \"Células tronco, fatos, ficção, futuro\"
type \"Congress Course\" \"Curso en congreso\"  \"Curso em congresso\"
orilan Portuguese Portugués -
institution \"53 Brazilian congress of genetics, Águas de Lindóia, Brasil\"
term 3h
period \"02/09-05/09 2007\"
year 2007",
  fill = T, header = T, stringsAsFactors = F
)
Learningdata6[Learningdata6 == ""] <- NA
Learningdata6 <- data.frame(t(apply(Learningdata6, 1, zoo::na.locf)))
attr(Learningdata6, "dataType") <- "learningItem"

Learningdata7 <- read.table(
  text = "field english spanish portuguese
title \"Cytogenetics in the diagnosis and genetic counseling of recurrent abortion\" \"Papel de la citogenética en el diagnóstico y consejería genética de parejas con aborto recurrente\" \"Papel da citogenética no diagnóstico e aconselhamento genético de casais com aborto recorrente\"
type \"Congress Course\" \"Curso en congreso\"  \"Curso em congresso\"
orilan Spanish -  Espanhol
institution \"II Latin-American symposium of cytogenetics and evolution, Palmira, Colombia\"
term 4h
period \"15/08-18/08 2007\"
year 2007",
  fill = T, header = T, stringsAsFactors = F
)
Learningdata7[Learningdata7 == ""] <- NA
Learningdata7 <- data.frame(t(apply(Learningdata7, 1, zoo::na.locf)))
attr(Learningdata7, "dataType") <- "learningItem"

Learningdata8 <- read.table(
  text = "field english spanish portuguese
title \"Chromosomes and phylogeny: The use of cladistics in cytogenetics\" \"Cromosomas y filogenia: Uso de la cladistica en citogenética\" \"Cromossomos e filogenia: o uso da cladística em citogenética\"
type \"Congress Course\" \"Curso en congreso\"  \"Curso em congresso\"
orilan Portuguese Portugués -
institution \"52 Brazilian congress of genetics, Foz de Iguaçú, Brasil\"
term 3h
period \"03/09-09/09 2006\"
year 2006",
  fill = T, header = T, stringsAsFactors = F
)
Learningdata8[Learningdata8 == ""] <- NA
Learningdata8 <- data.frame(t(apply(Learningdata8, 1, zoo::na.locf)))
attr(Learningdata8, "dataType") <- "learningItem"

Learningdata9 <- read.table(
  text = "field english spanish portuguese
title \"Physical mapping and positional cloning in plants\" \"Mapeo físico y clonación posicional en plantas\" \"Mapeamento físico e clonagem posicional em plantas\"
type \"Congress Course\" \"Curso en congreso\"  \"Curso em congresso\"
orilan Portuguese Portugués -
institution \"52 Brazilian congress of genetics, Foz de Iguaçú, Brasil\"
term 3h
period \"03-09-09/09 2006\"
year 2006",
  fill = T, header = T, stringsAsFactors = F
)
Learningdata9[Learningdata9 == ""] <- NA
Learningdata9 <- data.frame(t(apply(Learningdata9, 1, zoo::na.locf)))
attr(Learningdata9, "dataType") <- "learningItem"

Learningdata10 <- read.table(
  text = "field english spanish portuguese
title \"Basic and molecular cytogenetics \" \"Citogenética básica y molecular\" \"Citogenética básica e molecular\"
type Training Entrenamiento Estágio
orilan Spanish  - Espanhol
institution \"Instituto de Genética Humana, Bogotá, Colombia\"
term 2520h
period \"15/09-15/12 2002\"
year 2002",
  fill = T, header = T, stringsAsFactors = F
)
Learningdata10[Learningdata10 == ""] <- NA
Learningdata10 <- data.frame(t(apply(Learningdata10, 1, zoo::na.locf)))
attr(Learningdata10, "dataType") <- "learningItem"

Learningdata11 <- read.table(
  text = "field english spanish portuguese
title \"Animal cell culture\" \"Cultivo de células animales \" \"Cultura de células animais \"
type \"Theoretical and Practical Course\" \"Curso teórico-práctico\"  \"Curso teórico-prático\"
orilan Spanish  - Espanhol
institution \"Instituto Nacional de Salud, Bogotá, Colombia\"
term 84h
period \"04/06-14/07 2001\"
year 2001",
  fill = T, header = T, stringsAsFactors = F
)
Learningdata11[Learningdata11 == ""] <- NA
Learningdata11 <- data.frame(t(apply(Learningdata11, 1, zoo::na.locf)))
attr(Learningdata11, "dataType") <- "learningItem"

Learningdata12 <- read.table(
  text = "field english spanish portuguese
title \"Transgenic plants\" \"Plantas transgénicas\" \"Plantas transgênicas\"
type \"Theoretical Course\" \"Curso teórico\"  \"Curso teórico\"
orilan Spanish  - Espanhol
institution \"Universidad Nacional de Colombia, Bogotá, Colombia\"
term 19h
period \"23/06-24/06 2000\"
year 2000",
  fill = T, header = T, stringsAsFactors = F
)
Learningdata12[Learningdata12 == ""] <- NA
Learningdata12 <- data.frame(t(apply(Learningdata12, 1, zoo::na.locf)))
attr(Learningdata12, "dataType") <- "learningItem"

Learningdata13 <- read.table(
  text = "field english spanish portuguese
title \"Linux Course\" \"Curso de Linux\" \"Curso de Linux\"
type \"Theoretical Course\" \"Curso teórico\"  \"Curso teórico\"
orilan - Inglés Inglês
institution \"Geek University https://geek-university.com\"
term
period \"23/01-24/02 2020\"
year 2020",
  fill = T, header = T, stringsAsFactors = F
)
Learningdata13[Learningdata13 == ""] <- NA
Learningdata13 <- data.frame(t(apply(Learningdata13, 1, zoo::na.locf)))
attr(Learningdata13, "dataType") <- "learningItem"

Learningdata14 <- read.table(
  text = "field english spanish portuguese
title \"Data Science for All (ds4a)\"
type \"Theoretical and Practical Course\" \"Curso teórico-práctico\"  \"Curso teórico-prático\"
orilan - Inglés Inglês
institution \"Correlation One\"
term 375h
period \"20/05-10/09 2021\"
year 2021",
  fill = T, header = T, stringsAsFactors = F
)
Learningdata14[Learningdata14 == ""] <- NA
Learningdata14 <- data.frame(t(apply(Learningdata14, 1, zoo::na.locf)))
attr(Learningdata14, "dataType") <- "learningItem"
