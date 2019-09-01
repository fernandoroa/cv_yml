Posidata1 <- read.table(text = "
field english spanish           portuguese
title        \"Post-doc\"        \"Investigador Pos-doctoral\" \"Pesquisador Pós-Doc\"
activity      \"Research in Plant molecular cytogenetics; Teaching in undergraduate and post-graduate\" \"Investigación en citogenética molecular vegetal; docencia en pregrado y posgrado\" \"Pesquisa em citogenética molecular vegetal; docência em graduação e pós-graduação\"
projects     \"\n- Plant cytogenetics database of Cerrado   \n- Reconstruction of ancestral characters in the phylogeny of Fabaceae    \n - Reconstruction of ancestral characters in the phylogenetic context of genus Callisia    \n - Genome size of plants of Cerrado\" \"\n- Base de datos de citogenética vegetal del Cerrado   \n- Reconstrucción de caracteres ancestrales en el contexto filogenético en Fabaceae    \n - Reconstrucción de caracteres ancestrales en el contexto filogenético en el género Callisia    \n - Tamaño genómico de plantas del Cerrado\"  \"\n- Base de dados de citogenética vegetal do Cerrado   \n- Reconstrução de caracteres ancestrais no contexto filogenético em Fabaceae    \n - Reconstrução de caracteres ancestrais no contexto filogenético do género Callisia    \n - Tamanho genômico de plantas do Cerrado\"
institution  \"Universidade Federal de Goiás, Goiânia, Brazil. PPGMP Agronomia\" \"Universidad Federal de Goiás, Goiânia, Brasil. PPGMP Agronomia\" \"Universidade Federal de Goiás, Goiânia, Brasil. PPGMP Agronomia\"
begin feb-2014 02-2014 02-2014
end   jan-2019 01-2019 01-2019
period 2014-2019
year 2014
contact +55-62-3521-1688 +55-62-3521-1688 +55-62-3521-1688
scholarship CAPES CAPES CAPES-PNPD                       ", fill = T, header = T, stringsAsFactors = F)
Posidata1[Posidata1 == ""] <- NA
Posidata1 <- data.frame(t(apply(Posidata1, 1, zoo::na.locf)))
attr(Posidata1, "dataType") <- "posiItem"

Posidata2 <- read.table(text = "
field english spanish           portuguese
title        \"School teacher\"        \"Profesor de Escuela Secundaria\" \"Professor de segundo grau\"
activity      Sciences       Ciencias       Ciências
institution  \"INEM Francisco de Paula Santander, Bogotá - SED\" \"INEM Francisco de Paula Santander, Bogotá - SED\" \"INEM Francisco de Paula Santander, Bogotá - SED\"
begin mai-2013 05-2013 05-2013
end   jan-2014 01-2014 01-2014
period 2013
year 2013
contact +57-1-3241000 1-3241000 +57-1-3241000", fill = T, header = T, stringsAsFactors = F)
Posidata2[Posidata2 == ""] <- NA
Posidata2 <- data.frame(t(apply(Posidata2, 1, zoo::na.locf)))
attr(Posidata2, "dataType") <- "posiItem"

# listofPosiIt<-ls(pattern="Posidata")
# listofPosiItr<-lapply(listofPosiIt, function(x) eval(parse(text=x)) )

Posidata3 <- read.table(text = "field english spanish           portuguese
title        \"Freelance Shiny Developer\"        \"Desarrollador Freelance Shiny\" \"Desenvolvedor Freelance Shiny\"
activity      \"Shiny Interfaces and Backend\" \"Interfaces Shiny y Backend\" \"Interfaces Shiny e Backend\"
projects     \"\n- Modify Shiny Apps to use Shiny modules \n- Backend and Frontend for Production of .pdf .docx reports, from SQL queries\"
             \"\n- Modificar Shiny Apps usando Shiny modules \n- Backend y Frontend para producir reportes .pdf .docx desde consultas SQL\"
             \"\n- Modificar Shiny Apps usando Shiny modules \n- Backend e Frontend para produzir reportes .pdf .docx desde consultas SQL\"
institution  \"Home Office B2B\" \"Home Office B2B\" \"Home Office B2B\"
begin feb-2022 feb-2022 feb-2022
year 2022
end   present presente presente
period 2022-present 2022-presente 2022-presente", fill = T, header = T, stringsAsFactors = F)
Posidata3[Posidata3 == ""] <- NA
Posidata3 <- data.frame(t(apply(Posidata3, 1, zoo::na.locf)))
attr(Posidata3, "dataType") <- "posiItem"
