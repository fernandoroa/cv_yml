Studydata1 <- read.table(
  text =
    "field english spanish portuguese
1 level Doctorate Doctorado Doutorado
2 title \"Plant Biology\" \"Ciencias Biológicas\" \"Biologia Vegetal\"
3 emphasis Sistematics Sistemática Sistemática
4 institution \"Universidade Federal de Pernambuco, Recife, Brazil\" \"Universidad Federal de Pernambuco, Recife, Brasil\" \"Universidade Federal de Pernambuco, Recife, Brasil\"
5 orilan portuguese portugués NA
6 thesis \"Analysis of the distribution of 5S and 45S rDNA sites in plant karyotypes\" \"Análisis de la distribución de sitios de DNAr 5S y 45S rDNA en cariotipos de plantas\" \"Análise da distribuição dos sitios de rDNA 5S e 45S em cariótipos de plantas\"
7 keywords \"database, ribosomal DNA, FISH, molecular cytogenetics\" \"Base de datos, DNA ribosomal, FISH, citogenética-molecular\" \"Base de datos, DNA ribossomal, FISH, citogenética-molecular\"
8 oprojects \"- Citotaxonomy of Aristolochia  \n- Citotaxonomy of Tripogandra  \n- Citrus Heterocromatin\" \"- Citotaxonomía de Aristolochia  \n- Citotaxonomía de Tripogandra  \n- Heterocromatina de Citrus\" \"- Citotaxonomia de Aristolochia  \n- Citotaxonomia de Tripogandra  \n- Heterocromatina de Citrus\"
9 supervisor \"Marcelo Guerra\" \"Marcelo Guerra\"  \"Marcelo Guerra\"
10 url https://repositorio.ufpe.br/handle/123456789/747 https://repositorio.ufpe.br/handle/123456789/747 https://repositorio.ufpe.br/handle/123456789/747
11 period 2007-2011 2007-2011 2007-2011
12 diplomadate jul-02-2012 02-jul-2012 02-jul-2012
13 validationdate - 02-oct-2013 -
14 grade \"4.3 (from 0.0 to 5.0)\" \"4.3 (de 0.0 a 5.0)\" \"4.3 (de 0.0 a 5.0)\"
15 scholarship \"CNPq – Research funding agency - Brazil\" \"CNPq – Agencia de Fomento a la Investigación - Brasil\" \"CNPq\" ", header = T, stringsAsFactors = F
)
attr(Studydata1, "dataType") <- "studyItem"

Studydata2 <- read.table(
  text =
    "field english spanish portuguese
1 level Master Maestría Mestrado
2 title \"Plant Biology\" \"Biologia Vegetal\" \"Biologia Vegetal\"
3 emphasis Sistematics Sistemática Sistemática
4 institution \"Universidade Federal de Pernambuco, Recife, Brazil\" \"Universidad Federal de Pernambuco, Recife, Brasil\" \"Universidade Federal de Pernambuco, Recife, Brasil\"
5 orilan portuguese portugués NA
6 thesis \"Molecular citotaxonomy of genus Callisia (Commelinaceae)\" \"Citotaxonomía molecular del género Callisia (Commelinaceae)\" \"Citotaxonomia molecular do género Callisia (Commelinaceae)\"
7 keywords \"Molecular cytogenetics, FISH\" \"Citogenética molecular, FISH\" \"Citogenética molecular, FISH\"
9 supervisor \"Marcelo Guerra\" \"Marcelo Guerra\"  \"Marcelo Guerra\"
10 url https://repositorio.ufpe.br/handle/123456789/966 https://repositorio.ufpe.br/handle/123456789/966 https://repositorio.ufpe.br/handle/123456789/966
11 period 2006-2007 2006-2007 2006-2007
12 diplomadate jul-05-2010 05-jul-2010 05-jul-2010
13 validationdate - 02-sept-2011 -
14 grade \"4.0 (from 0.0 to 5.0)\" \"4.0 (de 0.0 a 5.0)\" \"4.0 (de 0.0 a 5.0)\"
15 scholarship \"CAPES – Education funding agency - Brazil\" \"CAPES – Agencia de Fomento a la Ed. Superior - Brasil\" \"CAPES\"
16 distinctions \"Theses - aproved with disctinction\" \"Tesis - aprobada con distinción\" \"Dissertação - aprovada con distinção\"
                        ", header = T, stringsAsFactors = F
)
attr(Studydata2, "dataType") <- "studyItem"

Studydata3 <- read.table(
  text =
    "field english spanish portuguese
1 level College Pregrado Graduação
2 title \"Biology\" \"Biología\" \"Biologia\"
3 emphasis Genetics Genética Genética
4 institution \"Universidad Nacional de Colombia\" \"Universidad Nacional de Colombia\" \"Universidad Nacional de Colombia\"
5 orilan spanish NA espanhol
6 thesis \"Cytogenetic analysis with 4MV X-radiation of cells of early onset Alzheimer patients and controls (Instituto de Genética Humana, PUJ)\" \"Análisis citogenético con radiación X de 4MV de células de pacientes con Alzheimer de inicio temprano y controles (Instituto de Genética Humana, PUJ)\" \"Análise citogenética com radiação X de 4MV de células de pacientes con Alzheimer de inicio precoce e controles (Instituto de Genética Humana, PUJ)\"
7 keywords \"cytogenetics, Alzheimer\" \"Citogenética, Alzheimer\" \"Citogenética, Alzheimer\"
9 supervisor \"Gloria Osorio, Marta Lucía Bueno\" \"Gloria Osorio, Marta Lucía Bueno\"  \"Gloria Osorio, Marta Lucía Bueno\"
10 url http://www.bdigital.unal.edu.co/8057/ http://www.bdigital.unal.edu.co/8057/ http://www.bdigital.unal.edu.co/8057/
12 diplomadate sept-15-2005 15-sept-2005 15-set-2005
13 validationdate - - \"3-dez-2014 Universidade Federal de Minas Gerais\"
14 grade \"4.0 (from 0.0 to 5.0)\" \"4.0 (de 0.0 a 5.0)\" \"4.0 (de 0.0 a 5.0)\"
16 distinctions \"with honors, tuition payment exemption\" \"Matrículas de honor, exención de pago de matrícula\" \"Honras, isenção de pagamento de matrícula\" ", header = T, stringsAsFactors = F
)
attr(Studydata3, "dataType") <- "studyItem"


Studydata4 <- read.table(
  text =
    "field english spanish portuguese
1 level \"High-School\" Bachillerato Segundo-grau
2 title \"High school graduate\" \"Bachiller Ciencias Exactas\" \"Certificado de Segundo-grau\"
3 emphasis Sciences Ciencias Ciências
4 institution \"German School, Bogota\" \"Colegio Andino, Bogotá\" \"Colégio Alemão, Bogotá\"
5 orilan spanish-german español-alemán espanhol-alemão
6 thesis -
7 keywords -
9 supervisor -
10 url -
12 diplomadate jul-15-1995 15-jul-1995
13 validationdate -
14 grade \"8.1 (from 0.0 to 10.0)\" \"8.1 (de 0.0 a 10.0)\"
16 distinctions -",
  fill = T, header = T, stringsAsFactors = F
)
Studydata4[Studydata4 == ""] <- NA
Studydata4 <- data.frame(t(apply(Studydata4, 1, zoo::na.locf)))
attr(Studydata4, "dataType") <- "studyItem"
Studydata4

# listofStIt<-ls(pattern="Studydata")
# listofStItr<-lapply(listofStIt, function(x) eval(parse(text=x)) )
