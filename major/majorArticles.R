# articledata5<-read.table(text="
# field english spanish portuguese
# author \"Roa, F; TELLES, MPC\"
# year 2017
# title \"The Cerrado (Brazil) plant cytogenetics database\"
# journal \"COMPARATIVE CYTOGENETICS\"
# volume 11
# pages 285-297
# url https://compcytogen.pensoft.net/article/11395/",fill=T,header=T, stringsAsFactors=F)
# articledata5[articledata5==""]<-NA
# articledata5<-data.frame(t(apply(articledata5, 1, zoo::na.locf)))
# attr(articledata5, 'dataType') <- "articleItem"
#
# articledata4<-read.table(text="
# field english spanish portuguese
# author \"Roa F, Guerra, M\"
# year    2015
# title    \"Non-Random Distribution of 5S rDNA Sites and Its Association with 45S rDNA in Plant Chromosomes\"
# journal    \"CYTOGENETIC AND GENOME RESEARCH\"
# volume    146
# pages    243-249
# url https://www.karger.com/Article/Abstract/440930",fill=T,header=T, stringsAsFactors=F)
# articledata4[articledata4==""]<-NA
# articledata4<-data.frame(t(apply(articledata4, 1, zoo::na.locf)))
# attr(articledata4, 'dataType') <- "articleItem"
#
# articledata3<-read.table(text="
# field english spanish portuguese
# author \"Roa F, Guerra, M\"
# year 2012
# title \"Distribution of 45S rDNA sites in chromosomes of plants: Structural and evolutionary implications\"
# journal \"BMC Evolutionary Biology\"
# volume 12
# pages 225
# url https://bmcevolbiol.biomedcentral.com/articles/10.1186/1471-2148-12-225",fill=T,header=T, stringsAsFactors=F)
# articledata3[articledata3==""]<-NA
# articledata3<-data.frame(t(apply(articledata3, 1, zoo::na.locf)))
# attr(articledata3, 'dataType') <- "articleItem"
#
# articledata2<-read.table(text="
# field english spanish portuguese
# author \"Marques A, Roa F, Guerra M\"
# year 2010
# title \"Karyotype differentiation in three species of Tripogandra Raf. (Commelinaceae) with different ploidy levels\"
# journal \"Genetics and Molecular Biology\"
# volume 33
# pages 731-738
# url http://www.scielo.br/scielo.php?script=sci_arttext&pid=S1415-47572010000400020",fill=T,header=T, stringsAsFactors=F)
# articledata2[articledata2==""]<-NA
# articledata2<-data.frame(t(apply(articledata2, 1, zoo::na.locf)))
# attr(articledata2, 'dataType') <- "articleItem"
#
# articledata1<-read.table(text="
# field english spanish portuguese
# author \"Berjano  R, Roa F, Talavera S,  Guerra M\"
# year 2009
# title \"Cytotaxonomy of diploid and polyploid 	Aristolochia (Aristolochiaceae) species based on the distribution of CMA/DAPI bands and 5S and 45S rDNA sites\"
# journal \"Plant Systematics and Evolution\"
# volume 208
# pages 219-227
# url https://url.springer.com/article/10.1007/s00606-009-0184-6",
# fill=T,header=T, stringsAsFactors=F)
# articledata1[articledata1==""]<-NA
# articledata1<-data.frame(t(apply(articledata1, 1, zoo::na.locf)))
# attr(articledata1, 'dataType') <- "articleItem"
# View(articledata1)

# View(t(Roa2012) )

#############################################################################################################
#
#    You can use the traditional data.frames, see above, or use a .bib file with your citations -automatic- below
#
#############################################################################################################
listoffiles1 <- list.files(pattern = ".bib", path = paste0(getwd(), "/major"))
ListOfFileNames <- listoffiles1[which(listoffiles1 != "forReadmeOnly.bib")]
ListOfFileNames <- paste0("major/", ListOfFileNames)
# ListOfFileNames

if (length(ListOfFileNames) > 0) {
  filename <- "all.bib"
  tempfile <- paste0(tempdir(), "/", get("filename", envir = environment()))
  outFile <- file(tempfile, "w")
  for (i in ListOfFileNames) {
    x <- readLines(i)
    writeLines(x, outFile)
  }
  close(outFile)
}

# install.packages("bib2df")
library(bib2df)
tryCatch(mybibdf <<- bib2df(tempfile), error = function(e) {
  cat("all.bib not found")
})
# mybibdf
if (exists("mybibdf")) {
  colnames(mybibdf) <- tolower(colnames(mybibdf))
  splitdflist <- split(mybibdf, sort((rownames(mybibdf))))
  for (i in 1:length(splitdflist)) {
    for (j in 1:length(splitdflist[[i]])) {
      splitdflist[[i]][j] <- paste0(unlist(splitdflist[[i]][j]), collapse = "; ")
      splitdflist[[i]][j] <- gsub("(\\{|\\})", "", splitdflist[[i]][j])
      splitdflist[[i]][j] <- gsub("\'", "", splitdflist[[i]][j])
      splitdflist[[i]][j] <- gsub("\\\\", "", splitdflist[[i]][j])
    }
    splitdflist[[i]][splitdflist[[i]] == "NA"] <- NA
  }
  for (i in 1:length(splitdflist)) {
    splitdflist[[i]] <- as.list(splitdflist[[i]])
    fields <- names(splitdflist[[i]])
    splitdflist[[i]] <- do.call(rbind, splitdflist[[i]])
    splitdflist[[i]] <- as.data.frame(splitdflist[[i]], stringsAsFactors = F)
    splitdflist[[i]]$field <- fields
    splitdflist[[i]] <- splitdflist[[i]][, c(2, 1)]
    colnames(splitdflist[[i]])[2] <- "english"
    splitdflist[[i]]$spanish <- splitdflist[[i]]$english
    splitdflist[[i]]$portuguese <- splitdflist[[i]]$english
  }
  for (i in 1:length(splitdflist)) {
    attr(splitdflist[[i]], "dataType") <- "articleItem"
    splitdflist[[i]][splitdflist[[i]] == ""] <- NA
    names(splitdflist)[[i]] <- splitdflist[[i]][which(splitdflist[[i]]$field == "bibtexkey"), ]$english
  }
}

if (exists("splitdflist")) {
  for (n in names(splitdflist)) {
    assign(paste0(n, "data"), splitdflist[[n]])
  }
}
