
<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src=figures/logo.png align="right" width="12%">

# curriculumPu <br></br>Rmarkdown for creating a CV <br></br><br></br><br></br>

<!-- badges: start -->

<a href="https://liberapay.com/ferroao/donate"><img alt="Donate using Liberapay" src="https://liberapay.com/assets/widgets/donate.svg"></a>
<img src="http://img.shields.io/liberapay/receives/ferroao.svg?logo=liberapay">
<!-- badges: end -->

To access this curriculum: <https://ferroao.gitlab.io/curriculumpu>

The goal of curriculumPu is to make an html curriculum using a file(s)
named `major...R` with data.frames with all your data.

For several languages, use the file `_site.yml` (adding `index2.html`,
etc. pages). Create `index*.Rmd`, to add language versions.

Use `main.Rmd` to change display options, see the functions below.

Use optionally the folder `figures` if you need any, and `.bib` files
(see Summary below)

`functions.R` and `setup.R` are mandatory files, called in `.Rmd files`

This version uses `style.css`, called in `index.Rmd`

## dataTypes and Sections

dataType is an attribute for a data.frame. They are located in the
`major... .R` file(s).

In this example of file `majorDataPu.R` the data.frame `persondata1` is
created.

``` r
persondata1<-read.table(text="
field english spanish portuguese 
name         \"John smith\"
email        \"john at server.com\"  
skype        \"username\" 
id -
idIssuePlace -
idIssueDate -
profId -
profIdDate -
birthdate -
birthCity -
phonewa -
address -
",fill=T,header=T, stringsAsFactors=F)
persondata1[persondata1==""]<-NA
persondata1<-data.frame(t(apply(persondata1, 1, zoo::na.locf)))
attr(persondata1, 'dataType') <- "personItem"
```

**Use the string `data` for naming each of the data.frames in
`major...R` , as in: `persondata1`**, because the `main.Rmd` reads them
so: `listofAllIts<-ls(pattern="data")`

Note the attribute `dataType` and its value `personItem`

Currently, in the `main.Rmd` there are several `dataType` attributes
preestablished. Look at the end of this lines:

``` bash
grep -rnw -e  'readDataTypes' --include='main.Rmd'
##> main.Rmd:167:DataCasted<-readDataTypes("personItem")
##> main.Rmd:196:summaryCasted<-readDataTypes("summaryItem")
##> main.Rmd:209:plinksCasted<-readDataTypes("linkItem")
##> main.Rmd:223:PosiCasted<-readDataTypes("posiItem")
##> main.Rmd:241:StudyCasted<-readDataTypes("studyItem")
##> main.Rmd:267:languageCasted<-readDataTypes("languageItem")
##> main.Rmd:281:ArticleCasted<-readDataTypes("articleItem")
##> main.Rmd:301:RepoCasted<-readDataTypes("repoItem")
##> main.Rmd:321:LecturesCasted<-readDataTypes("lectureItem")
##> main.Rmd:335:MentoringCasted<-readDataTypes("mentoringItem")
##> main.Rmd:348:CoursesCasted<-readDataTypes("courseItem")
##> main.Rmd:360:talkCasted<-readDataTypes("talkItem")
##> main.Rmd:374:abstractCasted<-readDataTypes("abstractItem")
##> main.Rmd:390:LearningCasted<-readDataTypes("learningItem")
##> main.Rmd:406:ArmyCasted<-readDataTypes("armyItem")
##> main.Rmd:418:PerRefCasted<-readDataTypes("perrefItem")
##> main.Rmd:430:DonCasted<-readDataTypes("donateItem")
```

Let see another of those dataTypes, i.e. `abstractItem` in
`majorAbstracts.R`

``` r
abstractdata1<-read.table(text="
field english spanish portuguese 
author \"Smith, J; Trump, A\"
title \"Title of Abstract\"
event \"IV Congress of research in\"
mode Poster
book \"Annals of the IV Congress of research\"
pages -
orilan - inglés inglês
place \"New York\"
date 2016
year 2016",fill=T,header=T, stringsAsFactors=F)
abstractdata1[abstractdata1==""]<-NA
abstractdata1<-data.frame(t(apply(abstractdata1, 1, zoo::na.locf)))
attr(abstractdata1, 'dataType') <- "abstractItem"
```

Each `dataType` corresponds to a section, which title is controlled in
the data.frame `SectionIts` in `main.Rmd`

``` r
SectionIts<-read.table(text=
"dataType english spanish portuguese
1 personItem \"Personal Data\" \"Datos personales\" \"Dados pessoais\"
2 summaryItem Summary Resumen Resumo 
...
```

##### Articles

One exception to the above logic is for the “Articles”. They can be also
imported automatically from a `.bib` file, see `majorArticles.R`

## Fields

Now you have notice the fields differ from `personItem`and
`abstractItem`. You can use any fields you like, but when rendering, you
will have to change the related part of the `main.Rmd`:

Note that for abstracts, this are the fields currently shown: `title,
mode, author, event, place, book, pages, orilan` and `date`:

    abstractCasted<-readDataTypes("abstractItem")
    
    if(nrow(abstractCasted)>0){
    for (i in 1:nrow(abstractCasted)){
        addFields("title","mode",abstractCasted,fieldIts, xtitle = F, Section = T, separator = ". ", FontSizeRightSmall = T)
        addFields("author","date",xCasted=abstractCasted,xFielddata=fieldIts,spaces=1,xtitle = F, separator = ". ")
        addFields(c("event","place"),NA,abstractCasted,fieldIts,spaces=1,xtitle = F, separator = ". ")
        addFields(c("book","pages","orilan"),NA,abstractCasted,fieldIts,spaces=1,xtitle = F, separator = ". ")
    }}

As you see above, the displaying of a title (displayed name for field)
is controlled by the parameter `xtitle`, and also, you will notice the
field name (`field` column) displayed in the CV is actually different.
This is determined (optionally) in the data.frame `fieldIts` at the
beginning of `main.Rmd`, that looks like:

    # main.Rmd
    fieldIts<-read.table(text=
    "             field             english                    spanish            portuguese
          summary1               Part1                      
          summary2               Part2                      
          summary3               Part3                      
          summary4               Part4                    
           activity       \"Main activity\"        \"Actividad principal\"   \"Atividade principal\"
            address             Address                  Dirección              Endereço
              author              author                                     
              begin               Begin                     Inicio                Início
          birthCity                  in                         en                    em
    ...

## Functions

There are 3 functions for displaying the fields and values (Name: John
Smith), which are:

##### `addFields` allows you to make Sections (greater font size) and display any field under each section

See two chunks above for more examples.

    #  example use:
    #    addFields(leftFields=c("email","skype","phonewa","address"), # this is displayed to the left side or sequentially, see separator
    #              rightField=NA,                                 # this is displayed to the right side
    #              xCasted=DataCasted,                            # data source
    #              xFielddata=fieldIts,                           # names of fields data.frame
    #              spaces=0,                                      # number of spaces (for separation) when leftFields' length >1
    #              separator="",                                  # when leftFields len. >1 use "<br>" for each in a new line
    #              ifNAsep=T,                                     # if nothing in the left side, add new empty line when TRUE
    #              xtitle = T,                                    # add name of field
    #              Section=T,                                     # add numeration and font of section
    #              Bold=T,                                        # uses bold font
    #              xtitleRight=F,                                 # same as xtitle for the right side: rightField
    #              FontSizeRightSmall=F,                          # makes font of rightField smaller
    #              )
    #
    
    # functions.R
    addFields<-function (leftFields,rightField=NA,xCasted,xFielddata,spaces,separator="",ifNAsep=FALSE,
                         xtitle,Section=F, Bold=F, xtitleRight=F, FontSizeRightSmall=F){
    ...

##### `CommonTitleOnly` allows you to write a single title (field) without value.

    ##
    ##  example use:
    ##  CommonTitleOnly("signature",fieldIts) 
    ##
    
    CommonTitleOnly<-function (leftFields,xFielddata){
    ...

##### `printLogo` allows you to put a logo floating right

    ##
    ##  example use:
    ##    printLogo("logo", RepoCasted)
    ##
    
    printLogo<-function (field="logo",xCasted){
    ...

## Summary

So all you have to do is:

  - Prepare all your `major...R` files with data.frames’ name containing
    `data` string, and having the attribute `dataType` in folder `major`
  - Use `_site.yml` and `index*.Rmd`to add languages.
  - Modify `main.Rmd` as desired
  - You will need the `style.css` (folder css), `functions.R` and
    `setup.R` files (folder R)
  - If you want, also the `figures` folder and the `*.bib` files (folder
    major)
  - If you use `*.bib` files (folder major), the code in
    `majorArticles.R` must be present
  - Having this types of files, press the “knit button” in Rstudio in
    `index.Rmd`
  - After that, you can print it with your browser, or publish it in
    your git repo, as this one (`.gitlab-ci.yml`), or publish only the
    `_site` folder
  - For producing pdfs print with your internet browser
  - Thank you for your donation

## References

<div id="refs" class="references">

<div id="ref-R-markdown">

Allaire J, Horner J, Xie Y, Marti V, Porte N. 2019. *Markdown: Render
markdown with the c library ’sundown’*.
<https://CRAN.R-project.org/package=markdown> 

</div>

<div id="ref-R-bib2df">

Ottolinger P. 2019. *Bib2df: Parse a bibtex file to a data frame*.
<https://CRAN.R-project.org/package=bib2df> 

</div>

<div id="ref-R-reshape2">

Wickham H. 2020. *Reshape2: Flexibly reshape data: A reboot of the
reshape package*. <https://CRAN.R-project.org/package=reshape2> 

</div>

<div id="ref-R-zoo">

Zeileis A, Grothendieck G, Ryan JA. 2020. *Zoo: S3 infrastructure for
regular and irregular time series (z’s ordered observations)*.
<https://CRAN.R-project.org/package=zoo> 

</div>

</div>
