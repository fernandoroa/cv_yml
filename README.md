<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src=figures/logo.png align="right" width="12%">

# CV\_yml <br></br>Rmarkdown for creating a CV <br></br><br></br><br></br>

<!-- badges: start -->

<a href="https://liberapay.com/ferroao/donate"><img alt="Donate using Liberapay" src="https://liberapay.com/assets/widgets/donate.svg"></a>
<img src="https://img.shields.io/liberapay/receives/ferroao.svg?logo=liberapay" width="250">
<!-- badges: end -->

To access this curriculum: <https://ferroao.gitlab.io/curriculumpu>

The goal of curriculumPu is to make an html curriculum using a `yml`
files.

For several languages, use the file `_site.yml` (adding `index2.html`,
etc. pages). Create `index*.Rmd`, to add language versions.

Use `config/...yml` to change display options.

Use optionally the folder `figures` if you need any, and `.bib` files in
the folder `bib`.

## folders: `data` and `config`

fields in `data` and `config` yml files should match. Use `data` to add
your information, and config to configure how to show it.

## folder `names`

Chapters and field names for several languages are configured in folder
`names`

## Summary

So all you have to do is:

-   Clone this repository
-   Use `_site.yml` and `index*.Rmd`to add languages.
-   Modify `config` `.yml` files as desired
-   If you want, also the `figures` folder and the `*.bib` files (folder
    `bib`)
-   Having this types of files, press the “knit button” in Rstudio in
    `index.Rmd`, or use `rmarkdown::render_site()`
-   After that, you can print it with your browser, or publish it in
    your git repo, as this one (`.gitlab-ci.yml`).
-   For producing pdfs print with your internet browser

## References

Allaire J, Xie Y, Dervieux C, McPherson J, Luraschi J, Ushey K, Atkins
A, Wickham H, Cheng J, Chang W, Iannone R. 2024. *Rmarkdown: Dynamic
documents for r*. <https://github.com/rstudio/rmarkdown>

Chang W, Cheng J, Allaire J, Sievert C, Schloerke B, Xie Y, Allen J,
McPherson J, Dipert A, Borges B. 2024. *Shiny: Web application framework
for r*. <https://shiny.posit.co/>

Garbett SP, Stephens J, Simonov K, Xie Y, Dong Z, Wickham H, Horner J,
reikoch, Beasley W, O’Connor B, Warnes GR, Quinn M, Kamvar ZN. 2023.
*Yaml: Methods to convert r data to YAML and back*.
<https://github.com/vubiostat/r-yaml/>

Ottolinger P. 2024. *bib2df: Parse a BibTeX file to a data frame*.
<https://docs.ropensci.org/bib2df/>

R Core Team. 2024. *R: A language and environment for statistical
computing* R Foundation for Statistical Computing: Vienna, Austria.
<https://www.R-project.org/>

Wickham H. 2023. *Stringr: Simple, consistent wrappers for common string
operations*. <https://stringr.tidyverse.org>

Wickham H, François R, Henry L, Müller K, Vaughan D. 2023. *Dplyr: A
grammar of data manipulation*. <https://dplyr.tidyverse.org>

Wickham H, Henry L. 2023. *Purrr: Functional programming tools*.
<https://purrr.tidyverse.org/>

Xie Y, Allaire JJ, Grolemund G. 2018. *R markdown: The definitive guide*
Chapman; Hall/CRC: Boca Raton, Florida.
<https://bookdown.org/yihui/rmarkdown>

Xie Y, Dervieux C, Riederer E. 2020. *R markdown cookbook* Chapman;
Hall/CRC: Boca Raton, Florida.
<https://bookdown.org/yihui/rmarkdown-cookbook>
