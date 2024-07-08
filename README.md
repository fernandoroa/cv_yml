<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src=assets/logo.png align="right" width="12%">

# CV\_yml <br></br>Rmarkdown for creating a CV <br></br><br></br><br></br>

The goal of CV\_yml is to make an .html curriculum and .pdf using `yml`
files.

To define languages and profiles, use the file
`site/yml/shared_params.yml` (and `R/script_to_render_indiv_pdf.R`).

Use optionally the folder `site/figures` if you need any, and `.bib`
files in the folder `site/bib`.

## folders: `data` and `config`

Fields in `site/yml/data` and `site/yml/config` yml files should match.
Use files in `site/yml/data` folder to add your information, and files
in `site/yml/config` folder to configure how to show it.

## folder `dictionaries`

Chapters and field names for several languages are configured in folder
`site/yml/dictionaries`

## `css` changes

After using custom classes  
modify files in folder `styles`  
and do:

    library(sass)

    sass(
      sass_file("site/styles/main.scss"),
      output = "site/css/style.css"
    )

## Print CV to pdf file

    # system terminal
    # requires: node, npm
    npm install puppeteer

    # R
    # Usage, function in R folder
    # run in the root folder
    source("R/render_single_Rmd.R")
    render_single_Rmd("site/index.Rmd",
      location_site.yml = "site",
      output_dir = "simple_html_no_toc")

    system(paste(
      "node js/print.js",
      file.path(getwd(), "simple_html_no_toc/index.html"),
      file.path(getwd(), "pdf/index_en_devel.pdf")
    ))

## Render site

-   Use the code in `R/script_to_render_site.R`

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
