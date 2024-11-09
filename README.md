
<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src=assets/logo.png align="right" width="12%">

# CV_yml <br></br>Rmarkdown for creating a CV <br></br><br></br><br></br>

The goal of CV_yml is to make an `.html` curriculum and `.pdf` files,
defining formatting and adding data in `yml` files.

## Example workflow

- Create two sibling folders, one of this repository and one with
  personal data.

``` bash
mkdir parent
cd parent
git clone https://github.com/fernandoroa/cv_yml
git clone https://gitlab.com/ferroao/curriculumpu
cd cv_yml
# open folder cv_yml in VSCode to run the scripts
code .

# Create _site (htmls)
# open and run R/script_to_render_site.R

# Create .pdf files
# open and run R/script_to_render_indiv_pdf.R
```

## Adding your personal data

To define languages and profiles, use the file
`curriculumpu/custom/yml/shared_params.yml`

### folders: `data` and `config`

Use files in `curriculumpu/custom/yml/data` folder to add your
information, and files in `curriculumpu/custom/yml/config` folder to
configure how to show it.  
Fields in `curriculumpu/custom/yml/data` and
`curriculumpu/custom/yml/config` yml files should match.

## Configuring fields and chapters

### folder `dictionaries`

Chapters and field names for several languages are configured in folder
`curriculumpu/custom/yml/dictionaries`

### `css` changes

`class` is a key that can be used inside
`curriculumpu/custom/yml/config/FILE.yml`

After using custom classes in `.yml` files,  
add styles in folders `cv_yml/site/styles` or
`curriculumpu/custom/styles`

## Make .pdf documents

To define languages and profiles, use the file
`curriculumpu/custom/yml/shared_params.yml` (and
`cv_yml/R/script_to_render_indiv_pdf.R`).

Use optionally the folder `curriculumpu/custom/figures` if you need any,
and `.bib` files in the folder `curriculumpu/custom/bib`.

- Use file `cv_yml/R/script_to_render_indiv_pdf`

## Render site

- Use the code in `cv_yml/R/script_to_render_site.R`

## Modify default Rmarkdown header

- Use `cv_yml/R/script_footer_generator`

## Secrets

A file such as `curriculumpu/custom/yml/data/personal.yml` can use
secrets

``` yml
name:
  system_var: PERSONAL_NAME
```

Those values come from `curriculumpu/.Renviron` a file that should go in
`.gitignore`

    PERSONAL_NAME="value"

## References

<div id="refs" class="references csl-bib-body" entry-spacing="1">

<div id="ref-R-rmarkdown" class="csl-entry">

Allaire J, Xie Y, Dervieux C, McPherson J, Luraschi J, Ushey K, Atkins
A, Wickham H, Cheng J, Chang W, Iannone R. 2024. *Rmarkdown: Dynamic
documents for r*. <https://github.com/rstudio/rmarkdown>

</div>

<div id="ref-R-shiny" class="csl-entry">

Chang W, Cheng J, Allaire J, Sievert C, Schloerke B, Xie Y, Allen J,
McPherson J, Dipert A, Borges B. 2024. *Shiny: Web application framework
for r*. <https://shiny.posit.co/>

</div>

<div id="ref-R-yaml" class="csl-entry">

Garbett SP, Stephens J, Simonov K, Xie Y, Dong Z, Wickham H, Horner J,
reikoch, Beasley W, O’Connor B, Warnes GR, Quinn M, Kamvar ZN. 2023.
*Yaml: Methods to convert r data to YAML and back*.
<https://github.com/vubiostat/r-yaml/>

</div>

<div id="ref-R-bib2df" class="csl-entry">

Ottolinger P. 2024. *bib2df: Parse a BibTeX file to a data frame*.
<https://docs.ropensci.org/bib2df/>

</div>

<div id="ref-R-base" class="csl-entry">

R Core Team. 2024. *R: A language and environment for statistical
computing* R Foundation for Statistical Computing: Vienna, Austria.
<https://www.R-project.org/>

</div>

<div id="ref-R-stringr" class="csl-entry">

Wickham H. 2023. *Stringr: Simple, consistent wrappers for common string
operations*. <https://stringr.tidyverse.org>

</div>

<div id="ref-R-dplyr" class="csl-entry">

Wickham H, François R, Henry L, Müller K, Vaughan D. 2023. *Dplyr: A
grammar of data manipulation*. <https://dplyr.tidyverse.org>

</div>

<div id="ref-R-purrr" class="csl-entry">

Wickham H, Henry L. 2023. *Purrr: Functional programming tools*.
<https://purrr.tidyverse.org/>

</div>

<div id="ref-rmarkdown2018" class="csl-entry">

Xie Y, Allaire JJ, Grolemund G. 2018. *R markdown: The definitive guide*
Chapman; Hall/CRC: Boca Raton, Florida.
<https://bookdown.org/yihui/rmarkdown>

</div>

<div id="ref-rmarkdown2020" class="csl-entry">

Xie Y, Dervieux C, Riederer E. 2020. *R markdown cookbook* Chapman;
Hall/CRC: Boca Raton, Florida.
<https://bookdown.org/yihui/rmarkdown-cookbook>

</div>

</div>
