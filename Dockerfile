FROM conoria/alpine-r-bookdown

WORKDIR /usr/src

COPY . .

RUN R -q -e 'rmarkdown::render("index.Rmd")' && mv _html /public
