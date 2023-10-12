FROM fredhutch/r-shiny-base:4.3.1

RUN echo break cache

RUN apt-get --allow-releaseinfo-change update -y

RUN apt-get install -y curl libfontconfig1-dev libharfbuzz-dev libfribidi-dev libfreetype-dev libtiff5-dev libsodium-dev

RUN R -e "install.packages(c('dplyr', 'shiny', 'googlesheets4'), repos='https://cran.rstudio.com/')"

ADD . /app

WORKDIR /app

# make sure all packages are installed
# because R does not fail when there's an error installing a package.
RUN R -f check.R --args dplyr shiny googlesheets4

EXPOSE 3838


CMD R -f app.R
