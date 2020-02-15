FROM trestletech/plumber
MAINTAINER Docker User <docker@user.org>

RUN apt-get update --allow-releaseinfo-change -qq && apt-get install -y \
  texlive-full \
  git-core \
  libssl-dev \
  default-jdk \
  libcurl4-openssl-dev \
  libxml2-dev \
  texlive-latex-recommended \
  pandoc \
  libpq-dev -y

RUN R CMD javareconf

RUN R -e "install.packages('devtools')"
RUN R -e "install.packages('glue')"
RUN R -e "install.packages('tidyverse')"
RUN R -e "install.packages('tidyquant')"
RUN R -e "install.packages('tictoc')"
RUN R -e "install.packages('openxlsx')"
RUN R -e "install.packages('Rook')"
RUN R -e "install.packages('jsonlite')"
RUN R -e "install.packages('RPostgres')"
RUN R -e "install.packages('dotenv')"

ARG DUMMY=unknown
RUN R -e 'devtools::install_github("fdrennan/stockAPI")'
ARG DUMMY=unknown
COPY base_notebook.Rmd base_notebook.Rmd
COPY plumber.R /app/plumber.R

EXPOSE 8000
ENTRYPOINT ["R", "-e", "pr <- plumber::plumb(commandArgs()[4]); pr$run(host='0.0.0.0', port=8000)"]
CMD ["/app/plumber.R"]
