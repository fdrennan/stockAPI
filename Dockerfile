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
RUN R -e "install.packages('Rcpp')"
RUN R -e 'devtools::install_github("fdrennan/biggr")'
RUN R -e 'devtools::install_github("gregce/ipify")'
RUN R -e 'devtools::install_github("fdrennan/stockAPI")'

RUN apt-get install -y \
 python3.6 \
 virtualenv \
 python3-pip \
 python-pip \
 python3-venv

RUN R -e 'library(stockAPI); \
          library(reticulate); \
          use_python("/usr/bin/python3", required = TRUE); \
          install_python(envname="biggr")'

ARG DUMMY=unknown

COPY base_notebook.Rmd base_notebook.Rmd
COPY plumber.R /app/plumber.R
EXPOSE 8000
ENTRYPOINT ["R", "-e", "pr <- plumber::plumb(commandArgs()[4]); pr$run(host='0.0.0.0', port=8000)"]
CMD ["/app/plumber.R"]
