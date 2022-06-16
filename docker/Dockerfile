FROM mattocci/radian:4.1.3

ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update -q -y \
  && apt-get install --no-install-recommends --fix-missing -y \
    cmake \
  && apt-get autoremove -y \
  && apt-get clean all

# Install renv
ENV RENV_VERSION 0.15.4
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"


WORKDIR /project
COPY renv.lock renv.lock
RUN R -e 'renv::restore(repos = c(CRAN = "https://cloud.r-project.org"))'

# make a fake Rprofile to skip .Rprofile in the docker container
RUN echo "R_PROFILE_USER = .Rprofile_docker" >> /usr/local/lib/R/etc/Renviron.site


RUN mkdir -p  /opt/cmdstan \
 && Rscript -e "cmdstanr::install_cmdstan(dir = '/opt/cmdstan', release_url = 'https://github.com/stan-dev/cmdstan/releases/download/v2.29.2/cmdstan-2.29.2.tar.gz', cores = 8, timeout = 2400)"

COPY scripts/entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh

ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
