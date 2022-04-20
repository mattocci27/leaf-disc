FROM mattocci/myenv-light:4.1.3

ENV DEBIAN_FRONTEND noninteractive

# Install renv
ENV RENV_VERSION 0.15.4
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"


WORKDIR /project
COPY renv.lock renv.lock
RUN R -e 'renv::restore(repos = c(CRAN = "https://cloud.r-project.org"))'

# make a fake Rprofile to skip .Rprofile in the docker container
RUN echo "R_PROFILE_USER = .Rprofile_docker" >> /usr/local/lib/R/etc/Renviron.site

COPY scripts/entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh

ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]