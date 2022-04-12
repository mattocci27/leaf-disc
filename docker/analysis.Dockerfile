FROM mattocci/rstan:4.1.2

ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update -q -y \
  && apt-get install --no-install-recommends --fix-missing -y \
    ttf-mscorefonts-installer \
    gosu \
  && apt-get autoremove -y \
  && apt-get clean all

RUN install2.r -n -4 --skipinstalled --error \
  devtools \
  tictoc \
  patchwork \
  smatr \
  ggrepel \
  DT \
  extrafont \
  lmtest \
  semPlot \
  tarchetypes \
  clustermq \
  janitor \
  visNetwork

RUN installGithub.r -u FALSE \
  mattocci27/ggsma \
  mattocci27/ggpubr \
  ropensci/targets \
  ropensci/stantargets

RUN R -e 'remotes::install_version("Rttf2pt1", version = "1.3.8")'

RUN R -e 'extrafont::font_import(prompt = FALSE)'

COPY scripts/entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]