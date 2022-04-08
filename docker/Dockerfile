FROM mattocci/rmd-light:4.1.2

ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update \
  && apt-get install -y \
  ttf-mscorefonts-installer \
  libzmq3-dev \
  && fc-cache -f \
  && apt-get autoremove -y \
  && apt-get autoclean -y

RUN install2.r -n -4  \
  tictoc \
  patchwork \
  smatr \
  ggrepel \
  DT \
  lmtest \
  semPlot \
  targets \
  tarchetypes \
  clustermq \
  janitor \
  visNetwork

RUN installGithub.r \
  mattocci27/ggsma \
  mattocci27/ggpubr

COPY scripts/entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
