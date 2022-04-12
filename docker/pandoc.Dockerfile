FROM mattocci/rmd-light:4.1.2

ENV DEBIAN_FRONTEND noninteractive

COPY scripts/entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
