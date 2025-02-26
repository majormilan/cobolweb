FROM debian:stable
RUN apt-get update -qq > /dev/null 2>&1 && \
    apt-get install -y \
    gnucobol \
    curl \
    > /dev/null 2>&1 && \
    rm -rf /var/lib/apt/lists/*

RUN mkdir -p /app
COPY webserver.cbl /app/webserver.cbl
COPY entrypoint.sh /app/entrypoint.sh
RUN chmod +x /app/entrypoint.sh
WORKDIR /app
RUN cobc -x -o webserver webserver.cbl
EXPOSE 8080
CMD ["/app/entrypoint.sh"]
