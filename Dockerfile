FROM ubuntu:20.04

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && \
    apt-get install -y git curl pkg-config libncurses5-dev libpcre3-dev

RUN curl -sSL https://get.haskellstack.org/ | sh

WORKDIR /data

COPY . /data

RUN stack install --local-bin-path bin

FROM ubuntu:20.04

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y ca-certificates curl wget python3 && mkdir -p /data

COPY --from=0 /data/bin/* /usr/bin/
COPY --from=0 /data/public /app

WORKDIR /app

ENTRYPOINT ["/usr/bin/proc"]

CMD ["--host", "0.0.0.0", "--port", "8000", "--source", "/data"]
