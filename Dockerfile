FROM ubuntu:20.04

ENV DEBIAN_FRONTEND=noninteractive
ENV LC_ALL=C

RUN apt-get update && \
    apt-get install -y git curl pkg-config libncurses5-dev libpcre3-dev locales

RUN curl -sSL https://get.haskellstack.org/ | sh

WORKDIR /data

COPY . /data

RUN sed -i 's/build-type:.*/build-type: Simple/' proc.cabal

RUN stack install --local-bin-path bin

FROM ubuntu:20.04

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && \
    apt-get install -y ca-certificates curl wget python3 locales python3-pip git && \
    pip3 install --upgrade pip numpy pandas matplotlib scipy scikit-learn && \
    mkdir -p /data && \
    mkdir -p /app

COPY --from=0 /data/bin/* /usr/bin/
COPY --from=0 /data/public /app

WORKDIR /app

ENTRYPOINT ["/usr/bin/proc"]

CMD ["--host", "0.0.0.0", "--port", "8000", "--source", "/data"]
