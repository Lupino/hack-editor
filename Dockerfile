FROM ubuntu:20.04

ENV DEBIAN_FRONTEND=noninteractive
ENV LC_ALL=C

RUN apt-get update && \
    apt-get install -y git curl pkg-config libncurses5-dev libpcre3-dev locales

RUN curl -sSL https://get.haskellstack.org/ | sh

WORKDIR /data

COPY . /data

RUN sed -i 's/build-type:.*/build-type: Simple/' hack-editor.cabal

RUN stack install --local-bin-path bin

FROM ubuntu:20.04

ENV DEBIAN_FRONTEND=noninteractive
ENV TERM=xterm-color
ENV PATH $PATH:/data/bin
ENV SHELL bash
ENV LC_ALL=C

RUN apt-get update && \
    apt-get install -y curl wget python3 locales python3-pip git vim screen xfonts-wqy && \
    locale-gen zh_CN.UTF-8 && \
    locale-gen en_US.UTF-8 && \
    pip3 install --upgrade pip numpy pandas matplotlib scipy scikit-learn && \
    mkdir -p /data && \
    mkdir -p /app/public && \
    ln -s /data/.vim/vimrc /root/.vimrc && \
    ln -s /data/.vim /root/.vim

ENV LC_ALL=en_US.UTF-8

COPY --from=0 /data/bin/* /usr/bin/
COPY public /app/public
COPY source /data

WORKDIR /app

ENTRYPOINT ["/usr/bin/hack-editor"]

CMD ["--host", "0.0.0.0", "--port", "8000", "--source", "/data"]
