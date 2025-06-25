FROM ubuntu:24.04
COPY src /workspace/src/
COPY test /workspace/test/
COPY README.md /workspace/
COPY run.py /workspace/
COPY setup.sh /workspace/
RUN apt update
ENV DEBIAN_FRONTEND=noninteractive
RUN apt -y install gcc-14 g++-14 python3 wget unzip git cmake make vim
