FROM debian:12-slim AS builder
RUN apt-get update \
  && apt-get install -yqq libc-dev binutils \
  && apt-get clean \
  ;
ARG ROC_URL="https://github.com/roc-lang/roc/releases/download/nightly/roc_nightly-linux_x86_64-latest.tar.gz"
ADD ${ROC_URL} /tmp/roc.tar.gz
WORKDIR /tmp
RUN tar xzf roc.tar.gz \
  && mkdir /roc \
  && mv roc*/* /roc \
  ;
WORKDIR /app
COPY main.roc /app
RUN /roc/roc build --optimize --linker legacy main.roc | grep "successfully building"

FROM debian:12-slim
COPY --from=builder /app/main /app/main
ENTRYPOINT ["/app/main"]
