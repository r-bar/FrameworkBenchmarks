FROM debian:12-slim AS builder
RUN apt-get update \
  && apt-get install -yqq libc-dev binutils sqlite3 \
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
COPY create-sqlite.sql /app
RUN sqlite3 main.db < create-sqlite.sql
RUN /roc/roc build --optimize --linker legacy main.roc \
  | grep -q "successfully building"

FROM debian:12-slim
EXPOSE 8000
ENV ROC_BASIC_WEBSERVER_HOST="0.0.0.0"
COPY --from=builder /app/main /app/main
COPY --from=builder /app/main.db /app/main.db
WORKDIR /app
ENTRYPOINT ["./main"]
