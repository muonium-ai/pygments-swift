# Dockerfile (sample)
FROM swift:5.9

WORKDIR /app

COPY . .

ARG CONFIG=release
ENV CONFIG=${CONFIG}

RUN swift build -c ${CONFIG}

CMD ["/app/.build/release/codeviewer", "--help"]
