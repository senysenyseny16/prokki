FROM haskell:9.8.4 AS build

WORKDIR /build
COPY . .
RUN stack install --local-bin-path .

FROM debian:bookworm-slim
LABEL org.opencontainers.image.source=https://github.com/senysenyseny16/prokki
LABEL org.opencontainers.image.description="Python index reverse-proxy cache"
LABEL org.opencontainers.image.licenses=BSD-3

ARG DEBIAN_FRONTEND=noninteractive

COPY --from=build /build/prokki /usr/bin/prokki
RUN apt update && apt install -y --no-install-recommends ca-certificates && rm -rf /var/lib/apt/lists/*

EXPOSE 8080
ENTRYPOINT ["prokki"]
CMD ["--config", "/config.toml"]
