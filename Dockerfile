FROM debian:bookworm-slim AS build

WORKDIR /build
COPY . .
RUN apt update && apt install -y --no-install-recommends \
        ca-certificates curl gcc g++ git gnupg make netbase xz-utils \
        libc6-dev libffi-dev libgmp-dev libnuma-dev libpq-dev libtinfo-dev zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*
RUN curl -sSL https://get.haskellstack.org/ | sh
RUN stack setup
RUN stack install --local-bin-path .

FROM debian:bookworm-slim
LABEL org.opencontainers.image.source=https://github.com/senysenyseny16/prokki
LABEL org.opencontainers.image.description="Python Package Index Cache"
LABEL org.opencontainers.image.licenses=BSD-3

ARG DEBIAN_FRONTEND=noninteractive

COPY --from=build /build/prokki /usr/bin/prokki
RUN apt update && apt install -y --no-install-recommends ca-certificates libpq5 && rm -rf /var/lib/apt/lists/*

EXPOSE 8080
ENTRYPOINT ["prokki"]
CMD ["--config", "/config.toml"]
