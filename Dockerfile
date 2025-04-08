FROM haskell:9.6.6 AS build

WORKDIR /build
COPY . .
RUN --mount=type=cache,target=/build/.stack-work \
    --mount=type=cache,target=/root/.stack \
    stack install --local-bin-path .

FROM debian:bookworm-slim
ARG DEBIAN_FRONTEND=noninteractive

COPY --from=build /build/prokki /usr/bin/prokki
RUN apt update && apt install -y --no-install-recommends ca-certificates && rm -rf /var/lib/apt/lists/*

EXPOSE 8080
ENTRYPOINT ["prokki"]
CMD ["--host", "0.0.0.0", "--port", "8080", "--index-url", "https://pypi.org/", "--cache-dir", "/index-cache"]
