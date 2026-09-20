# Prokki 🦖

[![Haskell](https://img.shields.io/badge/Haskell-5e5086?logo=haskell&logoColor=white)](https://haskell.org)
[![Python](https://img.shields.io/badge/Python-3776AB.svg?style=flat&logo=python&logoColor=white)](https://www.python.org)

**Prokki** is a lightweight, blazing-fast reverse proxy cache for Python package indexes (like PyPI).
It acts as an middleman between your Python package installer (e.g., `pip`, `uv` or `poetry`) and the public index,
reducing bandwidth usage, improving install speeds, and enhancing reliability in CI/CD pipelines.

Tested with the MinIO and Ceph RGW implementations of S3.

### Features

- Drop-in caching proxy for any [simple repository API](https://packaging.python.org/en/latest/specifications/simple-repository-api/) index (PyPI, PyTorch, etc).
- Serve multiple upstream indexes from a single instance, each mapped under its own path.
- Two-tier caching: a fast in-memory LRU-like cache in front of durable storage — package metadata in PostgreSQL, package files in S3.
- Packages are served via presigned S3 redirects, so package bytes don't flow through Prokki itself once cached.
- Concurrent requests for the same not-yet-cached package are coalesced into a single upstream download.
- Stale-while-unavailable fallback: if the upstream index is unreachable, previously cached project listings keep being served.

### Usage

Create a configuration file, `config.toml` for example:

```toml
host = "0.0.0.0"  # address Prokki listens on
port = 8080

log.severity = "Warning"

project_cache_ttl = 1440  # minutes
project_cache_max_size = 3000  # max in-memory entries in the project (simple index) page cache; LRU-evicted past this size
package_cache_max_size = 3000  # max in-memory entries in the package/metadata cache entry table; LRU-evicted past this size (package bytes themselves live in S3, not this cache)

response_timeout = 30

[http]
base_url = "http://localhost:8080"  # public URL clients use to reach this Prokki instance

[postgres]
host = "localhost"
port = 5432
database = "prokki"
secure = false

[s3]
host = "localhost"
port = 9000
bucket = "prokki"
secure = false

[[index]]
name = "pypi"
url = "https://pypi.org/simple"

[[index]]
name = "torch-cu118"
url = "https://download.pytorch.org/whl/cu118"
```

Apply migrations:

```bash
PGUSER=? PGPASSWORD=? PGDATABASE=prokki PGHOST=? PGPORT=? sqitch deploy db:pg:prokki
```

Start Prokki:

```bash
docker run \
	--detach \
	--name prokki \
	-e AWS_ACCESS_KEY_ID=? \
	-e AWS_SECRET_ACCESS_KEY=? \
	-e PGUSER=? \
	-e PGPASSWORD=? \
	--publish 8080:8080 \
	--volume $(pwd)/config.toml:/config.toml \
	ghcr.io/senysenyseny16/prokki
```

Specify it as the index for your package manager; in this example, `uv` is used:

```bash
uv pip install torch --index http://<host>:<port>/<index>
```

##### Filesystem cache (no S3/Postgres)

Starting from `0.3.0`, Prokki requires S3 (or an S3-compatible service) and Postgres to run — it still needs to
reach the public index to serve packages, so it's not a fully offline mode.
If you'd rather not run S3/Postgres and are fine with a single instance caching to local disk/volume,
pin the Docker image to the last `0.2.x` release, `0.2.15`.

Note the config format differs from the `0.3.x` example above.
See [`v0.2.15`](https://github.com/senysenyseny16/prokki/tree/v0.2.15) for the full set of options for this version.

Note that version 0.2.x supports fewer features than 0.3.x.
This is especially true for PyTorch indexes, which have recently started to contain links pointing not only to themselves.
