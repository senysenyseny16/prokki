# Prokki 🦖

[![Haskell](https://img.shields.io/badge/Haskell-5e5086?logo=haskell&logoColor=white)](https://haskell.org)
[![Python](https://img.shields.io/badge/Python-3776AB.svg?style=flat&logo=python&logoColor=white)](https://www.python.org)

**Prokki** is a lightweight, blazing-fast reverse proxy cache for Python package indexes (like PyPI).
It acts as an middleman between your Python package installer (e.g., `pip`, `uv` or `poetry`) and the public index,
reducing bandwidth usage, improving install speeds, and enhancing reliability in CI/CD pipelines.

### Usage

Create a configuration file, `config.toml` for example:

```toml
host = "0.0.0.0"
port = 8080

log.severity = "Warning"

project_cache_ttl = 1440  # minutes
project_cache_max_size = 3000
package_cache_max_size = 3000

response_timeout = 30

[http]
base_url = "http://localhost:8080"

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
