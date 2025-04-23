# Prokki 🦖

[![Haskell](https://img.shields.io/badge/Haskell-5e5086?logo=haskell&logoColor=white)](https://haskell.org)
[![Python](https://img.shields.io/badge/Python-3776AB.svg?style=flat&logo=python&logoColor=white)](https://www.python.org)

**Prokki** is a lightweight, blazing-fast reverse proxy cache for Python package indexes (like PyPI).
It acts as an middleman between your Python package installer (e.g., `pip`, `uv` or `poetry`) and the public index,
reducing bandwidth usage, improving install speeds, and enhancing reliability in CI/CD pipelines.

### Usage

- Create a volume for packages (you can also mount a folder instead):
```bash
docker volume create prokki-packages-cache
```

- Start Prokki:
```bash
docker run -d --name prokki -p 5000:8080 -v prokki-packages-cache:/index-cache ghcr.io/senysenyseny16/prokki
```

By default it:
- listens on port 8080 (in this example, it's mapped to port 5000)
- caches the PyPI index

You can view available options using the following command:
```bash
docker run -it --rm ghcr.io/senysenyseny16/prokki --help
```

- Specify it as the index for your package manager; in this example, `uv` is used:
```bash
uv pip install torch --index http://<address>:<port>/simple
```
