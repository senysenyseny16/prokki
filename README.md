# Prokki

[![Haskell](https://img.shields.io/badge/Haskell-5e5086?logo=haskell&logoColor=white)](https://haskell.org)
[![Python](https://img.shields.io/badge/Python-3776AB.svg?style=flat&logo=python&logoColor=white)](https://www.python.org)


```
docker volume create prokki-packages-cache
docker run -it --rm --name prokki -p 5000:8080 -v prokki-packages-cache:/index-cache prokki
```
