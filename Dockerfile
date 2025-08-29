FROM alpine:latest AS tool-builder

RUN apk update && \
  apk add --no-cache \
  build-base \
  zlib-dev \
  zlib-static \
  git
# RUN apt-get update && apt-get install -y --no-install-recommends git zlib1g-dev

WORKDIR /build
COPY tools .
RUN make

# Database builder image
FROM rocker/r-ver:4.5.1

# install base dependencies
RUN <<EOF
set -eux
apt-get update
apt-get install -y --no-install-recommends \
  libxml2-dev                              \
  libcairo2-dev                            \
  libgit2-dev                              \
  default-libmysqlclient-dev               \
  libpq-dev                                \
  libsasl2-dev                             \
  libsqlite3-dev                           \
  libssh2-1-dev                            \
  libxtst6                                 \
  libcurl4-openssl-dev                     \
  libharfbuzz-dev                          \
  libfribidi-dev                           \
  libfreetype6-dev                         \
  libpng-dev                               \
  libtiff5-dev                             \
  libjpeg-dev
EOF

# install R packages
RUN <<EOF
#!/usr/bin/env Rscript
install.packages("pak", repos = sprintf(
  "https://r-lib.github.io/p/pak/devel/%s/%s/%s",
  .Platform$pkgType,
  R.Version()$os,
  R.Version()$arch
))

pak::pkg_install(c(
  "tidyverse",
  "r-spatial/s2",
  "sf",
  "terra",
  "exactextractr",
  "yyjsonr",
  "SurfRough",
  "arrow",
  "geoarrow",
  "rmarkdown",
  "fs",
  "bench",
  "httr2",
  "promises",
  "future",
  "RcppTOML",
  "R.utils",
  "archive",
  "remotes",
  "stringfish",
  "RcppParallel",
  "toposort",
  "DescTools",
  "qs2",
  "bibtex",
  "openxlsx",
  "prettyunits",
  "testthat",
  "usethis"
))

pak::pak_cleanup(
  package_cache = TRUE,
  metadata_cache = TRUE,
  force  = TRUE
)
EOF

# cleanup
RUN <<EOF
strip /usr/local/lib/R/site-library/*/libs/*.so
rm -rf /var/lib/apt/lists/*
rm -rf /tmp/*
EOF

# setup R
RUN cat <<EOF >> /usr/local/lib/R/etc/Rprofile.site
Sys.setenv(PROCESSX_NOTIFY_OLD_SIGCHLD = 1)
options(
  tidyverse.quiet = TRUE,
  warnPartialMatchAttr = TRUE,
  warnPartialMatchDollar = TRUE,
  warnPartialMatchArgs = TRUE,
  warn = 2,
  mc.cores = parallelly::availableCores(constraints = "multicore", logical = FALSE)
)
# qs2::qopt("nthreads", parallelly::availableCores())
rlang::global_entrace()
library(rlang)
library(vctrs)
EOF

# copy tools
COPY --from=tool-builder /build/ivea-grid/ivea-grid /usr/local/bin

# copy pipeline
COPY . /pipeline/

# set up the the pipeline
WORKDIR /pipeline

RUN touch /.db-builder
RUN mkdir /data && touch /data/.novolume

CMD ["Rscript", "orchestration/execute-pipeline.R"]
