# nrelutils
NREL utilities R package, initially for ecoquants/nrel-uses



## dependencies

Here's some bookkeeping on dependencies added to DESCRIPTION.

```r
devtools::use_package("magrittr")
devtools::use_package("tibble")
devtools::use_package("readr")
devtools::use_package("dplyr")
devtools::use_package("tidyr")
devtools::use_package("purrr")
devtools::use_package("stringr")
devtools::use_package("glue")
devtools::use_package("sf")
devtools::use_package("raster")
devtools::use_package("ggplot2")
devtools::use_package("fasterize") # devtools::install_github("ecohealthalliance/fasterize")
devtools::use_package("tabularaster")  # as_tibble(r)
devtools::use_package("leaflet")
devtools::use_package("ggsn")
devtools::use_package("rasterVis")
devtools::use_package("yaml")
```

## data

```r
devtools::use_data_raw()
```

