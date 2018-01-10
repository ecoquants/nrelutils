# nrel_limits ----
nrel_limits <- list(
  depth = list(
    breaks = c(0, 60, 200, 1000),
    break_labels = c("0 - 60", "60 - 200", "200 - 1,000")),
  tide = list(
    breaks = c(500, 1000, 1500, Inf),
    break_labels = c("500 - 1,000", "1000 - 1,500", ">1,500"),
    depth = list(
      min = 0,
      max = 100)),
  wave = list(
    breaks = c(10, 20, 30, Inf),
    break_labels = c("10 - 20", "20 - 30", ">30"),
    depth = list(
      min = 0,
      max = 200)),
  wind = list(
    breaks = c(7, 8, 9, Inf),
    break_labels = c("7", "8", ">9"),
    depth = list(
      min = 0,
      max = 1000)))
devtools::use_data(nrel_limits, overwrite = T)
