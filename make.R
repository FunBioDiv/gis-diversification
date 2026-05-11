system.time({
  source("analysis/F01_get_field_metrics_FR.R")
})

system.time({
  source("analysis/F01_get_field_metrics_CH.R")
})

system.time({
  source("analysis/F02_get_cover_metrics.R")
}) # 15 minutes when everything recomputed
