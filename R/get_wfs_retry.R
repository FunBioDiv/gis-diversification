get_wfs_retry <- function(x, layer, ntry = 5, wait = 5) {
  for (i in seq_len(ntry)) {
    result <- tryCatch(
      get_wfs(x = x, layer = layer),
      error = function(e) e
    )

    if (!inherits(result, "error")) {
      return(result)
    }

    msg <- conditionMessage(result)

    message(
      sprintf(
        "Attempt %d/%d failed",
        i,
        ntry
      )
    )

    Sys.sleep(wait * i)
  }

  warning("All attempts failed")
  return(NULL)
}
