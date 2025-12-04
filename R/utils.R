# small handy functions
concat <- function(x, sep = ",") {
  paste(as.character(x), collapse = sep)
}

paste_unique <- function(x) {
  paste(sort(unique(as.character(x))), collapse = ",")
}

lunique <- function(x) {
  length(unique(as.character(x)))
}

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  #usr <- par("usr")
  usr <- par()$usr
  on.exit(par(usr = usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y, use = "complete.obs")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste(prefix, txt, sep = "")
  if (missing(cex.cor)) {
    cex <- 0.5 / strwidth(txt)
  }

  test <- cor.test(x, y, use = "complete.obs")
  # borrowed from printCoefmat
  Signif <- symnum(
    test$p.value,
    corr = FALSE,
    na = FALSE,
    cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
    symbols = c("***", "**", "*", ".", " ")
  )

  xtxt <- ifelse(par()$xlog, 10^0.5, 0.5)
  ytxt <- ifelse(par()$ylog, 10^0.5, 0.5)
  xstar <- ifelse(par()$xlog, 10^0.8, 0.8)
  ystar <- ifelse(par()$ylog, 10^0.8, 0.8)
  text(xtxt, ytxt, txt, cex = cex * abs(r))
  text(xstar, ystar, Signif, cex = cex, col = 2)
}


plotly_df <- function(df, xlab = "project") {
  pal <- pals::glasbey(ncol(df)) #cols25
  fig <- plot_ly(df, type = "bar", name = "Land cover")
  for (i in 1:ncol(df)) {
    fig <- fig %>%
      add_bars(
        x = 1:nrow(df),
        y = df[, i],
        marker = list(color = pal[i], line = list(color = 'black', width = 1)),
        name = names(df)[i],
        width = ~width,
        hoverinfo = 'text',
        hovertext = paste(names(df)[i], ":", round(df[, i] * 100), "%")
      )
  }
  fig <- fig %>%
    layout(
      xaxis = list(
        showline = TRUE,
        linecolor = '#000',
        tickvals = 1:nrow(df),
        ticktext = row.names(df),
        title = xlab
      ),
      barmode = 'stack'
    )
  fig <- fig %>%
    config(modeBarButtons = list(list("toImage")), displaylogo = FALSE)
  return(fig)
}


edges <- function(x, id = NULL, rm = c(), out = c("lines", "perim")) {
  # transform as SpatVect if needed
  if ("sf" %in% class(x)) {
    x <- vect(x)
  }
  if (!"SpatVector" %in% class(x)) {
    stop("Need a SpatVector")
  }
  if (is.null(id)) {
    id <- names(x)[1]
  }
  # define the output format
  out <- match.arg(out, c("lines", "perim"))
  # transform as line
  li_all <- as.lines(x)
  # get the lines of the out border
  li_outer <- as.lines(aggregate(x))
  # get the lines of the out border
  li_rm <- li_all[values(x)[, id] %in% rm]

  # erase the outer border
  li_edge <- erase(li_all, li_outer)
  # erase the lines to remove
  li_edge <- erase(li_edge, li_rm)
  # remove duplicated lines
  li_edge <- aggregate(li_edge)
  # output the edges
  if (out == "lines") {
    return(li_edge)
  } else {
    return(perim(li_edge))
  }
}
