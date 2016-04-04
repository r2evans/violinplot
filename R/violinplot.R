#' Violin Plots
#'
#' Produce violin (density) plots of the given (optionally grouped)
#' values. Optionally add slender superimposed boxplots.
#'
#' @details
#' 
#' The generic function \code{violinplot} currently has a default method
#' (\code{violinplot.default}), several helper methods (for "list", "matrix",
#' and "data.frame"), and a "formula" interface (\code{violinplot.formula}).
#'
#' If multiple groups are supplied either as multiple arguments are
#' via a formula, parallel violin plots will be plotted, in the order
#' of the arguments or the order of the levels of the groups.
#'
#' Missing values are ignored when forming violin plots.
#'
#' @param x for specifying data from which the violin plots (and
#'   optionally boxplots) are to be produced. Either a numeric vector,
#'   a single list containing such vectors, or a data.frame or matrix
#'   (where columns are the vectors to be used). Additional unnamed
#'   arguments specify further data as separate vectors (each
#'   corresponding to a component violin plot with or without a
#'   corresponding boxplot). \code{NA}s are allowed in the data.
#' @param ... Unnamed arguments ('default' method only) are additional
#'   data vectors; for all other methods, unnamed arguments are
#'   ignored. Named arguments are passed through to the 'default'
#'   method and optionally to \code{\link{plot}}.
#' @param at numeric vector giving the locations where the violin
#'   plots should be drawn, particularly when \code{add = TRUE};
#'   defaults to \code{1:n} for the default, "matrix", and
#'   "data.frame" methods, and spaced for the "formula" method (see
#'   \code{spacer.type}).
#' @param width numeric, how wide the violin plot can extend where "1"
#'   means two neighboring plots may touch but not overlap; greater
#'   than 1 and overlap may occur, less than 1 and there will always
#'   be space between neighboring violin plots.
#' @param names character vector of names to be printed under the
#'   plots; if missing, the names will be derived from \code{groups}
#'   (if not null), the column names of a matrix or data.frame (if
#'   provided), or variables (if a formula).
#' @param col if not \code{NULL} then it is assumed to contain colors
#'   to be used to color the bodies of the violin plots. If the length
#'   is less than the number of plots, then the colors will be
#'   recycled; in the "formula" method, the length should be the
#'   length of the last factor, and it will be recycled "smartly"
#'   (taking into account empty plots). Note: this does not affect the
#'   boxplots: use \code{boxplot = list(col = c(...))} to affect them.
#' @param border an optional vector of colors for the outlines of the
#'   violin plots. As with \code{col}, the values in \code{border} are
#'   recycled. Note: does not affect boxplots, use \code{boxplot =
#'   list(border = c(...))} to change them.
#' @param groups for internal use only (calls between methods), a list
#'   for assigning groups and levels of the data. (Internally, the
#'   structure is a list of lists, where each element of the top-level
#'   list represents a grouping level. Each grouping-level list
#'   contains more lists, where each contains contiguous x positions
#'   for that group.)
#' @param group.spacers numeric, the amount of space to be used for a
#'   spacing between groups, defaults to 1 (same width as an
#'   individual violin plot). When there are multiple levels
#'   ("formula" method), this vector has as many numbers as levels.
#' @param group.labels if logical and \code{TRUE}, plot labels under
#'   all plots and groups (under lines, if present). If numeric, then
#'   the levels under which to add labels. Defaults to \code{TRUE},
#'   all groups.
#' @param group.lines if logical and \code{TRUE}, then lines are drawn
#'   to visually group plots, for all but the lowest-level; if
#'   numeric, indicating which of the levels to apply lines.
#' @param density.auto logical, whether to auto-scale the density
#'   curves to the maximum density of all curves, default \code{TRUE}.
#' @param density.scale if logical and \code{TRUE}, include the scale
#'   (+/- percentages) for the density scales; if a positive integer,
#'   the columns (left-to-right) to show them on; if a negative
#'   integer, right-to-left. Default \code{TRUE} (all).
#' @param density.ticks logical or integer, tick marks on each
#'   grouping line (lowest level only). Default \code{TRUE}. This is
#'   forced to \code{TRUE} if \code{density.scale} is not
#'   \code{FALSE}.
#' @param density.args list of arguments to pass to
#'   \code{\link{density}}. The parameter \code{n} has a default value
#'   of 32 for simplifying the plots, but it may be overridden with
#'   this argument. If not a list, then the actual value is ignored.
#' @param boxplot.args list of arguments to pass to
#'   \code{\link{boxplot}}. The following parameters have defaults to
#'   prevent plotting of the frame, axes, etc as well as for
#'   reasonable aesthetic defaults: \code{pars}, \code{frame.plot},
#'   \code{main}, \code{xlab}, \code{ylab}, \code{xaxt}, \code{yaxt},
#'   \code{add}. Providing any of these arguments overrides the
#'   defaults. If \code{TRUE}, all defaults are used and boxplots are
#'   plotted. If \code{FALSE} or \code{NULL}, boxplots are not
#'   plotted.
#' @param legend.args list of arguments to pass to
#'   \code{\link{legend}}. Parameters \code{xpd} and \code{horiz} are
#'   predefined with defaults. If \code{TRUE} (the default), a legend
#'   is plotted in the top left corner, horizontally. If \code{FALSE}
#'   or \code{NULL}, no legend is printed.
#' @param grid.args list of arguments to pass to \code{\link{grid}}.
#'   If \code{TRUE}, horizontal gray lines are plotted. If
#'   \code{FALSE} or \code{NULL}, grid is not called.
#' @param text.cex numeric, cex to use for group labels; the scale (if
#'   \code{density.scale} is TRUE) is shown at 70\% of this.
#' @param mar if logical and \code{TRUE}, calculate the margins based
#'   on the presence of \code{main}, \code{group.labels}, and
#'   \code{legend}; if numeric, the margins will be assigned to this
#'   value. (The calculation does not adjust the side margins if using
#'   "left" or "right" for the position of \code{legend}.) Default is
#'   \code{FALSE}.
#' @param add logical, create a new plot or add to the existing
#'   device. Default is \code{FALSE}.
#' @return list of various parameters, invisibly
#' @seealso \code{\link{density}} which does much of the computation;
#'   \code{\link{boxplot}} for optional overlaid boxplots;
#'   \code{\link{legend}}; \code{\link{model.frame}}
#' @export
#' @importFrom graphics boxplot
#' @importFrom stats density
#' @importFrom stats model.frame
#' @examples
#' \dontrun{
#'
#' # default method
#' violinplot(mtcars$mpg)
#' violinplot(mtcars$mpg, col = 2)
#' violinplot(mtcars$mpg, col = 4, boxplot.args = FALSE)
#'
#' # matrix method
#' m <- matrix(runif(99), ncol = 3)
#' violinplot(m, col = 2:4)
#' violinplot(m, col = 2:4, width = 1.5)
#'
#' # data.frame method
#' violinplot(mtcars[,1:4], col = 2:9) # extra colors are silently discarded
#'
#' # list method
#' l <- list(x1 = runif(100), x2 = rnorm(200, mean = 1))
#' violinplot(l)
#' 
#' # formula method
#' violinplot(mpg ~ cyl, data = mtcars, col = 2:4)
#' violinplot(mpg ~ cyl, data = mtcars, col = 2:4,
#'     legend.args = list(x = "bottomleft", bty = "n"))
#' violinplot(mpg ~ cyl + vs, data = mtcars, col = 2:4,
#'     group.labels = 2)
#' violinplot(mpg ~ cyl + vs, data = mtcars, col = 2:4,
#'     group.labels = 2, drop.unused = TRUE)
#'
#' violinplot(~ ., data = mtcars) # same as violinplot(mtcars)
#'
#' }
violinplot <- function(x, ...) UseMethod("violinplot")

#' @describeIn violinplot matrix method:
#' @export
violinplot.matrix <- function (x, ...) {
  dots <- list(...)
  at <- 1L:ncol(x)
  groups <- lapply(at, function(a) list(a))
  if (! "names" %in% names(dots)) dots$names <- colnames(x)
  names(groups) <- dots$names
  groups <- list(groups)
  x <- split(c(x), rep.int(at, rep.int(nrow(x), ncol(x))))
  names(x) <- NULL
  do.call('violinplot.default', c(list(x = x[[1]], at = at, groups = groups),
                           x[-1], dots))
}

#' @describeIn violinplot data.frame method:
#' @export
violinplot.data.frame <- function(x, ...) {
  dots <- list(...)
  at <- 1L:ncol(x)
  groups <- list(lapply(at, function(a) list(a)))
  if (! "names" %in% names(dots)) dots$names <- colnames(x)
  names(groups[[1]]) <- dots$names
  x <- as.list(x)
  names(x) <- NULL
  do.call('violinplot.default', c(list(x = x[[1]], at = at, groups = groups),
                           x[-1], dots))
}

#' @describeIn violinplot list method:
#' @export
violinplot.list <- function(x, ...) {
  dots <- list(...)
  at <- seq_along(x)
  groups <- list(lapply(at, function(a) list(a)))
  if (! "names" %in% names(dots)) names <- .Primitive("names")(x)
  names(groups[[1]]) <- names
  names(x) <- NULL
  do.call('violinplot.default', c(list(x = x[[1]], at = at, groups = groups),
                           x[-1], dots))
}

#' @describeIn violinplot "formula" method
#' @param formula a formula, such as \code{y ~ grp}, where \code{yh}
#'   is a numeric vector of data values to be split into groups
#'   according to the grouping variable(s) \code{grp}. Also supported
#'   is \code{~ grps} and \code{~ .}, which is analogous to choosing
#'   select columns of the data.frame.
#' @param data data.frame or list from which the variables in
#'   \code{formula} should be taken.
#' @param na.action passed to \code{\link{model.frame}}
#' @param drop.unused passed to \code{\link{model.frame}}
#' @export
violinplot.formula <- function(formula, data = NULL, ...,
                        na.action = NULL, drop.unused = FALSE) {
  if (missing(formula) || (length(formula) < 2L)) 
    stop("'formula' missing or incorrect")
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame()))) 
    m$data <- as.data.frame(data)
  m$... <- NULL
  m$na.action <- na.action
  m[[1L]] <- quote(stats::model.frame)
  mf <- eval(m, parent.frame())
  response <- attr(attr(mf, "terms"), "response")

  dots <- list(...)
  dots <- dots[ names(dots) != '' ]

  rhsonly <- length(formula) == 2L

  spl <- 
    if (rhsonly) {
      # only RHS of formula, treat now like a data.frame
      mf
    } else {
      # LHS and RHS of formula
      split(mf[,1], mf[,-1], drop = drop.unused)
    }
  
  numfctrs <- if (rhsonly) ncol(mf) else rev(lengths(lapply(mf[,-1,drop = FALSE], unique)))
  nf <- length(numfctrs)
  prodf <- prod(numfctrs)

  sn <- lapply(strsplit(names(spl), '\\.'), rev)
  foo1 <-
    if (ncol(mf) == 1) {
      1
    } else {
      nf + 1 -
        sapply(seq_along(spl)[-1], function(i) which(sn[[i-1]] != sn[[i]])[1])
    }

  dots$group.spacers <- 
    if (! "group.spacers" %in% names(dots)) {
    rep(1, length(spl))
    } else if (is.character(dots$group.spacers)) {
      switch(dots$group.spacers,
             "exponential" = 2 ^ (seq_along(numfctrs) - 1),
             "linear" = seq_along(numfctrs),
             stop(sprintf("unrecognized group.spacers: '%s'",
                          dots$group.spacers)) )
    } else if (length(dots$group.spacers) == 1) {
      rep(dots$group.spacers, nf)
    }
  at <- cumsum(c(if (rhsonly && ncol(mf) == 1) NULL else 1,
                 dots$group.spacers[ foo1 ]))

  groups <- lapply(1:max(lengths(sn)), function(i) {
    first <- sapply(sn, `[[`, i)
    sapply(split(seq_along(first), first), function(is) {
      lapply(split(is, cumsum(c(1, diff(is)) != 1)), function(j) at[j])
    }, simplify = FALSE)
  })
  names(groups) <- if (rhsonly) NULL else names(numfctrs)
  
  if (! "ylab" %in% names(dots)) {
    dots$ylab <- if (rhsonly) "Y" else colnames(mf)[1]
  }

  do.call('violinplot.default', c(list(x = unname(spl[[1]]), at = at, groups = groups),
                           unname(spl[-1]), dots))
}

#' @describeIn violinplot default method:
#' @export
violinplot.default <- function(x, ..., at = NULL, width = 1, names = NULL,
                        col = NULL, border = 1,
                        groups = NULL, group.spacers = 1,
                        group.labels = NULL, group.lines = TRUE,
                        density.auto = TRUE, density.scale = TRUE, density.ticks = TRUE,
                        density.args = NULL, boxplot.args = TRUE, legend.args = NULL,
                        grid.args = TRUE, text.cex = 1,
                        mar = FALSE, add = FALSE) {
  args <- list(x, ...)
  nms <- names(args)
  if (is.null(nms)) nms <- rep("", length(args))
  dots <- args[ ! nms %in% c("x", "") ]
  args <- args[ nms %in% c("x", "") ]
  nargs <- length(args)

  if (is.null(names)) {
    m <- as.list(match.call(expand.dots = TRUE))[-1]
    nms <- names(m)
    names <- as.character(m[ nms %in% c("x", "") ])
    names(args) <- names
  }

  defdensityargs <- list(n = 32L)
  defgridargs <- list(nx = NA, ny = NULL, col = "gray", lty = "dotted", lwd = 0.2)
  deflegendargs <- list(x = "topleft", xpd = TRUE, horiz = TRUE)
  defboxplotargs <- list(pars = list(boxwex = 0.05, staplewex = 0, outwex = 0.5),
                         # the following options are to prevent boxplot()
                         # from plotting anything other than the actual boxplots
                         frame.plot = FALSE, main = "", xlab = "", ylab = "",
                         xaxt = "n", yaxt = "n", add = TRUE,
                         # I think the default pch should be 16 for most everything ...
                         pch = 16
                         )
  doBP <- is.list(boxplot.args) || (is.logical(boxplot.args) && boxplot.args)

  dens <- lapply(args, function(l) {
    if (is.null(l) || length(l) < 2) {
      list(x = l, y = rep(0, length(l)), n = length(l))
    } else {
      do.call("density", merge.list(list(x = l),
                                    density.args,
                                    defdensityargs))
    }
  })

  if (is.null(at)) {
    at <-
      if (group.spacers == 1) {
        seq_along(args)
      } else {
        seq(1, by = group.spacers[[1]], length = length(args))
      }
  }

  if (is.null(groups)) {
    groups <- lapply(1:max(lengths(names)), function(i) {
      sapply(split(seq_along(names), names), function(is) {
        lapply(split(is, cumsum(c(1, diff(is)) != 1)), function(j) at[j])
      }, simplify = FALSE)
    })
  }

  if (missing(at)) at <- 1
  # "just above" 0.5 in order to contain the plot that should go out
  # to 0.5 at some point
  xrange <- width * c(-0.51, 0.51) + range(at)
  yrange <- range(sapply(dens, function(d) {
    if (d$n == 0) NA else range(d$x, na.rm = TRUE)
  }), na.rm = TRUE)
  densmax <- 
    if (density.auto) {
      max(sapply(dens, function(d) {
        if (d$n == 0) NA else max(d$y, na.rm = TRUE)
      }), na.rm = TRUE)
    } else 1

  if (is.null(group.labels)) {
    l <- length(groups)
    group.labels <- if (l > 1) l else 1
  }

  mainmar <- if (! is.null(dots$main)) 3 else 0

  if (is.null(legend.args)) {
    legtopmar <- legbotmar <- 0
  } else {
    # legend.args <- list()
    deflabels <- names(groups[[ length(groups) ]])
    group.labels <- min(group.labels, length(groups) - 1)
    if (is.character(legend.args)) {
      legpos <- legend.args
      legtext <- deflabels
    } else if (is.list(legend.args)) {
      legpos <- if ("x" %in% names(legend.args)) legend.args$x else "bottomleft"
      legend.args <- legend.args[ ! names(legend.args) %in% c('x', 'legend') ]
      legtext <-
        if ("legend" %in% names(legend.args)) {
          if (is.logical(legend.args$legend)) {
            deflabels
          } else {
            legend.args$legend
          }
        } else deflabels
    } # if (is.character(legend.args)) ... else
    if (grepl("top", legpos)) {
      legtopmar <- 2
      legbotmar <- 0
    } else {
      legtopmar <- 0
      legbotmar <- 2
    }
  } # if (is.null(legend.args)) .. else
  
  defplotargs <- list(xlim = xrange, ylim = yrange,
                      xaxt = "n", xaxs = "i", yaxt = "n",
                      xlab = "", ylab = "Y", frame.plot = FALSE)
  dots <- merge.list(dots, defplotargs)

  if (! add) {
    if (is.null(mar) || (is.logical(mar) && mar)) {
      opar <- par(mar = c(legbotmar + group.labels + 1, 4, mainmar + legtopmar, 1) + 0.1)
      on.exit(par(opar))
    }

    do.call("plot", c(list(x = NA), dots))
    axis(2, las = 1)
  }
  usr <- par('usr')
  widen <- 0.4 * min(1, width)

  if (is.logical(grid.args)) grid.args <- if (grid.args) defgridargs else NULL
  if (! is.null(grid.args)) {
    grid.args <- merge.list(grid.args, defgridargs)
    do.call("grid", grid.args)
  }
  tick.grid.args <- merge(grid.args, defgridargs)
  # remove these, we're only really interested in "col", "lwd", "lty", ...
  tick.grid.args[c("nx", "ny")] <- NULL

  col <- rep(if (is.null(col)) NA else col, length = nargs)
  border <- rep(if (is.null(border)) 1 else border, length = nargs)
  denom <- (densmax / (width/2))

  if (length(group.lines) == 1) group.lines <- rep(group.lines, nargs)

  # this finds an "order of magnitude" for how to round-down the
  # densmax, going by 0.01 up to 0.25, 0.05 up to 0.5, 0.1 up to 2,
  # and 1 above that.
  z <- cut(0.8 * densmax / denom, breaks = c(-0.01, 0.25, 0.5, 2, Inf),
           labels = FALSE)
  z <- c(100, 20, 10, 1)[z]

  # find a "pretty" x-value for the ticks, no more than 80% of
  # densmax. (The 0.8 reflects the double-width 80% bar underneath).
  xtick <- ceiling(z * 0.8 * densmax / denom - 1) / z
  ytick <- ceiling(z * 0.8 * densmax - 1) / z

  if (! is.null(groups)) {

    seght <- strheight("w", cex = text.cex) / 2

    ngrps <- lengths(groups)
    ngrp1 <- prod(ngrps[-1])
    ngrps <- prod(ngrps)

    for (i in seq_along(groups)) {
      if (i > group.labels) next
      mtext(names(groups)[i], side = 1, line = group.labels - i - 0.5,
            at = line2user(line = 1, side = 2), adj = 1, cex = text.cex)
      for (j in seq_len(length(groups[[i]]))) {
        y <- line2user(line = group.labels - i, side = 1)
        for (k in seq_along(groups[[i]][[j]])) {
          dat <- groups[[i]][[j]][[k]]
          mtext(names(groups[[i]])[j], side = 1, line = group.labels - i,
                at = mean(dat), adj = 0.5, cex = text.cex)
          if (group.lines[i]) {
            lines(widen * c(-1, 1) + range(dat), rep(y, 2), xpd = TRUE)

            if (i == length(groups)) {
              if (density.ticks || density.scale) {
                if ((is.numeric(density.ticks) && 
                       ((density.ticks > 0 && ((k-1)*ngrp1 + j) %in% density.ticks) ||
                          (density.ticks < 0 && ((k-1)*ngrp1 + j - ngrps -1) %in% density.ticks))) ||
                      (is.logical(density.ticks) && density.ticks)) {
                  if (density.scale) {
                    segments(dat[1] - c(-xtick, 0, xtick),
                             rep(y, 3L),
                             dat[1] - c(-xtick, 0, xtick),
                             c(y + seght, y + seght / 4L, y + seght),
                             xpd = TRUE)
                    do.call("abline", c(list(v = dat[1] + c(-xtick, 0, xtick)),
                                        tick.grid.args))
                  } else {
                    segments(dat[1] - widen * c(-1, 0, 1), rep(y, 3L),
                             dat[1] - widen * c(-1, 0, 1), c(y + seght, y + seght / 4L, y + seght),
                             xpd = TRUE)
                  }
                }
              }
              if (density.scale) {
                if ((is.numeric(density.scale) && 
                       ((density.scale > 0 && ((k-1)*ngrp1 + j) %in% density.scale) ||
                          (density.scale < 0 && ((k-1)*ngrp1 + j - ngrps - 1) %in% density.scale))) ||
                      (is.logical(density.scale) && density.scale)) {
                  # text(dat[1] - widen, y, labels = sprintf("%0.0f%%", densmax * -100),
                  #      adj = c(-0.1, 1.2), xpd = TRUE, cex = 0.7 * text.cex)
                  text(dat[1] + xtick, y, labels = sprintf("%0.02f", ytick),
                       adj = c(0.5, 1.2), xpd = TRUE, cex = 0.7 * text.cex)
                }
              }
            } # if (i == length(groups))
          } # if (group.lines[i])
        } # for (k in seq_along(groups[[i]][[j]]))
      } # for (j in seq_len(length(groups[[i]])))
    } # for (i in seq_along(groups))

  } # if (! is.null(groups))

  for (i in seq_along(dens)) {
    if (dens[[i]]$n < 2) {
      thiscol <-
        if (is.null(col[i])) {
          if (is.null(border[i])) 'black' else border[i]
        } else {
          col[i]
        }
      points(rep(at[i], dens[[i]]$n), dens[[i]]$x, pch = 16, col = thiscol)
    } else {
      polygon(c(at[i] + dens[[i]]$y / denom,
                at[i] - rev(dens[[i]]$y) / denom),
              c(dens[[i]]$x, rev(dens[[i]]$x)),
              col = col[i], border = border[i])
    }
  } # for (i in seq_along(dens))

  if (! is.null(legend.args)) {
    if (grepl("top", legpos)) {
      y <- line2user(line = 0, side = 3)
      yjust <- 0
    } else if (grepl("bottom", legpos)) {
      y <- line2user(line = group.labels + 1.5, side = 1)
      yjust <- 0.5
    } else {
      y <- mean(usr[3:4])
      yjust <- 0.5
    }

    legtext <- c(names(groups)[ length(groups) ], legtext)
    fill <- c(NA, col[1:length(legtext)])
    border <- c(NA, rep(1, length(legtext)))

    if (grepl("left", legpos)) {
      if (grepl("(top|bottom)", legpos)) {
        # add the group label to the left margin, aligned with the other
        # group labels (on the bottom left)
        mtext(legtext[1],
              side = if (grepl("top", legpos)) 3 else 1,
              line = if (grepl("top", legpos)) 0.5 else (group.labels + 1),
              at = line2user(line = 1, side = 2),
              adj = 1
              )
        # undo the extra label at the beginning, added for all other cases
        legtext <- legtext[-1]
        fill <- fill[-1]
        border <- border[-1]
      }
      x <- usr[1]
      xjust <- 0
    } else if (grepl("right", legpos)) {
      x <- usr[2]
      xjust <- 1
    } else {
      x <- mean(usr[1:2])
      xjust <- 0.5
    }

    do.call("legend",
            merge.list(
              list(x = x, y = y, legend = legtext, fill = fill, border = border),
              legend.args,
              # allow the user overrule these two defaults
              list(xjust = xjust, yjust = yjust),
              deflegendargs)
            )
  } # if (! is.null(legend.args))

  if (doBP) {
    if (! is.null(boxplot.args)) {
      boxplot.args <- merge.list(boxplot.args, defboxplotargs)
    }
    if (is.null(boxplot.args$col)) {
      # only white-ify the boxplots if the violin colors are defined
      boxplot.args$col <- ifelse(is.na(col), NA, 'white')
    }
    if (! is.null(boxplot.args$width) && length(boxplot.args$width) != nargs) {
      boxplot.args$width <- rep(boxplot.args$width, nargs)
    }

    bps <- do.call("boxplot", c(list(x = args, at = at), boxplot.args))
  } else {
    bps <- do.call('boxplot', c(list(x = args, at = at, plot = FALSE), boxplot.args))
  } # if (doBP) ... else

  invisible(list(at = at,
                 xrange = xrange, yrange = yrange,
                 densmax = densmax,
                 boxplot = bps))
}
