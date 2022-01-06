# Custom functions required for CLARITY figures

library(tidyverse)
library(ggbeeswarm)
geo_mean <- function(x, na.rm = FALSE) {
  exp(mean(log(x), na.rm = na.rm))
}

geo_sd <- function(x, na.rm = FALSE) {
  exp(sd(log(x), na.rm = na.rm))
}


StatMeanSd <- ggproto(
  "StatMeanSd",
  Stat,
  required_aes = c("y|x"),
  # non_missing_aes = "weight",
  setup_data = function(data, params) {
    data <- flip_data(data, params$flipped_aes)
    data$x <- data$x %||% 0
    data <- remove_missing(data,
                           na.rm = params$na.rm,
                           vars = "x",
                           name = "stat_meansd")
    flip_data(data, params$flipped_aes)
  },
  
  setup_params = function(data, params) {
    params$flipped_aes <-
      has_flipped_aes(
        data,
        params,
        main_is_orthogonal = TRUE,
        group_has_equal = TRUE,
        main_is_optional = TRUE
      )
    data <-
      flip_data(data, params$flipped_aes)
    
    has_x <-
      !(is.null(data$x) && is.null(params$x))
    has_y <-
      !(is.null(data$y) && is.null(params$y))
    if (!has_x && !has_y) {
      abort("stat_meansd() requires an x or y aesthetic.")
    }
    
    params$width <-
      params$width %||% (resolution(data$x %||% 0) * 0.75)
    
    if (is.double(data$x) &&
        !has_groups(data) && any(data$x != data$x[1L])) {
      warn(
        glue(
          "Continuous {flipped_names(params$flipped_aes)$x} aesthetic -- did you forget aes(group=...)?"
        )
      )
    }
    
    params
  },
  
  extra_params = c("na.rm", "orientation"),
  
  compute_group = function(data,
                           scales,
                           width = NULL,
                           na.rm = FALSE,
                           coef = 1.5,
                           flipped_aes = FALSE) {
    data <- flip_data(data, flipped_aes)
    
    # if (!is.null(data$weight)) {
    #   mod <- quantreg::rq(y ~ 1,
    #                       weights = weight,
    #                       data = data,
    #                       tau = qs)
    #   stats <- as.numeric(stats::coef(mod))
    # } else {
    #   stats <- as.numeric(stats::quantile(data$y, qs))
    # }
    stats <- c(
      "mean" = mean(data$y, na.rm = na.rm),
      "sd" = sd(data$y, na.rm = na.rm)
    )
    
    df <- ggplot2:::new_data_frame(as.list(stats))
    
    n <- sum(!is.na(data$y))
    
    df$x <-
      if (is.factor(data$x))
        data$x[1]
    else
      mean(range(data$x))
    df$width <- width
    df$relvarwidth <- sqrt(n)
    df$flipped_aes <- flipped_aes
    flip_data(df, flipped_aes)
  }
)

stat_meansd <- function(mapping = NULL,
                        data = NULL,
                        geom = "meansd",
                        position = "dodge2",
                        ...,
                        coef = 1.5,
                        na.rm = FALSE,
                        orientation = NA,
                        show.legend = NA,
                        inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatMeanSd,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      orientation = orientation,
      coef = coef,
      ...
    )
  )
}

gm_fig <- function(data, x, y, colour, facet_cols, cex = 1) {
  x <- enquo(x)
  y <- enquo(y)
  colour <- enquo(colour)
  groups <- quos(!!x, !!colour, !!!facet_cols)
  groups <- groups[!duplicated(groups)]
  gmsd_fig <- data %>% 
    mutate(x_mid = as.integer(factor(!!x))) %>% 
    group_by(!!!groups) %>% 
    summarise(
      x_mid = x_mid[[1]],
      gm = geo_mean(!!y, na.rm = TRUE),
      gsd = geo_sd(!!y, na.rm = TRUE),
      g_lower = gm * gsd,
      g_upper = gm / gsd
    ) %>%
    select(-gsd) %>%
    pivot_longer(cols = c(gm, g_lower, g_upper)) %>% 
    transmute(
      !!!facet_cols,
      width = if_else(name == "gm", 0.3, 0.1),
      size = if_else(name == "gm", 1.5, 1),
      alpha = if_else(name == "gm", 0.75, 0.5),
      x = x_mid - width,
      xend = x_mid + width,
      y = value,
      yend = value,
    )
  
  data %>% 
    ggplot(aes(x = !!x, y = !!y)) +
    geom_hline(colour = "#8080FF", yintercept = 15) +
    geom_beeswarm(aes(colour = !!colour), cex = cex) +
    geom_segment(aes(x = x, xend = xend, y = y, yend = yend, size = size, alpha = alpha), gmsd_fig) +
    scale_size_identity() +
    facet_grid(cols = facet_cols, scales = "free_x", space = "free_x") +
    scale_alpha_identity()
}

nk_StatCount <-
  ggproto("StatCount", Stat,
          required_aes = "x",
          default_aes = aes(weight = 1),
          
          setup_params = function(data, params) {
            params
          },
          
          compute_group = function(self, data, scales, width = NULL) {
            x <- data$x
            weight <- data$weight %||% rep(1, length(x))
            width <- width %||% (resolution(x) * 0.9)
            
            count <- as.numeric(tapply(weight, x, sum, na.rm = TRUE))
            count[is.na(count)] <- 0
            
            data.frame(
              count = count,
              prop = count / sum(abs(count)),
              total = sum(count),
              x = sort(unique(x)),
              width = width
            )
          }
  )

nk_stat_count <- function(mapping = NULL, data = NULL, geom = "bar", position = "stack", 
                           ..., width = NULL, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
  params <- list(na.rm = na.rm, width = width, ...)
  layer(data = data, mapping = mapping, stat = nk_StatCount, geom = geom, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = params)
}

label_stat_count_n <- function(mapping = aes(y = 1.05, label = ..count..), data = NULL, geom = "text", position = "stack", 
                                ..., width = NULL, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
  params <- list(na.rm = na.rm, width = width, ...)
  layer(data = data, mapping = mapping, stat = nk_StatCount, geom = geom, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = params)
}

#' Format p values for display
#'
#' @param p p value to format
#' @param p_digits number of digits to display
#' @param small_p_format format used to display p values below a threshold
#' @param small_p_cutoff cutoff for displaying alternative formatting
#' @param sig_fig whether to use significant figures when rounding p values
#' @param n_sig_fig number of significant figures to use
#'
#' @return formatted p value
#' @export
#'
#' @examples
#' p_values <- 10 ^ seq(-1, -6, -1)
#' # Format p values as for example 1.0x10^-4
#' pretty_p(p_values, p_digits = 3, small_p_format = "x10")
#' # Format p values for use on a graph
#' x <- rlnorm(100, 3, 1)
#' y <- rlnorm(100, 4, 1)
#' p <- wilcox.test(x, y)$p.value
#' p_formatted <- pretty_p(p, p_digits = 3, small_p_format = "plotmath")
#' boxplot(x, y, ylim = c(0, max(c(x, y)) + 100))
#' text(1.5, max(c(x, y) + 50), parse(text = paste0("p == ", p_formatted)))
pretty_p <- function(p,
                     p_digits,
                     small_p_format = c("<", "E", "x10", "plotmath", "html"),
                     small_p_cutoff = 10^-p_digits,
                     sig_fig = FALSE,
                     n_sig_fig = 2
) {
  small_p_format <- match.arg(small_p_format)
  if (small_p_format == "<") {
    small_p_func <- function(p, small_p_cutoff) {
      sprintf("<%.*f", p_digits, small_p_cutoff)
    }
  } else if (small_p_format == "E") {
    small_p_func <- function(p, small_p_cutoff) {
      sprintf("%.1E", p)
    }
  } else if (small_p_format == "x10") {
    small_p_func <- function(p, small_p_cutoff) {
      sub("E(-?)\\+?0?(\\d+)", "x10^\\1\\2", sprintf("%.1E", p))
    }
  } else if (small_p_format == "plotmath") {
    small_p_func <- function(p, small_p_cutoff) {
      sub("E(-?)\\+?0?(\\d+)", " %*% 10^\\1\\2", sprintf("%.1E", p))
    }
  } else if (small_p_format == "html") {
    small_p_func <- function(p, small_p_cutoff) {
      sub("E(-?)\\+?0?(\\d+)", " &times; 10<sup>\\1\\2</sup>", sprintf("%.1E", p))
    }
  }
  
  ifelse(
    is.na(p) | p == "",
    "",
    ifelse(
      p >= small_p_cutoff,
      sprintf(
        "%.*f",
        ifelse(
          sig_fig & !is.na(p) & p != "" & p > 0,
          n_sig_fig - 1 - floor(log10(signif(p, n_sig_fig))),
          p_digits
        ),
        p
      ),
      small_p_func(p, small_p_cutoff)
    )
  )
}

lighten <- function(colours) {
  col2rgb(colours) %>%
    rgb2hsv() %>%
    {.["s", ] <- .["s", ] - 0.5; .} %>%
    t() %>%
    as.data.frame() %>%
    do.call(hsv, .)
}
