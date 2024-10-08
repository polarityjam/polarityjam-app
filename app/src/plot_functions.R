# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Polarity JaM: Shiny app for plotting and comparing polarity data
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Takes spreadsheet type data as input with circular and non-circular features
# Visualization of circular and non-circular distributions
# Comparative circular statistics
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MIT License
#
# Copyright (c) 2021 Wolfgang Giese
# electronic mail address: wolfgang #dot# giese #at# mdc #minus# berlin #dot# de
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


transform_axial <- function(input, feature_axial) {
  feature_axial <- unlist(feature_axial)
  feature_transformed <- feature_axial

  if (input$hemi_rose_options == "left") {
    for (i in 1:length(feature_axial)) {
      if (feature_axial[i] < pi / 2.0) {
        feature_transformed[i] <- feature_axial[i] + pi
      } else {
        feature_transformed[i] <- feature_axial[i]
      }
    }
  }

  return(feature_transformed)
}

select_color <- function(parameters, input, plot_nr) {
  color <- "black"

  if (input$select_colormap == "Okabe_Ito") {
    if (plot_nr == 0) {
      color_n <- input$select_color %% length(Okabe_Ito)
    } else {
      color_n <- 1 + (plot_nr + input$select_color) %% length(Okabe_Ito)
    }

    color <- Okabe_Ito[color_n]
  } else if (input$select_colormap == "Tol_bright") {
    if (plot_nr == 0) {
      color_n <- input$select_color %% length(Tol_bright)
    } else {
      color_n <- 1 + (plot_nr + input$select_color) %% length(Tol_bright)
    }

    color <- Tol_bright[color_n]
  } else if (input$select_colormap == "Tol_muted") {
    if (plot_nr == 0) {
      color_n <- input$select_color %% length(Tol_muted)
    } else {
      color_n <- 1 + (plot_nr + input$select_color) %% length(Tol_muted)
    }

    color <- Tol_muted[color_n]
  } else if (input$select_colormap == "Tol_light") {
    if (plot_nr == 0) {
      color_n <- input$select_color %% length(Tol_light)
    } else {
      color_n <- 1 + (plot_nr + input$select_color) %% length(Tol_light)
    }

    color <- Tol_light[color_n]
  }

  return(color)
}

rose_plot_circular <- function(parameters, input, statistics, feature_circular, plot_title, plot_nr = 0, text_size = 24) {

  bin_size <- 360 / input$bins
  
  #statistics <- as.data.frame(t(statistics))

  polarity_index <- signif(statistics[1, "polarity_index"], digits = 3)
  v_score <- signif(statistics[1, "V_score"], digits = 3)

  # Tests: "Rayleigh uniform", "V-Test", "Rao's Test", "Watson's Test"

  if (input$stats_method == "Rayleigh uniform") {
    p_value_ <- signif(statistics[1, "rayleigh_test"], digits = 3)
    if (statistics[1, "rayleigh_test"] < 0.001) {
      p_value <- "P < 0.001"
    } else {
      p_value <- paste0("P = ", toString(p_value_))
    }
  }

  if (input$stats_method == "V-Test") {
    p_value_ <- signif(statistics[1, "v_test"], digits = 3)
  }

  if (input$stats_method == "Watson's Test") {
    p_value <- statistics[1, "watson_test"]
  }

  if (input$stats_method == "Rao's Test") {
    p_value <- statistics[1, "rao_test"]
  }

  p <- ggplot()

  color_fill <- select_color(parameters, input, plot_nr)
  print("using color: ")
  print(color_fill)

  color <- color_fill

  alpha <- 0.5
  if (input$adjust_alpha == TRUE) {
    alpha <- input$alpha_fill
    if (input$outline != "color") {
      color <- input$outline
    }
  }

  #if (input$kde_plot) {
  #  p <- p +
  #    geom_density(aes(x = feature_circular, y = ..count.. / max(..count..)),
  #      color = color,
  #      fill = color_fill,
  #      alpha = alpha
  #    )
  #}

  if (input$histogram_plot) {
    p <- p +
      geom_histogram(aes(x = feature_circular, y = ..ncount..),
        breaks = seq(0, 360, bin_size),
        color = color,
        fill = color_fill,
        alpha = alpha
      )
  }
  
  if (input$scatter_plot) {
    print("plot points")
    p <- p + geom_point(aes(x = feature_circular, y = 1), size = input$marker_size, alpha = 0.5)
    #p <- p + geom_jitter(aes(x = feature_circular, y = 1), size = input$marker_size)
  }

  p <- p + ggtitle(plot_title) +
    theme(plot.title = element_text(size = 18, face = "bold")) +
    theme(axis.text.x = element_text(size = 18)) +
    coord_polar(start = -pi / 2.0, direction = -1) +
    scale_x_continuous(
      limits = c(0, 360),
      breaks = (c(0, 90, 180, 270))
    ) +
    scale_y_continuous(limits = c(0, 1.1)) +
    theme_minimal(base_size = text_size) +
    # xlab(sprintf("number of cells = : %s \n polarity index: %s, %s, \n condition: %s", length(feature_circular), polarity_index, p_value, input$exp_condition)) +
    ylab("polarity index")
  # theme(axis.text.y=element_blank()) +


  if (input$stats_method != "None") {
    p <- p + xlab(sprintf("N = %s \n polarity index: %s, %s", length(feature_circular), polarity_index, p_value))
  } else if (input$plot_polar_direction) {
    p <- p + xlab(sprintf("N = %s \n polarity index: %s, \n V-score: %s", length(feature_circular), polarity_index, v_score))
  } else {
    p <- p + xlab(sprintf("N = %s \n polarity index: %s", length(feature_circular), polarity_index))
  }

  if (input$area_scaled) {
    p <- p + scale_y_sqrt(limits = c(0, sqrt(1.1))) ## + scale_y_continuous(limits = c(0, sqrt(1.1)))
  }
  
  if (input$plot_polar_direction) {
    mu0 <- input$cond_mean_direction
    p <- p + geom_segment(data = statistics, aes(x = mu0, y = 0, xend = mu0, yend = 1.0), size = 1.5, color = "gray", arrow = arrow()) + theme(legend.position = "none")
  }

  if (input$plot_PI) {
    p <- p + geom_segment(data = statistics, aes(x = mean, y = 0, xend = mean, yend = polarity_index,  size = 3.0, color = "red"), arrow = arrow()) + theme(legend.position = "none")   
  }
  
  if (input$plot_polar_direction) {
    v_score_ <- abs(v_score)
    mu0_ <- mu0
    if (v_score < 0.0) mu0_ <- mu0 - 180.0
    if (mu0_ < 0.0) mu0_ <- mu0_ + 360.0
    p <- p + geom_segment(data = statistics, aes(x = mu0_, y = 0, xend = mu0_, yend = v_score_), size = 3.0, color = "black", lineend = "square") + theme(legend.position = "none")#arrow = NULL, 
  }

  if (input$ci_plot) {
    if (input$ci_method == "95% CI of the mean") {
      p <- p + geom_segment(data = statistics, aes(x = ci_95_lower_limit, y = 0, xend = ci_95_lower_limit, yend = 1), size = 1.5, color = "red", linetype = "dashed", arrow = NULL) + theme(legend.position = "none")
      p <- p + geom_segment(data = statistics, aes(x = ci_95_upper_limit, y = 0, xend = ci_95_upper_limit, yend = 1), size = 1.5, color = "red", linetype = "dashed", arrow = NULL) + theme(legend.position = "none")
    }
    if (input$ci_method == "90% CI of the mean") {
      p <- p + geom_segment(data = statistics, aes(x = ci_90_lower_limit, y = 0, xend = ci_90_lower_limit, yend = 1), size = 1.5, color = "red", linetype = "dashed", arrow = NULL) + theme(legend.position = "none")
      p <- p + geom_segment(data = statistics, aes(x = ci_90_upper_limit, y = 0, xend = ci_90_upper_limit, yend = 1), size = 1.5, color = "red", linetype = "dashed", arrow = NULL) + theme(legend.position = "none")
    }
    if (input$ci_method == "50% CI of the mean") {
      p <- p + geom_segment(data = statistics, aes(x = ci_50_lower_limit, y = 0, xend = ci_50_lower_limit, yend = 1), size = 1.5, color = "red", linetype = "dashed", arrow = NULL) + theme(legend.position = "none")
      p <- p + geom_segment(data = statistics, aes(x = ci_50_upper_limit, y = 0, xend = ci_50_upper_limit, yend = 1), size = 1.5, color = "red", linetype = "dashed", arrow = NULL) + theme(legend.position = "none")
    }
    if (input$ci_method == "circular standard deviation") {
      p <- p + geom_segment(data = statistics, aes(x = std_circ_low_lim, y = 0, xend = std_circ_low_lim, yend = 1), size = 1.5, color = "red", linetype = "dashed", arrow = NULL) + theme(legend.position = "none")
      p <- p + geom_segment(data = statistics, aes(x = std_circ_up_lim, y = 0, xend = std_circ_up_lim, yend = 1), size = 1.5, color = "red", linetype = "dashed", arrow = NULL) + theme(legend.position = "none")
    }
    if (input$ci_method == "angular standard deviation") {
      p <- p + geom_segment(data = statistics, aes(x = std_ang_low_lim, y = 0, xend = std_ang_low_lim, yend = 1), size = 1.5, color = "red", linetype = "dashed", arrow = NULL) + theme(legend.position = "none")
      p <- p + geom_segment(data = statistics, aes(x = std_ang_up_lim, y = 0, xend = std_ang_up_lim, yend = 1), size = 1.5, color = "red", linetype = "dashed", arrow = NULL) + theme(legend.position = "none")
    }
  }

  return(p)
}

compare_plot_circular <- function(parameters, input, statistics, feature_circular_1, feature_circular_2, plot_title, text_size = 24) {
  bin_size <- 360 / input$bins_comparison

  polarity_index <- signif(statistics[1, "polarity_index"], digits = 3)
  p_value_ <- signif(statistics[1, "rayleigh_test"], digits = 3)
  if (statistics[1, "rayleigh_test"] < 0.001) {
    p_value <- "p < 0.001"
  } else {
    p_value <- paste0("p = ", toString(p_value_))
  }

  p <- ggplot()

  if (input$histogram_comparison) {
    p <- p + geom_histogram(aes(x = feature_circular_1, y = ..density..), # , y = ..ncount..),
      breaks = seq(0, 360, bin_size),
      color = "black",
      fill = "blue",
      alpha = 0.5
    ) +
      geom_histogram(aes(x = feature_circular_2, y = ..density..), # , y = ..ncount..),
        breaks = seq(0, 360, bin_size),
        color = "black",
        fill = "red",
        alpha = 0.5
      )
  }

  if (input$kde_comparison) {
    p <- p + geom_density(aes(x = feature_circular_1, y = ..density..),
      color = "black",
      fill = "red",
      alpha = 0.5
    ) +
      geom_density(aes(x = feature_circular_2, y = ..density..),
        color = "black",
        fill = "blue",
        alpha = 0.5
      )
  }

  p <- p + ggtitle(plot_title) +
    theme(axis.text.x = element_text(size = 18)) +
    coord_polar(start = -pi / 2.0, direction = -1) +
    scale_x_continuous(
      limits = c(0, 360),
      breaks = (c(0, 90, 180, 270))
    ) +
    theme_minimal(base_size = text_size) +
    xlab(sprintf("N = %s \n polarity index: %s, %s", length(feature_circular_1), polarity_index, p_value)) +
    ylab("polarity index")
  
  if (input$area_scaled) {
    p <- p + scale_y_sqrt()
  }

  # p <- p + geom_segment(data = statistics, aes(x=mean, y=0, xend=mean, yend=polarity_index, size = 1.5, color="red", lineend = "butt"), arrow = arrow()) + theme(legend.position = "none")

  return(p)
}




rose_plot_axial <- function(parameters, input, statistics, feature_circular, plot_title, plot_nr = 0, text_size = 24) {
  bin_size <- 360 / input$bins

  polarity_index <- signif(statistics[1, "polarity_index"], digits = 3)
  v_score <- signif(statistics[1, "V_score"], digits = 3)
  #v_proj <- signif(statistics[1, "V_proj"], digits = 3)
  
  p_value_ <- signif(statistics[1, "rayleigh_test"], digits = 3)
  if (statistics[1, "rayleigh_test"] < 0.001) {
    p_value <- "p < 0.001"
  } else {
    p_value <- paste0("p = ", toString(p_value_))
  }

  if (input$hemi_rose_options != "mirrored") {
    feature_circular_ <- feature_circular
  } else {
    n <- length(feature_circular)
    feature_circular_ <- numeric(2 * n)
    for (i in 1:n) {
      feature_circular_[i] <- feature_circular[i]
      feature_circular_[i + n] <- feature_circular[i] - 180.0
    }
  }

  color_fill <- select_color(parameters, input, plot_nr)
  color <- color_fill
  alpha <- 0.5
  if (input$adjust_alpha == TRUE) {
    alpha <- input$alpha_fill
    if (input$outline != "color") {
      color <- input$outline
    }
  }

  p <- ggplot()

  #if (input$kde_plot) {
  #  p <- p +
  #    geom_density(aes(x = feature_circular_, y = ..count.. / max(..count..)),
  #                 color = color,
  #                 fill = color_fill,
  #                 alpha = alpha
  #    )
  #}

  if (input$histogram_plot) {
    p <- p +
      geom_histogram(aes(x = feature_circular_, y = ..ncount..),
                     #breaks = seq(0, 360, bin_size),  # TODO adapt
                     breaks = seq(-180, 180, bin_size),  # TODO adapt
                     color = color,
                     fill = color_fill,
                     alpha = alpha
      )
  }

  if (input$scatter_plot) {
    print("plot points")
    p <- p + geom_point(aes(x = feature_circular_, y = 1), size = input$marker_size, alpha = 0.5)
  }
  
  p <- p + ggtitle(plot_title) +
    ggtitle(plot_title) +
    theme(plot.title = element_text(size = 10, face = "bold")) +
    theme(axis.text.x = element_text(size = 18)) +
    #coord_polar(start = -pi / 2.0, direction = -1) +
    coord_polar(start = pi / 2.0, direction = -1) +
    scale_x_continuous(
      # limits = c(0, 360), # TODO Adapt
      # breaks = (c(0, 90, 180, 270)) # TODO Adapt
      limits = c(-180, 180), # TODO Adapt
      breaks = (c(0, 45, 90, 135, 180, -45, -90, -135)) # TODO Adapt
    ) +
    theme_minimal(base_size = text_size) +
    #        xlab(sprintf("number of cells = : %s \n polarity index: %s, %s, \n condition: %s" , length(feature_circular), polarity_index, p_value, input$exp_condition)) +
    ylab("polarity index")
  # theme(axis.text.y=element_blank()) +

  if (input$stats_method != "None") {
    p <- p + xlab(sprintf("N = %s \n polarity index: %s, %s", length(feature_circular), polarity_index, p_value))
  } else if (input$plot_polar_direction) {
    p <- p + xlab(sprintf("N = %s \n polarity index: %s, \n V-score: %s", length(feature_circular), polarity_index, v_score))
  } else {
    p <- p + xlab(sprintf("N = %s \n polarity index: %s", length(feature_circular), polarity_index))
  }

  if (input$area_scaled) {
    p <- p + scale_y_sqrt()
  }
  
  
  

  if (input$hemi_rose_options == "left") {
    # if (input$left_directional) {
    print(statistics[1, "mean"])
    if (statistics[1, "mean"] < 90.0) {
      statistics[1, "mean"] <- statistics[1, "mean"] + 180.0
    }
    if (statistics[1, "ci_95_lower_limit"] < 90.0) {
      statistics[1, "ci_95_lower_limit"] <- statistics[1, "ci_95_lower_limit"] + 180.0
    }
    if (statistics[1, "ci_95_upper_limit"] < 90.0) {
      statistics[1, "ci_95_upper_limit"] <- statistics[1, "ci_95_upper_limit"] + 180.0
    }
    if (statistics[1, "ci_90_lower_limit"] < 90.0) {
      statistics[1, "ci_90_lower_limit"] <- statistics[1, "ci_90_lower_limit"] + 180.0
    }
    if (statistics[1, "ci_50_upper_limit"] < 90.0) {
      statistics[1, "ci_50_upper_limit"] <- statistics[1, "ci_50_upper_limit"] + 180.0
    }
  }
  
  if (input$plot_polar_direction) {
    mu0 <- input$cond_mean_direction
    p <- p + geom_segment(data = statistics, aes(x = mu0, y = 0, xend = mu0, yend = 1.0), size = 1.5, color = "gray", arrow = NULL) + theme(legend.position = "none")
    v_score_ <- abs(v_score)
    p <- p + geom_segment(data = statistics, aes(x = mu0, y = 0, xend = mu0, yend = v_score_), size = 3.0, color = "black", lineend = "square") + theme(legend.position = "none")#arrow = NULL, 
  }



  if (input$plot_PI) {
    p <- p + geom_segment(data = statistics, aes(x = mean, y = 0, xend = mean, yend = polarity_index, size = 1.5, color = "red", lineend = "butt"), arrow = NULL) + theme(legend.position = "none")
  }
  

  
    
  if (input$ci_plot) {
    # p <- p + geom_segment(data = statistics, aes(x=ci_lower_limit, y=0, xend=ci_lower_limit, yend=1), size = 1.5, color="red",linetype = "dashed", arrow = NULL) + theme(legend.position = "none")
    # p <- p + geom_segment(data = statistics, aes(x=ci_upper_limit, y=0, xend=ci_upper_limit, yend=1), size = 1.5, color="red",linetype = "dashed", arrow = NULL) + theme(legend.position = "none")
    if (input$ci_method == "95% CI of the mean") {
      p <- p + geom_segment(data = statistics, aes(x = ci_95_lower_limit, y = 0, xend = ci_95_lower_limit, yend = 1), size = 1.5, color = "red", linetype = "dashed", arrow = NULL) + theme(legend.position = "none")
      p <- p + geom_segment(data = statistics, aes(x = ci_95_upper_limit, y = 0, xend = ci_95_upper_limit, yend = 1), size = 1.5, color = "red", linetype = "dashed", arrow = NULL) + theme(legend.position = "none")
    }
    if (input$ci_method == "90% CI of the mean") {
      p <- p + geom_segment(data = statistics, aes(x = ci_90_lower_limit, y = 0, xend = ci_90_lower_limit, yend = 1), size = 1.5, color = "red", linetype = "dashed", arrow = NULL) + theme(legend.position = "none")
      p <- p + geom_segment(data = statistics, aes(x = ci_90_upper_limit, y = 0, xend = ci_90_upper_limit, yend = 1), size = 1.5, color = "red", linetype = "dashed", arrow = NULL) + theme(legend.position = "none")
    }
    if (input$ci_method == "50% CI of the mean") {
      p <- p + geom_segment(data = statistics, aes(x = ci_50_lower_limit, y = 0, xend = ci_50_lower_limit, yend = 1), size = 1.5, color = "red", linetype = "dashed", arrow = NULL) + theme(legend.position = "none")
      p <- p + geom_segment(data = statistics, aes(x = ci_50_upper_limit, y = 0, xend = ci_50_upper_limit, yend = 1), size = 1.5, color = "red", linetype = "dashed", arrow = NULL) + theme(legend.position = "none")
    }
    if (input$ci_method == "circular standard deviation") {
      p <- p + geom_segment(data = statistics, aes(x = std_circ_low_lim, y = 0, xend = std_circ_low_lim, yend = 1), size = 1.5, color = "red", linetype = "dashed", arrow = NULL) + theme(legend.position = "none")
      p <- p + geom_segment(data = statistics, aes(x = std_circ_up_lim, y = 0, xend = std_circ_up_lim, yend = 1), size = 1.5, color = "red", linetype = "dashed", arrow = NULL) + theme(legend.position = "none")
    }
    if (input$ci_method == "angular standard deviation") {
      p <- p + geom_segment(data = statistics, aes(x = std_ang_low_lim, y = 0, xend = std_ang_low_lim, yend = 1), size = 1.5, color = "red", linetype = "dashed", arrow = NULL) + theme(legend.position = "none")
      p <- p + geom_segment(data = statistics, aes(x = std_ang_up_lim, y = 0, xend = std_ang_up_lim, yend = 1), size = 1.5, color = "red", linetype = "dashed", arrow = NULL) + theme(legend.position = "none")
    }
  }


  if (input$hemi_rose_options == "mirrored") {
    statistics[1, "mean"] <- statistics[1, "mean"] - 180.0
    statistics[1, "mu0"] <- statistics[1, "mu0"] - 180.0
    statistics[1, "ci_95_lower_limit"] <- statistics[1, "ci_95_lower_limit"] - 180.0
    statistics[1, "ci_95_upper_limit"] <- statistics[1, "ci_95_upper_limit"] - 180.0
    statistics[1, "ci_90_lower_limit"] <- statistics[1, "ci_90_lower_limit"] - 180.0
    statistics[1, "ci_90_upper_limit"] <- statistics[1, "ci_90_upper_limit"] - 180.0
    statistics[1, "ci_50_lower_limit"] <- statistics[1, "ci_50_lower_limit"] - 180.0
    statistics[1, "ci_50_upper_limit"] <- statistics[1, "ci_50_upper_limit"] - 180.0
    statistics[1, "std_circ_low_lim"] <- statistics[1, "std_circ_low_lim"] - 180.0
    statistics[1, "std_circ_up_lim"] <- statistics[1, "std_circ_up_lim"] - 180.0
    statistics[1, "std_ang_low_lim"] <- statistics[1, "std_ang_low_lim"] - 180.0
    statistics[1, "std_ang_up_lim"] <- statistics[1, "std_ang_up_lim"] - 180.0

    if (input$plot_polar_direction) {
      #mu0 <- input$cond_mean_direction + 180
      p <- p + geom_segment(data = statistics, aes(x = mu0, y = 0, xend = mu0, yend = 1.0), size = 1.5, color = "gray", arrow = NULL) + theme(legend.position = "none")
      v_score_ <- abs(v_score)
      p <- p + geom_segment(data = statistics, aes(x = mu0, y = 0, xend = mu0, yend = v_score_), size = 3.0, color = "black", lineend = "square") + theme(legend.position = "none")#arrow = NULL, 
    }
    
    if (input$plot_PI) {
      p <- p + geom_segment(data = statistics, aes(x = mean, y = 0, xend = mean, yend = polarity_index, size = 1.5, color = "red", lineend = "butt"), arrow = NULL) + theme(legend.position = "none")
    }
    
    if (input$ci_plot) {
      if (input$ci_method == "95% CI of the mean") {
        p <- p + geom_segment(data = statistics, aes(x = ci_95_lower_limit, y = 0, xend = ci_95_lower_limit, yend = 1), size = 1.5, color = "red", linetype = "dashed", arrow = NULL) + theme(legend.position = "none")
        p <- p + geom_segment(data = statistics, aes(x = ci_95_upper_limit, y = 0, xend = ci_95_upper_limit, yend = 1), size = 1.5, color = "red", linetype = "dashed", arrow = NULL) + theme(legend.position = "none")
        #p <- p + geom_segment(data = statistics, aes(x = ci_95_lower_limit, y = 0.3, xend = mean, yend = 0.3), size = 1.5, color = "red", arrow = NULL) + theme(legend.position = "none")
        #p <- p + geom_segment(data = statistics, aes(x = mean, y = 0.3, xend = ci_95_upper_limit, yend = 0.3), size = 1.5, color = "red", arrow = NULL) + theme(legend.position = "none")
      }
      if (input$ci_method == "90% CI of the mean") {
        p <- p + geom_segment(data = statistics, aes(x = ci_90_lower_limit, y = 0, xend = ci_90_lower_limit, yend = 1), size = 1.5, color = "red", linetype = "dashed", arrow = NULL) + theme(legend.position = "none")
        p <- p + geom_segment(data = statistics, aes(x = ci_90_upper_limit, y = 0, xend = ci_90_upper_limit, yend = 1), size = 1.5, color = "red", linetype = "dashed", arrow = NULL) + theme(legend.position = "none")
      }
      if (input$ci_method == "50% CI of the mean") {
        p <- p + geom_segment(data = statistics, aes(x = ci_50_lower_limit, y = 0, xend = ci_50_lower_limit, yend = 1), size = 1.5, color = "red", linetype = "dashed", arrow = NULL) + theme(legend.position = "none")
        p <- p + geom_segment(data = statistics, aes(x = ci_50_upper_limit, y = 0, xend = ci_50_upper_limit, yend = 1), size = 1.5, color = "red", linetype = "dashed", arrow = NULL) + theme(legend.position = "none")
      }
      if (input$ci_method == "circular standard deviation") {
        p <- p + geom_segment(data = statistics, aes(x = std_circ_low_lim, y = 0, xend = std_circ_low_lim, yend = 1), size = 1.5, color = "red", linetype = "dashed", arrow = NULL) + theme(legend.position = "none")
        p <- p + geom_segment(data = statistics, aes(x = std_circ_up_lim, y = 0, xend = std_circ_up_lim, yend = 1), size = 1.5, color = "red", linetype = "dashed", arrow = NULL) + theme(legend.position = "none")
      }
      if (input$ci_method == "angular standard deviation") {
        p <- p + geom_segment(data = statistics, aes(x = std_ang_low_lim, y = 0, xend = std_ang_low_lim, yend = 1), size = 1.5, color = "red", linetype = "dashed", arrow = NULL) + theme(legend.position = "none")
        p <- p + geom_segment(data = statistics, aes(x = std_ang_up_lim, y = 0, xend = std_ang_up_lim, yend = 1), size = 1.5, color = "red", linetype = "dashed", arrow = NULL) + theme(legend.position = "none")
      }


      # p <- p + geom_segment(data = statistics, aes(x=ci_lower_limit, y=0, xend=ci_lower_limit, yend=1), size = 1.5, color="red",linetype = "dashed", arrow = NULL) + theme(legend.position = "none")
      # p <- p + geom_segment(data = statistics, aes(x=upper_limit, y=0, xend=upper_limit, yend=1), size = 1.5, color="red",linetype = "dashed", arrow = NULL) + theme(legend.position = "none")
    }
  }

  return(p)
}

compare_plot_axial <- function(parameters, input, statistics, feature_circular, plot_title, text_size = 24) {
  bin_size <- 360 / input$bins
  # plot_title <- parameters[input$feature_select][[1]][3]

  polarity_index <- signif(statistics[1, "polarity_index"], digits = 3)
  p_value_ <- signif(statistics[1, "rayleigh_test"], digits = 3)
  if (statistics[1, "rayleigh_test"] < 0.001) {
    p_value <- "p < 0.001"
  } else {
    p_value <- paste0("p = ", toString(p_value_))
  }

  p <- ggplot() +
    geom_histogram(aes(x = feature_circular, y = ..ncount..),
      breaks = seq(0, 360, bin_size),
      color = "black",
      fill = "black",
      alpha = 0.5
    ) +
    #        geom_density(aes(x = feature_circular)) +
    ggtitle(plot_title) +
    theme(axis.text.x = element_text(size = 18)) +
    coord_polar(start = -pi / 2.0, direction = -1) +
    scale_x_continuous(
      limits = c(0, 360),
      breaks = (c(0, 90, 180, 270))
    ) +
    theme_minimal(base_size = text_size) +
    xlab(sprintf("number of cells = : %s \n polarity index: %s, %s", length(feature_circular), polarity_index, p_value)) +
    ylab("polarity index")
  # theme(axis.text.y=element_blank()) +

  if (input$area_scaled) {
    p <- p + scale_y_sqrt()
  }

  if (input$left_directional) {
    print(statistics[1, "mean"])
    if (statistics[1, "mean"] < 90.0) {
      statistics[1, "mean"] <- statistics[1, "mean"] + 180.0
    }
  }


  p <- p + geom_segment(data = statistics, aes(x = mean, y = 0, xend = mean, yend = polarity_index, size = 1.5, color = "red", lineend = "butt"), arrow = NULL) + theme(legend.position = "none")
  return(p)
}

linear_histogram <- function(parameters, input, statistics, feature_linear, plot_title, plot_nr = 0, text_size = 24, x_start = 0, x_end = 1.0) {
  bin_size <- max(feature_linear) / input$bins
  # plot_title <- parameters[input$feature_select][[1]][3]

  p <- ggplot()

  color_fill <- select_color(parameters, input, plot_nr)
  color <- color_fill

  alpha <- 0.5
  if (input$adjust_alpha == TRUE) {
    alpha <- input$alpha_fill
    if (input$outline != "color") {
      color <- input$outline
    }
  }



  if (input$histogram_plot) {
    p <- p + geom_histogram(aes(x = feature_linear, y = ..density..),
      breaks = seq(0, max(feature_linear), bin_size),
      color = color,
      fill = color_fill,
      alpha = 0.5
    )
  }

  #if (input$kde_plot) {
  #  p <- p + geom_density(aes(x = feature_linear, y = ..density..),
  #    color = color,
  #    fill = color_fill,
  #    alpha = 0.5
  #  )
  #}

  p <- p + ggtitle(plot_title) +
    theme(axis.text.x = element_text(size = 18)) +
    theme_minimal(base_size = text_size) +
    xlab(sprintf("number of cells = : %s", length(feature_linear)))

  p <- p + geom_vline(data = statistics, aes(xintercept = mean), col = "red", size = 2)
  p <- p + xlim(c(x_start, x_end))
  return(p)
}

# linear_histogram <- function(parameters, input, statistics, feature_linear, plot_title, plot_nr = 0, text_size = 24) {
#
#
#    bin_size = max(feature_linear)/input$bins
# plot_title <- parameters[input$feature_select][[1]][3]
##
#    p <- ggplot()
#
#    if (input$histogram_plot) {
#        p <- p + geom_histogram(aes(x = feature_linear, y = ..density..),
#                   breaks = seq(0, max(feature_linear), bin_size),
#                   color = "black",
#                   fill = "black",
#                   alpha = 0.5)
#    }

#    if (input$kde_plot) {
#        p <- p + geom_density(aes(x = feature_linear, y = ..density..),
#                   color = "black",
#                   fill = "black",
#                   alpha = 0.5)
#
#    }

#    p <- p + ggtitle(plot_title) +
#        theme(axis.text.x = element_text(size = 18)) +
#        theme_minimal(base_size = text_size) +
#        xlab(sprintf("number of cells = : %s \n condition: %s", length(feature_linear), input$exp_condition))
#
#    p <- p + geom_vline(data = statistics, aes(xintercept=mean),  col="red", size = 2)
#    return(p)
# }

compare_plot_linear <- function(parameters, input, statistics, feature_linear_1, feature_linear_2, plot_title, text_size = 24) {

  # plot_title <- parameters[input$feature_select][[1]][3]

  p <- ggplot()
  max_1 <- max(feature_linear_1)
  max_2 <- max(feature_linear_2)

  # df_1 <- as.data

  bin_size <- max(max_1, max_2) / input$bins

  if (input$histogram_plot) {
    p <- p + geom_histogram(aes(x = feature_linear_1, y = ..density..),
      breaks = seq(0, max(max_1, max_2), bin_size),
      color = "black",
      fill = "black",
      alpha = 0.5
    ) +
      geom_histogram(aes(x = feature_linear_2, y = ..density..),
        breaks = seq(0, max(max_1, max_2), bin_size),
        color = "black",
        fill = "black",
        alpha = 0.5
      )
  }

  if (input$kde_plot) {
    p <- p + geom_density(aes(x = feature_linear_1, y = ..density..),
      color = "black",
      fill = "black",
      alpha = 0.5
    ) +
      geom_density(aes(x = feature_linear_1, y = ..density..),
        color = "black",
        fill = "black",
        alpha = 0.5
      )
  }

  p <- p + ggtitle(plot_title) +
    theme(axis.text.x = element_text(size = 18)) +
    theme_minimal(base_size = text_size) +
    xlab(sprintf("number of cells = : %s \n condition: %s", length(feature_linear_1), input$exp_condition))

  p <- p + geom_vline(data = statistics, aes(xintercept = mean), col = "red", size = 2)
  return(p)
}
