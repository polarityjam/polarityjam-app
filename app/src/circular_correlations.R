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


# rename to correlation
plot_circular_circular <- function(correlation_data, input, parameters, plot_title, plot_nr = 0, text_size = 24) {
  
  source(file = paste0(getwd(), "/src/plot_functions.R"), local = T)
  
  # TODO: consider the case that the feature is not in the parameters file
  feature_1 <- parameters[input$feature_select_1][[1]][1]
  feature_2 <- parameters[input$feature_select_2][[1]][1]
  feature_1_values <- unlist(correlation_data[feature_1])
  feature_2_values <- unlist(correlation_data[feature_2])

  feature_1_name <- parameters[input$feature_select_1][[1]][3]
  feature_2_name <- parameters[input$feature_select_2][[1]][3]

  mode_1 <- parameters[input$feature_select_1][[1]][2]
  mode_2 <- parameters[input$feature_select_2][[1]][2]
  

  # TODO: use stats function to convert to degrees
  if (mode_1 != "linear") {
    feature_1_values_ <- correlation_data[feature_1] * 180.0 / pi
  } else {
    feature_1_values_ <- correlation_data[feature_1]
  }
  
  #
  if (mode_2 != "linear") {
    feature_2_values_ <- correlation_data[feature_2] * 180.0 / pi
  } else {
    feature_2_values_ <- correlation_data[feature_2]
  }
  
  #
  color <- select_color(parameters, input, plot_nr)
    
  conditions <- correlation_data[input$condition_col]
  
  condition_list <- unlist(unique(correlation_data[input$condition_col]))
  color_palette <- list(select_color(parameters, input, 1))
  
  for (i in 2:length(condition_list)) {
    color_palette <- append(color_palette, select_color(parameters, input, i))
  }

  mean_dir_1 <- compute_mean_circular(feature_1_values, mode_1)
  mean_dir_2 <- compute_mean_circular(feature_2_values, mode_2)

  res <- compute_correlation(feature_1_values, mode_1, feature_2_values, mode_2)

  # center correlation plot either to  either 0 or
  # otherwise pi/2 (90 degrees) or pi (180 degrees) 
  # for axial or directional features, respectively
  
  if (input$center_corr_plot == TRUE) {
    if (mode_1 == "directional") {
      if ((mean_dir_1 < pi / 2.0) | (mean_dir_1 > 3.0 * pi / 2.0)) {
        for (i in 1:length(feature_2_values)) {
          if (feature_1_values[i] > pi) {
            correlation_data[i, feature_1] <- correlation_data[i, feature_1] - 2.0 * pi
          }
        }
        feature_1_values_ <- correlation_data[feature_1] * 180.0 / pi
      }
    } else if (mode_1 == "axial") {
      if ((mean_dir_1 < pi / 4.0) | (mean_dir_1 > 3.0 * pi / 4.0)) {
        for (i in 1:length(feature_2_values)) {
          if (feature_1_values[i] > pi / 2.0) {
            correlation_data[i, feature_1] <- correlation_data[i, feature_1] - pi
          }
        }
        feature_1_values_ <- correlation_data[feature_1] * 180.0 / pi
      }
    }

    if (mode_2 == "directional") {
      if ((mean_dir_2 < pi / 2.0) | (mean_dir_2 > 3.0 * pi / 2.0)) {
        for (i in 1:length(feature_2_values)) {
          if (feature_2_values[i] > pi) {
            correlation_data[i, feature_2] <- correlation_data[i, feature_2] - 2.0 * pi
          }
        }
        feature_2_values_ <- correlation_data[feature_2] * 180.0 / pi
      }
    } else if (mode_2 == "axial") {
      if ((mean_dir_2 < pi / 4.0) | (mean_dir_2 > 3.0 * pi / 4.0)) {
        for (i in 1:length(feature_2_values)) {
          if (feature_2_values[i] > pi / 2.0) {
            correlation_data[i, feature_2] <- correlation_data[i, feature_2] - pi
          }
        }
        feature_2_values_ <- correlation_data[feature_2] * 180.0 / pi
      }
    }
  }

  plot_df <- as.data.frame(c(feature_1_values_, feature_2_values_, conditions))

  colnames(plot_df) <- c("x", "y", "condition")
  
  
  if (plot_nr == 0) {
    #Okabe_Ito
    if (input$select_colormap == "gray") {
      p <- ggplot(plot_df, aes(x = x, y = y)) +
        geom_point(size = input$marker_size_corr, alpha=input$marker_alpha_corr) +
        theme_minimal(base_size = text_size)
    } else {
      
      
      #TODO: add colorblind friendly palette here
      p <- ggplot(plot_df, aes(x = x, y = y, color = condition)) +
        geom_point(size = input$marker_size_corr, alpha=input$marker_alpha_corr) +
        theme_minimal(base_size = text_size) +
        scale_colour_manual(values=color_palette) 
    }#+
      #scale_colour_manual(values=Okabe_Ito) # theme_bw()
  } else {
    p <- ggplot(plot_df, aes(x = x, y = y)) +
      geom_point(size = input$marker_size_corr, color = color, alpha=input$marker_alpha_corr) +
      theme_minimal(base_size = text_size) 

      # theme_bw()
  }  

  #if (mode_1 == 'directional') { p <- p + xlim(0,360) }
  #if (mode_1 == 'axial') { p <- p + xlim(0,180) }
  #if (mode_2 == 'directional') { p <- p + ylim(0,360) }
  #if (mode_2 == 'axial') { p <- p + ylim(0,180) }
  
  p <- p + theme(aspect.ratio = 3 / 3)
  
  if ( (mode_1 == "linear") | (mode_2 == "linear") ) {
    reg_coeff <- signif(res, digits = 3)
    p <- p + ggtitle(sprintf("N = : %s \n r = %s", length(feature_1_values), reg_coeff))
  } else {
    
    reg_coeff <- signif(res$r, digits = 3)
    p_value_ <- signif(res$p.value, digits = 3)
    
    if (p_value_ < 0.001) {
      p_value <- "P < 0.001"
    } else if (p_value_ < 0.01) {
      p_value <- "P < 0.01"
    } else {
      p_value <- p_value_
    }
    
    p <- p + ggtitle(sprintf(" %s \n N = %s, r = %s,\n p-value: %s", plot_title, length(feature_1_values), reg_coeff, p_value))
  }
  
  p <- p + xlab(feature_1_name) + ylab(feature_2_name)

  return(p) 
  
}

compute_mean_circular <- function(feature_values, mode = "directional") {
  mean_dir <- 0.0
  if (mode == "directional") {
    mean_dir <- circ.mean(feature_values)

    if (mean_dir < 0.0) {
      mean_dir <- mean_dir + 2.0 * pi
    }
  } else {
    feature_values_ <- feature_values

    for (i in 1:length(feature_values)) {
      feature_values_[i] <- 2.0 * feature_values[i]
    }

    mean_dir <- circ.mean(feature_values_) / 2.0

    if (mean_dir < 0.0) {
      mean_dir <- mean_dir + pi
    }
  }

  return(mean_dir)
}


compute_correlation <- function(feature_1_values, mode_1 = "directional", feature_2_values, mode_2 = "directional") {
  feature_1_values_ <- feature_1_values
  feature_2_values_ <- feature_2_values

  if (mode_1 == "axial") {
    for (i in 1:length(feature_1_values)) {
      feature_1_values_[i] <- 2.0 * feature_1_values[i]
    }
  }

  if (mode_2 == "axial") {
    for (i in 1:length(feature_2_values)) {
      feature_2_values_[i] <- 2.0 * feature_2_values[i]
    }
  }

  res <- NULL

  if ((mode_1 != "linear") && (mode_2 != "linear")) {
    res <- circ.cor(feature_1_values_, feature_2_values_, test = TRUE)
  }

  if ((mode_1 == "linear") && (mode_2 != "linear")) {
  
    r_sx <- cor(sin(feature_2_values_), feature_1_values_, method= "pearson")
    r_cx <- cor(cos(feature_2_values_), feature_1_values_, method= "pearson")
    r_cs <- cor(sin(feature_2_values_), cos(feature_2_values_), method= "pearson")
    
    res <- sqrt((r_cx*r_cx + r_sx*r_sx - 2*r_cx*r_sx*r_cs)/(1-0 - r_cs*r_cs))
  }
  
  if ((mode_1 != "linear") && (mode_2 == "linear")) {
    
    r_sx <- cor(sin(feature_1_values_), feature_2_values_, method= "pearson")
    r_cx <- cor(cos(feature_1_values_), feature_2_values_, method= "pearson")
    r_cs <- cor(sin(feature_1_values_), cos(feature_1_values_), method= "pearson")
    
    res <- sqrt((r_cx*r_cx + r_sx*r_sx - 2*r_cx*r_sx*r_cs)/(1-0 - r_cs*r_cs))
  }

  if ((mode_1 == "linear") && (mode_2 == "linear")) {
    
    # TODO: Use cor.test function and provide confidence interval
    res <- cor(feature_1_values_, feature_2_values_, method = "pearson")
  }
  # res <- circ.cor(feature_1_values, feature_2_values, test = TRUE)



  return(res)
}
