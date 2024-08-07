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


###############################################################
## Modified April 4, 2024

#' @description: This function converts circular data from radians to degree or vice versa
#' @param data: list with circular data (radians or degree)
#' @param input: shiny input object, user specification of input unit, takes values "degree" or "radians"
#' @param target: target unit, takes values "degree" or "radians"
#' @return: list with circular data in radians or degree
circular_unit_conversion <- function(data, circ_units, target = "degrees") {  
  if ((target == "degrees") && (circ_units == "radians")) {
    return(data * 180.0 / pi)
  } else if ((target == "degrees") && (circ_units == "degrees")) {
    return(data)
  } else if ((target == "radians") && (circ_units == "radians")) {
    return(data)
  } else {
    return(data * pi / 180.0)
  }
}

#' @param data: list with circular data (radians)
compute_mean <- function(data, stats_mode) {
  
  if (stats_mode == "linear") {
    return(mean(data))
  } else {
    sin_sum <- 0.0
    cos_sum <- 0.0
    p_mode <- 1
    
    if (stats_mode == "axial") {
      p_mode <- 2
    }
    for (i in 1:length(data)) {
      angle <- p_mode*data[i]
      sin_sum <- sin_sum + sin(angle)
      cos_sum <- cos_sum + cos(angle)
    }
    sin_mean <- sin_sum / length(data)
    cos_mean <- cos_sum / length(data)
    
    angle_mean_rad <- atan2(sin_mean, cos_mean)/p_mode
    
    return(angle_mean_rad)
  }
}


compute_statistics <- function(data, feature, stats_mode, parameters) {

  if (input$stats_mode == "directional") {
    statistics <- compute_directional_statistics(data, feature, input, parameters)
  } else if (input$stats_mode == "axial") {
    statistics <- compute_axial_statistics(data, feature, input, parameters)
  } else {
    statistics <- compute_linear_statistics(data, feature, input, parameters)
  }

  return(statistics)
}

compute_directional_statistics <- function(data, feature, input, parameters) {
  "
  Computes directional statistics for a given feature and returns a data frame with the results
  "

  circular_data <- unlist(data[feature])
  circular_data <- circular_unit_conversion(circular_data, input$circ_units, "radians")
  sin_sum <- 0.0
  cos_sum <- 0.0
  polarity_index <- 0.0
  std_circular <- 0.0
  std_angular <- 0.0

  for (i in 1:length(circular_data)) {
    angle <- circular_data[i]
    sin_sum <- sin_sum + sin(angle)
    cos_sum <- cos_sum + cos(angle)
  }
  sin_mean <- sin_sum / length(circular_data)
  cos_mean <- cos_sum / length(circular_data)
  polarity_index <- sqrt(sin_mean * sin_mean + cos_mean * cos_mean)


  std_angular <- sqrt(2.0 * (1.0 - polarity_index)) * 180.0 / pi
  std_circular <- sqrt(-2.0 * log(polarity_index)) * 180.0 / pi

  angle_mean_rad <- atan2(sin_mean, cos_mean)
  angle_mean_deg <- angle_mean_rad * 180.0 / pi
  if (angle_mean_rad < 0.0) {
    angle_mean_deg <- 360.0 + angle_mean_rad * 180.0 / pi
  }

  std_circ_up_lim <- angle_mean_deg + std_circular
  if (std_circ_up_lim > 360.0) {
    std_circ_up_lim <- std_circ_up_lim - 360.0
  }
  print(std_circ_up_lim)
  std_circ_low_lim <- angle_mean_deg - std_circular
  if (std_circ_low_lim < 0.0) {
    std_circ_low_lim <- std_circ_low_lim + 360.0
  }
  print(std_circ_low_lim)

  std_ang_up_lim <- angle_mean_deg + std_angular
  if (std_ang_up_lim > 360.0) {
    std_ang_up_lim <- std_ang_up_lim - 360.0
  }
  std_ang_low_lim <- angle_mean_deg - std_angular
  if (std_ang_low_lim < 0.0) {
    std_ang_low_lim <- std_ang_low_lim + 360.0
  }

  rayleigh_test_res <- r.test(circular_data)
  # rayleigh_test_res <- r.test(results_df$angle_deg, degree = TRUE)
  watson_res <- capture.output(watson.test(circular_data, alpha = 0, dist = "vonmises"))
  # v_test_res <- v0.test(circular_data, mu0 = pi)
  rao_res <- capture.output(rao.spacing.test(circular_data, alpha = 0))

  #mu0_rad <- pi
  #mu0_deg <- 180.0
  #v_test_res <- v0.test(circular_data, mu0 = mu0_rad)
  #if (input$stats_method %in% c("V-Test")) {
    mu0_deg <- input$cond_mean_direction
    mu0_rad <- pi * input$cond_mean_direction/ 180.0
    v_test_res <- v0.test(circular_data, mu0 = mu0_rad)
  #}
  v_score <- cos(angle_mean_rad - mu0_rad) * polarity_index

  rayleigh_test <- rayleigh_test_res$p.value
  v_test <- v_test_res$p.value

  ci_95_res <- vm.bootstrap.ci(circular_data, alpha = 0.05)
  ci_95_lower_limit <- transform_rad_degrees(ci_95_res$mu.ci[[1]], -pi, pi, 0.0, 360.0)
  ci_95_upper_limit <- transform_rad_degrees(ci_95_res$mu.ci[[2]], -pi, pi, 0.0, 360.0)

  ci_90_res <- vm.bootstrap.ci(circular_data, alpha = 0.1)
  ci_90_lower_limit <- transform_rad_degrees(ci_90_res$mu.ci[[1]], -pi, pi, 0.0, 360.0)
  ci_90_upper_limit <- transform_rad_degrees(ci_90_res$mu.ci[[2]], -pi, pi, 0.0, 360.0)

  ci_50_res <- vm.bootstrap.ci(circular_data, alpha = 0.5)
  ci_50_lower_limit <- transform_rad_degrees(ci_50_res$mu.ci[[1]], -pi, pi, 0.0, 360.0)
  ci_50_upper_limit <- transform_rad_degrees(ci_50_res$mu.ci[[2]], -pi, pi, 0.0, 360.0)

  #values <- c(
  values <- data.frame(
    "number of cells" = nrow(data),
    "polarity_index" = polarity_index,
    "mean" = angle_mean_deg,
    "V_score" = v_score,
    "std_angular" = std_angular,
    "std_circ_up_lim" = std_circ_up_lim,
    "std_circ_low_lim" = std_circ_low_lim,
    "std_circular" = std_circular,
    "std_ang_up_lim" = std_ang_up_lim,
    "std_ang_low_lim" = std_ang_low_lim,
    "rayleigh_test" = rayleigh_test,
    "v_test" = v_test,
    "watson_test" = watson_res[5],
    "rao_test" = rao_res[5],
    "ci_95_lower_limit" = ci_95_lower_limit,
    "ci_95_upper_limit" = ci_95_upper_limit,
    "ci_90_lower_limit" = ci_90_lower_limit,
    "ci_90_upper_limit" = ci_90_upper_limit,
    "ci_50_lower_limit" = ci_50_lower_limit,
    "ci_50_upper_limit" = ci_50_upper_limit
  )

  return(values)
}

transform_rad_degrees <- function(value, source_low, source_high, target_low, target_high) {
  res <- 180.0 * value / pi
  if (value < target_low) {
    res <- 180.0 * value / pi + target_high
  }
  if (value > target_high) {
    res <- 180.0 * value / pi - target_high
  }

  return(res)
}

comparison_circular_statistics <- function(data_1, data_2, feature, input, parameters) {
  circular_data_1 <- unlist(data_1[feature])
  circular_data_2 <- unlist(data_2[feature])

  values_1 <- compute_directional_statistics(data_1, feature, input, parameters)
  values_2 <- compute_directional_statistics(data_2, feature, input, parameters)

  # values <- data.frame( "polarity_index" = polarity_index,
  #                      "mean" = angle_mean_deg,
  #                      "rayleigh_test" = rayleigh_test,
  #                      "rayleigh_test_mu" = rayleigh_test_mu)

  return(values_1)
}


compute_axial_statistics <- function(data, feature, input, parameters) {


  circular_data <- circular_unit_conversion(unlist(data[feature]), input$circ_units, "radians")
  p_directional_data <- circular_unit_conversion(unlist(data[feature]), input$circ_units, "radians")

  sin_sum <- 0.0
  cos_sum <- 0.0
  polarity_index <- 0.0

  for (i in 1:length(circular_data)) {
    circular_data[i] <- 2.0 * p_directional_data[i]
    angle <- circular_data[i]
    sin_sum <- sin_sum + sin(angle)
    cos_sum <- cos_sum + cos(angle)
  }
  sin_mean <- sin_sum / length(circular_data)
  cos_mean <- cos_sum / length(circular_data)

  polarity_index <- sqrt(sin_mean * sin_mean + cos_mean * cos_mean)


  std_angular <- sqrt(2.0 * (1.0 - polarity_index)) * 180.0 / pi
  std_circular <- sqrt(-2.0 * log(polarity_index)) * 180.0 / pi
  angle_mean_rad <- atan2(sin_mean, cos_mean) / 2.0
  
  if (angle_mean_rad < 0.0) {
    angle_mean_rad <- angle_mean_rad + pi
    #angle_mean_deg <- 180.0 + angle_mean_rad * 180.0 / pi
  }
  angle_mean_deg <- angle_mean_rad * 180.0 / pi

  rayleigh_test_res <- r.test(circular_data)
  rayleigh_test <- rayleigh_test_res$p.value

  mu0_deg <- input$cond_mean_direction
  mu0_rad <- pi * input$cond_mean_direction/ 180.0
  v_test_res <- v0.test(circular_data, mu0 = mu0_rad)
  v_score <- cos(2.0*angle_mean_rad - 2.0*mu0_rad) * polarity_index
  v_proj <- cos(angle_mean_rad - mu0_rad) * polarity_index
  #v_test_res <- v0.test(circular_data, mu0 = pi)
  v_test <- v_test_res$p.value

  #watson_res <- capture.output(watson.test(circular_data, alpha = 0, dist = "vonmises"))
  # v_test_res <- v0.test(circular_data, mu0 = pi)
  rao_res <- capture.output(rao.spacing.test(circular_data, alpha = 0))

  ci_res <- vm.bootstrap.ci(circular_data)

  ci_95_res <- vm.bootstrap.ci(circular_data, alpha = 0.05)
  ci_95_lower_limit <- transform_rad_degrees(ci_95_res$mu.ci[[1]] / 2.0, -pi / 2.0, pi / 2.0, 0.0, 180.0)
  ci_95_upper_limit <- transform_rad_degrees(ci_95_res$mu.ci[[2]] / 2.0, -pi / 2.0, pi / 2.0, 0.0, 180.0)

  ci_90_res <- vm.bootstrap.ci(circular_data, alpha = 0.1)
  ci_90_lower_limit <- transform_rad_degrees(ci_90_res$mu.ci[[1]] / 2.0, -pi / 2.0, pi / 2.0, 0.0, 180.0)
  ci_90_upper_limit <- transform_rad_degrees(ci_90_res$mu.ci[[2]] / 2.0, -pi / 2.0, pi / 2.0, 0.0, 180.0)

  ci_50_res <- vm.bootstrap.ci(circular_data, alpha = 0.5)
  ci_50_lower_limit <- transform_rad_degrees(ci_50_res$mu.ci[[1]] / 2.0, -pi / 2.0, pi / 2.0, 0.0, 180.0)
  ci_50_upper_limit <- transform_rad_degrees(ci_50_res$mu.ci[[2]] / 2.0, -pi / 2.0, pi / 2.0, 0.0, 180.0)

  ci_lower_limit <- 90.0 * ci_res$mu.ci[[1]] / pi
  ci_upper_limit <- 90.0 * ci_res$mu.ci[[2]] / pi

  if (ci_res$mu.ci[[1]] < 0.0) {
    ci_lower_limit <- 180.0 + 90.0 * ci_res$mu.ci[[1]] / pi
  }

  if (ci_res$mu.ci[[2]] < 0.0) {
    ci_upper_limit <- 180.0 + 90.0 * ci_res$mu.ci[[2]] / pi
  }

  std_circ_up_lim <- angle_mean_deg + std_circular
  if (std_circ_up_lim > 180.0) {
    std_circ_up_lim <- std_circ_up_lim - 180.0
  }
  print(std_circ_up_lim)
  std_circ_low_lim <- angle_mean_deg - std_circular
  if (std_circ_low_lim < 0.0) {
    std_circ_low_lim <- std_circ_low_lim + 180.0
  }

  std_ang_up_lim <- angle_mean_deg + std_angular
  if (std_ang_up_lim > 180.0) {
    std_ang_up_lim <- std_ang_up_lim - 180.0
  }
  std_ang_low_lim <- angle_mean_deg - std_angular
  if (std_ang_low_lim < 0.0) {
    std_ang_low_lim <- std_ang_low_lim + 180.0
  }

  values <- data.frame(
    "number of cells" = nrow(data),
    "polarity_index" = polarity_index,
    "mean" = angle_mean_deg,
    "rayleigh_test" = rayleigh_test,
    "V_score" = v_score,
    "V_proj" = v_proj,
    "v_test" = v_test,
    "mu0" = mu0_deg, 
    #"watson_test" = watson_res[5],
    "rao_test" = rao_res[5],
    "std_angular" = std_angular,
    "std_circ_up_lim" = std_circ_up_lim,
    "std_circ_low_lim" = std_circ_low_lim,
    "std_circular" = std_circular,
    "std_ang_up_lim" = std_ang_up_lim,
    "std_ang_low_lim" = std_ang_low_lim,
    "ci_95_lower_limit" = ci_95_lower_limit,
    "ci_95_upper_limit" = ci_95_upper_limit,
    "ci_90_lower_limit" = ci_90_lower_limit,
    "ci_90_upper_limit" = ci_90_upper_limit,
    "ci_50_lower_limit" = ci_50_lower_limit,
    "ci_50_upper_limit" = ci_50_upper_limit
  )
  return(values)
}


compute_linear_statistics <- function(data, feature, input, parameters) {
  data_ <- unlist(data[feature])

  mean_ <- mean(data_)
  std_ <- sd(data_)
  median_ <- median(data_)

  values <- data.frame( "number of cells" = nrow(data),
                        "mean" = mean_,
                        "std" = std_,
                        "median" = median_)
  return(values)
}
