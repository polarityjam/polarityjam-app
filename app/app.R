# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Polarity-JaM: Shiny app for plotting and comparing polarity data
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

options(shiny.maxRequestSize = 100 * 1024^2) # upload size is limited to 100 MB

if (!require("pacman")) install.packages ("pacman")
require('pacman')

p_load(data.table,shiny,shinyFiles,shinycssloaders,circular,ggplot2,shinyWidgets,tools,grid,gridExtra,tidyverse,CircStats,readxl,rjson,optparse, install=TRUE, update=FALSE)

option_list <- list(make_option(c("-p", "--port"), type = "integer", default = 8888))
opt <- parse_args(OptionParser(option_list = option_list))

# Review of color palettes https://thenode.biologists.com/data-visualization-with-flying-colors/research/ and more examples of use see https://huygens.science.uva.nl/PlotTwist/
# Color palettes Paul Tol: https://personal.sron.nl/~pault/

# From Paul Tol: https://personal.sron.nl/~pault/
Tol_bright <- c("#EE6677", "#228833", "#4477AA", "#CCBB44", "#66CCEE", "#AA3377", "#BBBBBB")

Tol_muted <- c("#88CCEE", "#44AA99", "#117733", "#332288", "#DDCC77", "#999933", "#CC6677", "#882255", "#AA4499", "#DDDDDD")

Tol_light <- c("#BBCC33", "#AAAA00", "#77AADD", "#EE8866", "#EEDD88", "#FFAABB", "#99DDFF", "#44BB99", "#DDDDDD")

# From Color Universal Design (CUD): https://jfly.uni-koeln.de/color/
Okabe_Ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")

# Create a reactive object here that we can share between all the sessions.
vals <- reactiveValues(count = 0)

# include html file
shiny::addResourcePath("about", "www")

# UI: User interface ---------------------------

ui <- navbarPage(
  "Polarity-JaM - a web app for visualizing cell polarity, junction and morphology data",

# Panel A: Data upload and preparation ---------------------------

  tabPanel(
    "Data preparation",
    sidebarLayout(
      sidebarPanel(

        radioButtons("data_upload_form", "Data from:", choices = list("example 1", "upload data"), selected = "example 1"), # local version

        conditionalPanel(
          condition = "input.data_upload_form == 'upload data'",
          checkboxInput("terms_of_use", "I agree to 'Terms of Use'", FALSE),
        ),

        conditionalPanel(
          condition = "input.terms_of_use == true",
          fileInput("stackData", "Upload data file",
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv", ".xlsx"
            )
          ),
          tags$hr(),
          checkboxInput("header_correlation", "File upload", TRUE),
        ),
        #TODO: add in future release, grouping of sample for instance by image/filename
        #selectInput("sample_col", "Identifier of samples", choices = ""),
        selectInput("condition_col", "Identifier of conditions", choices = ""),
        
        selectInput("remove_these_conditions", "Deselect these conditions:", "", multiple = TRUE),

        checkboxInput("filter_data", "Filter data", FALSE),
        conditionalPanel(
          condition = "input.filter_data == true",
          selectInput("filter_column", "Filter based on this parameter:", choices = ""),
          numericInput("max_value", "Set maximum value:", value = 1.0),
          numericInput("min_value", "Set minimum value:", value = 0.0)
        ),
        
        checkboxInput("group_samples", "Set identifier for samples", FALSE),
        conditionalPanel(
          condition = "input.group_samples == true",
          selectInput("sample_col", "Identifier of samples", choices = ""),
        ),
        
        # the data frame can be sub-sampled by selecting only every n-th row. 
        checkboxInput("subsample_data", "Subsample data", FALSE),
        conditionalPanel(
          condition = "input.subsample_data == true",
          numericInput("subsample_n", "Select every n-th row:", value = 1, min = 1, max = 50, step = 1)
        ),
        
        downloadButton("downloadFilteredData", "Download filtered data")
      ),

      mainPanel(
        tabsetPanel(
          tabPanel("Data", htmlOutput("terms_of_use_text"), tableOutput("data_table")),
          tabPanel("Processed Data", tableOutput("data_processed_table"))
        )
      )
    )
  ),

# Panel B: Plot data ---------------------------
  tabPanel(
    "Plot data",
    sidebarLayout(
      sidebarPanel(
        h4("Features"),
        selectInput("feature_select", "Choose a feature:", choices = ""),
        selectInput("stats_mode", "Choose data modality:",
          choices = c("directional", "axial", "linear"),
          selected = "directional"
        ),
        conditionalPanel(
          condition = "input.stats_mode != 'linear'",
          radioButtons("circ_units", "Circular units (input):", choices = list("radians", "degrees"), selected = "radians"),
        ),
        #radioButtons("circ_units", "Circular units (input):", choices = list("radians", "degrees"), selected = "radians"),
        #selectInput("plot_type", "Choose a plot type",
        #  choices = c("Boxplot", "Violin plot", "Scatter plot", "Histogram", "Density plot")
        #),        
        checkboxInput("scatter_plot", "Scatter plot", TRUE),
        checkboxInput("histogram_plot", "Histogram plot", TRUE),
        #checkboxInput("kde_plot", "KDE plot", FALSE),
        conditionalPanel(
          condition = "input.histogram_plot == true",
          sliderInput("bins",
            "Number of bins:",
            min = 4,
            max = 36,
            value = 12
          ),
          checkboxInput("area_scaled", "Area scaled histogram", TRUE),
        ),
        h4("Statistics"),
        checkboxInput("plot_PI", "Plot mean and polarity index", TRUE),
        selectInput("stats_method", "Choose a stats test",
          choices = c("None", "Rayleigh uniform", "V-Test", "Rao's Test", "Watson's Test")
        ),
        checkboxInput("ci_plot", "Confidence interval (CI)", TRUE),
        conditionalPanel(
          condition = "input.ci_plot == true",
          selectInput("ci_method", "CI method",
            choices = c(
              "95% CI of the mean", "90% CI of the mean", "50% CI of the mean",
              "circular standard deviation", "angular standard deviation"
            )
          )
        ),
        numericInput("cond_mean_direction",
                      "Polar direction for V-test and V-score (degrees)",
                      value = 0
                    ),
        checkboxInput("plot_polar_direction", "Plot polar vector and V-score", FALSE),
        conditionalPanel(
          condition = "input.stats_mode == 'axial'",
          selectInput("hemi_rose_options", "Hemirose plot options:",
            choices = c("mirrored", "up", "down", "left", "right")
          )
        ),
        selectInput("select_colormap", "Choose a color scheme",
          choices = c("gray", "Okabe_Ito", "Tol_bright", "Tol_muted", "Tol_light")
        ),
        conditionalPanel(
          condition = "input.select_colormap != 'gray'",
          numericInput("select_color", "Select a color from color scheme:", value = 1, min = 1, max = 10, step = 1),
        ),
        #checkboxInput("cond_as_color", "Condition as color", FALSE),
        checkboxInput("adjust_alpha", "Adjust transparency", FALSE),
        conditionalPanel(
          condition = "input.adjust_alpha == true",
          numericInput("alpha_fill", "set alpha fill:", value = 0.5, min = 0.0, max = 1.0, step = 0.1),
          selectInput("outline", "choose outline style:", choice = c("color", "white", "black"))
        ),        
        numericInput("text_size", "Text size", value = 12, min = 4, max = 50, step = 1),
        numericInput("marker_size", "Marker size", value = 3, min = 1, max = 20, step = 1),
        numericInput("plot_height_A", "Height (# pixels): ", value = 720),
        numericInput("plot_width_A", "Width (# pixels):", value = 1280),
        downloadButton("downloadData", "Download statistics")
      ),
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Plot", downloadButton("downloadMultiPlotPDF", "Download pdf-file"),
            downloadButton("downloadMultiPlotEPS", "Download eps-file"),
            downloadButton("downloadMultiPlotSVG", "Download svg-file"),
            downloadButton("downloadMultiPlotPNG", "Download png-file"),
            div(`data-spy` = "affix", `data-offset-top` = "10", withSpinner(plotOutput("multi_dist_plot", height = "120%"))),
            #textOutput("parameter_error"),
            NULL,
          ),
          tabPanel(
            "MergedPlot", downloadButton("downloadMergedPlotPDF", "Download pdf-file"),
            downloadButton("downloadMergedPlotEPS", "Download eps-file"),
            downloadButton("downloadMergedPlotSVG", "Download svg-file"),
            downloadButton("downloadMergedPlotPNG", "Download png-file"),
            div(`data-spy` = "affix", `data-offset-top` = "10", withSpinner(plotOutput("merged_plot", height = "120%"))),
            #textOutput("parameter_error"),
            NULL,
          ),
          tabPanel("Statistics", tableOutput("summaryStatisticsTable"))
        )
      )
    )
  ),

  # Panel C: Correlation analysis ---------------------------

  tabPanel(
    "Correlation analysis",
    sidebarLayout(
      sidebarPanel(
        selectInput("feature_select_1", "Choose feature 1 (x-axis):", choices = ""),
        selectInput("stats_mode_1", "Data modality feature 1:",
                    choices = c("directional", "axial", "linear"),
                    selected = "directional"
        ),
        conditionalPanel(
          condition = "input.stats_mode_1 != 'linear'",
          radioButtons("circ_units_1", "Circular units (input 1):", choices = list("radians", "degrees"), selected = "radians"),
        ),
        selectInput("feature_select_2", "Choose feature 2 (y-axis):", choices = ""),
        selectInput("stats_mode_2", "Data modality feature 2:",
                    choices = c("directional", "axial", "linear"),
                    selected = "directional"
        ),
        conditionalPanel(
          condition = "input.stats_mode_2 != 'linear'",
          radioButtons("circ_units_2", "Circular units (input 2):", choices = list("radians", "degrees"), selected = "radians"),
        ),
        checkboxInput(inputId = "change_scale",
                                 label = "Change scale",
                                 value = FALSE),
        conditionalPanel(
          condition = "input.change_scale == true",
          numericInput("start_x", "x-axis start", value = 0)
        ),
        conditionalPanel(
          condition = "input.change_scale == true",
          numericInput("start_y", "y-axis start", value = 0)
        ),
        #selectInput("datasetSingleImage", "Download:",
        #  choices = c("results_file", "statistics_file", "orientation_plot", "rose_histogram")
        #),
        # tags$hr(),
        #selectInput("corr_plot_option", "Choose a plot option:",
        #  choices = c("correlation plot", "spoke plot")
        #),
        conditionalPanel(
          condition = "input.corr_plot_option == 'correlation plot'",
          checkboxInput("center_corr_plot", "center correlation plot", TRUE),
        ),
        conditionalPanel(
          condition = "input.corr_plot_option == 'spoke plot'",
          numericInput("spoke_subsample_n", "Subsample every n-th row:", value = 1, min = 1, max = 50, step = 1)
        ),
        numericInput("text_size_corr", "text size", value = 12, min = 4, max = 50, step = 1),
        numericInput("marker_size_corr", "marker size", value = 3, min = 1, max = 20, step = 1),
        numericInput("marker_alpha_corr", "marker transparency", value = 0.5, min = 0.0, max = 1.0, step = 0.05),
        numericInput("plot_height_corr", "Height (# pixels): ", value = 600),
        numericInput("plot_width_corr", "Width (# pixels):", value = 800),
        checkboxInput("header_image", "File upload", TRUE),
        downloadButton("downloadCorrelationData", "Download")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Plot", downloadButton("downloadCorrPlotPDF", "Download pdf-file"),
            downloadButton("downloadCorrPlotEPS", "Download eps-file"),
            downloadButton("downloadCorrPlotSVG", "Download svg-file"),
            downloadButton("downloadCorrPlotPNG", "Download png-file"),
            div(`data-spy` = "affix", `data-offset-top` = "10", withSpinner(plotOutput("multi_corr_display", height = "120%"))),
            NULL,
          ),
          tabPanel(
            "MergedPlot", downloadButton("downloadMergedCorrPlotPDF", "Download pdf-file"),
            downloadButton("downloadMergedCorrPlotEPS", "Download eps-file"),
            downloadButton("downloadMergedCorrPlotSVG", "Download svg-file"),
            downloadButton("downloadMergedCorrPlotPNG", "Download png-file"),
            div(`data-spy` = "affix", `data-offset-top` = "10", withSpinner(plotOutput("correlation_plot", height = "120%"))),
            NULL,
          ),
          tabPanel("Statistics", tableOutput("correlation_statistics"))
        )
      )
    )
  ),

  ### Panel D: Comparison statistics

  tabPanel(
    "Compare",
    sidebarLayout(
      sidebarPanel(

        selectInput("control_condition", "control condition", choices = ""),
        selectInput("feature_comparison", "Choose a feature:", choices = ""),
        checkboxInput("kde_comparison", "KDE plot", FALSE),
        checkboxInput("histogram_comparison", "Histogram plot", TRUE),
        #TODO: check whether split view can be beneficial
        #                checkboxInput("split_view_comparison", "Split view", TRUE),
      ),
      mainPanel(
        tabsetPanel(
          #TODO: revise plotting options and add download
          tabPanel("Plot", plotOutput("comparison_plot", height = "1000px")),
          tabPanel("CDF Plot", plotOutput("CDFPlot")),
          tabPanel("Statistics", tableOutput("comparison_statistics")),
          NULL
        )
      )
    )
  ),

  ### Panel E: Terms of Use

  tabPanel(
    "Terms of Use",
    sidebarLayout(
      sidebarPanel(
        checkboxInput("terms_of_use_all", "I agree to the terms of use", FALSE),
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Terms of Use", htmlOutput("terms_of_use_text_all"))
        )
      )
    )
  ),
  
  # Panel F: About the app ---------------------------

  tabPanel("About", 
           #includeHTML("About.html"),
           tags$iframe(src = "about/About.html", height = "300px", width = "100%", style = "border:0"),
           imageOutput("support_logo")
  )

)


# Define server logic
server <- function(input, output, session) {

  ### functions related to: Panel A, data preparation
  source(file = paste0(getwd(), "/src/circular_statistics.R"), local = T)
  source(file = paste0(getwd(), "/src/plot_functions.R"), local = T)
  source(file = paste0(getwd(), "/src/circular_correlations.R"), local = T)
  
  data_upload <- reactive({
    "
    reactive function that reads a csv file or xls file and returns a data frame,
    rows containing NA values are removed
    "

    inFileStackData <- input$stackData

    if (input$data_upload_form == "example 1") {

      data_df <- read.csv("example_1/example_1.csv", header = TRUE)

    } else if (!is.null(inFileStackData) & (input$data_upload_form == "upload data")) {

      data_df <- read.csv(inFileStackData$datapath, header = input$header_correlation)

    } else {

      data_df <- data.frame()

    }

    return(data_df)
  })

  observe({
    "
    update choices for fields according to data frame columns:
    "

    data <- data_upload()
    var_names <- colnames(data_upload())

    if (length(var_names) > 0) {
      var_list <- c("none", var_names)
    } else {
      var_list <- c("none")
    }

    # set defaults
    feature_select_default = "nuclei_golgi_rad"
    if (feature_select_default %in% var_list) {
      feature_select_default = feature_select_default
    } else if ("cell_shape_orientation_rad" %in% var_list) {
      feature_select_default = "cell_shape_orientation_rad"
    } else {
      feature_select_default = "none"
    }

    updateSelectInput(session, "sample_col", choices = var_list, selected = "label")
    updateSelectInput(session, "condition_col", choices = var_list, selected = "filename") # for Panel A
    #updateSelectInput(session, "feature_select", choices = var_list, selected = "cell_shape_orientation_rad")
    updateSelectInput(session, "feature_select", choices = var_list, selected = feature_select_default) # for Panel B
    updateSelectInput(session, "feature_select_1", choices = var_list, selected = "cell_shape_orientation_rad") # for Panel C
    updateSelectInput(session, "feature_select_2", choices = var_list, selected = "nuc_shape_orientation_rad") # for Panel C
    updateSelectInput(session, "feature_comparison", choices = var_list, selected = feature_select_default) # for Panel D
    updateSelectInput(session, "filter_column", choices = var_list, selected="none")

  })

  observeEvent(input$condition_col != 'none', {
    "
    update list of conditions that can be removed by the user in Panel A
    "

    data <- data_upload()
    var_names <- colnames(data_upload())
    if (length(var_names) > 0) {
      if (input$condition_col %in% colnames(data)) {
        condition_list <- unique(data[input$condition_col])
        updateSelectInput(session, "remove_these_conditions", choices = condition_list)
      }
    }
    
  })

  observeEvent(input$feature_select != 'none', {
    "
    select data modality directional circular data, axial circular data or linear (non-circular) data,
    auto select if present in parameter file
    "

    parameters <- fromJSON(file = "parameters/parameters.json")
    if (input$feature_select %in% names(parameters)) {
      stats_mode <- parameters[input$feature_select][[1]][2]
      updateSelectInput(session, "stats_mode", choices = c("directional", "axial", "linear"), selected = stats_mode)
    } else {
      updateSelectInput(session, "stats_mode", choices = c("directional", "axial", "linear"), selected = "linear")
    }
  })

  data_filtered <- reactive({
    df_filtered <- data_upload() 

    #source(file = paste0(getwd(), "/src/circular_statistics.R"), local = T)
    
    #if subsampling data is selected, only every n-th row is kept
    if (input$subsample_data) {
      N <- nrow(df_filtered) %/% input$subsample_n
      if (nrow(df_filtered) > N) {
        df_filtered <- df_filtered[sample(nrow(df_filtered), N), ]
      }
    }

    ### filter out selected conditions (categorial)
    if ( !is.null(input$remove_these_conditions) && (input$condition_col != "none"))
    {
      condition_column <- input$condition_col
      remove_these_conditions <- input$remove_these_conditions
      df_filtered <- df_filtered %>% filter(!.data[[condition_column[[1]]]] %in% !!remove_these_conditions)
    }

    # remove samples that are outside of the specified value range
    if ( (input$filter_data == TRUE) && (input$filter_column != "none")) {
      df_filtered <- df_filtered[df_filtered[input$filter_column] > input$min_value, ]
      df_filtered <- df_filtered[df_filtered[input$filter_column] < input$max_value, ]
    }
    
    return(df_filtered)
  })
  
  data_processed <- reactive({
    df_processed  <- NULL
    data <- data_filtered()
    
    if (input$group_samples == TRUE) {
      
      col_names <- colnames(data_filtered())
        
      if ( (input$condition_col %in% col_names) & (input$sample_col %in% col_names) ) {
        if ( (input$feature_select %in% col_names) & (input$feature_select_1 %in% col_names) & (input$feature_select_2 %in% col_names)) {
            
          df_processed <- data.frame(matrix(nrow = 0, ncol = 5))
          colnames(df_processed) <- c(input$condition_col, input$sample_col, input$feature_select,  input$feature_select_1,  input$feature_select_2)  
          
          df_col_select <- data[c(input$condition_col, input$sample_col, input$feature_select, input$feature_select_1, input$feature_select_2)]
          
          condition_list <- unlist(unique(df_col_select[input$condition_col]))
          
          for (condition in condition_list) {
            df_single_cond  <- df_col_select[df_col_select[input$condition_col] == condition,]

            sample_identifier_list <- unlist(unique(df_single_cond[input$sample_col]))
            num_samples <- length(sample_identifier_list)
            
            mean_list = c()
            mean_list_1 = c()
            mean_list_2 = c()
            
            #TODO: add support for axial and linear data
            #TODO: supper selections of degrees or radians
            x_data = circular_unit_conversion(unlist(df_single_cond[input$feature_select]),input$circ_units,target = "radians")
            x_data_1 = circular_unit_conversion(unlist(df_single_cond[input$feature_select_1]),input$circ_units,target = "radians")
            x_data_2 = circular_unit_conversion(unlist(df_single_cond[input$feature_select_2]),input$circ_units,target = "radians")

            for(i in 1:num_samples) {       # for-loop over columns
              df_sample = subset(df_single_cond, df_single_cond[input$sample_col] == sample_identifier_list[i])
              mean_list =  append(mean_list, compute_mean(unlist(df_sample[input$feature_select]),input$stats_mode))
              mean_list_1 =  append(mean_list_1, compute_mean(unlist(df_sample[input$feature_select_1]),input$stats_mode_1))
              mean_list_2 =  append(mean_list_2, compute_mean(unlist(df_sample[input$feature_select_2]),input$stats_mode_2))
            }
     
            df_mean <- data.frame(matrix(nrow = num_samples, ncol = 5))
            colnames(df_mean) <- colnames(df_processed)

            df_mean[input$condition_col] = condition
            df_mean[input$sample_col] = sample_identifier_list
            df_mean[input$feature_select] = mean_list
            df_mean[input$feature_select_1] = mean_list_1
            df_mean[input$feature_select_2] = mean_list_2
            df_processed <- rbind(df_processed, df_mean)
            
          }
          
        }
      }
      
    } else {
      df_processed <- data_filtered()
    }
    
    df_processed <- df_processed[, colSums(is.na(df_processed)) != nrow(df_processed)]
    df_processed <- na.omit(df_processed)

    return(df_processed) 
  })


  output$data_table <- renderTable({
    "
    function that prints data in Panel A
    "

    if ((input$data_upload_form == "upload data") & (input$terms_of_use == FALSE)) {

    } else {
      data_upload()
    }
  })
  
  output$data_processed_table <- renderTable({
    "
    function that prints filtered data in Panel A
    "
    
    if ((input$data_upload_form == "upload data") & (input$terms_of_use == FALSE)) {
      
    } else {
      data_processed()
    }
  })

  output$downloadFilteredData <- downloadHandler(
  #  "
  #  download filtered data displayed in Panel A
  #  "

    filename = function() {
      filename <- "data_filtered.csv"
      return(filename)
    },
    content = function(file) {
      return(write.csv(data_filtered(), file, row.names = FALSE))
    }
  )

  #####################################################
  ### functions related to: Panel B, data visualization
  #####################################################

  summaryStatistics <- reactive({
    "
    reactive function that reads a stack of spreadsheet and returns a data frame 
    with descriptive statistics depending on the selected data modality directional, axial or linear
    "

    data_df <- data_filtered()

    #source(file = paste0(getwd(), "/src/circular_statistics.R"), local = T)
    parameters <- fromJSON(file = "parameters/parameters.json")

    condition_col <- input$condition_col
    condition_list <- unlist(unique(data_df[condition_col]))

    feature <- input$feature_select
    if (feature %in% names(parameters)) {
      feature <- parameters[input$feature_select][[1]][1]
    }

    cols <- c("statistical measure")
    for (condition in condition_list) {
      cols <- c(cols, condition)
    }
    cols <- c(cols, "all (merged)")

    statistics <- compute_statistics(data_df, feature, parameters)
    t_statistics = t(statistics)
    statistics_df <- as.data.frame(matrix(ncol = length(condition_list) + 2, nrow = nrow(t_statistics)))
    colnames(statistics_df) <- cols
    statistics_df[,"statistical measure"] <- rownames(t_statistics)
    statistics_df[,"all (merged)"] <- t_statistics[,1]

    for (condition in condition_list) {
      condition_data <- data_df[data_df[condition_col] == condition,]
      
      #if (input$circ_units == "radians") {
      #  x_data <- radians_to_degrees(unlist(condition_data[feature]))
      #} else {
      #  x_data <- unlist(condition_data[feature]) 
      #}
      
      x_data <- circular_unit_conversion(unlist(condition_data[feature]), input$circ_units, target = "degrees")

      statistics <- compute_statistics(condition_data, feature, parameters)
      t_statistics = t(statistics)
      statistics_df[,condition] <- t_statistics[,1]
    }

    return(statistics_df)
  })

  output$summaryStatisticsTable <- renderTable(    {
     "
    function that shows the descriptive statistics of the merged data stack in table format
    "
      statistics_df <- summaryStatistics()
      statistics_df
    }#, digits = 3
  )

  merged_plot <- reactive({

    parameters <- fromJSON(file = "parameters/parameters.json")
    text_size <- input$text_size

    data <- data_processed()
    
    bin_size <- 360 / input$bins
    exp_condition <- input$exp_condition
    feature <- input$feature_select
    plot_title <- feature
    if (feature %in% names(parameters)) {
      plot_title <- parameters[input$feature_select][[1]][3]
    }

    if (input$stats_mode == "directional") {

      #if (input$circ_units == "radians") {
      #  x_data <- radians_to_degrees(unlist(data[feature]))
      #} else {
      #  x_data <- unlist(data[feature])
      #}

      x_data <- circular_unit_conversion(unlist(data[feature]), input$circ_units, target = "degrees")

      #x_data <- unlist(data[feature]) * 180.0 / pi
      statistics <- compute_directional_statistics(data, feature, parameters)

      p <- rose_plot_circular(parameters, input, statistics, x_data, plot_title, 0, text_size)

    } else if (input$stats_mode == "axial") {
      
      x_data <- data[feature]
      # TODO: add support for radians/degrees in stats analyis
      statistics <- compute_axial_statistics(data, feature, parameters)

      #if (input$circ_units == "radians") {
      #  x_data <- radians_to_degrees(unlist(transform_axial(input, x_data)))
      #}
      #else {
      #  x_data <- unlist(transform_axial(input, x_data))
      #}
      #x_data <- circular_unit_conversion(transform_axial(input, x_data), input$circ_units, target = "degrees")
      x_data <- circular_unit_conversion(data, input$circ_units, target = "degrees")
      #x_data <- unlist(transform_axial(input, x_data)) * 180.0 / pi

      p <- rose_plot_axial(parameters, input, statistics, x_data, plot_title, 0, text_size)
    } else {
      x_data <- unlist(data[feature])
      statistics <- compute_linear_statistics(data, feature, parameters)
      
      p <- linear_histogram(parameters, input, statistics, x_data, plot_title, 0, text_size, min(x_data), max(x_data))
    }

    # return the plot
    return(p)
  })

  width_A <- reactive({
    input$plot_width_A
  })
  height_A <- reactive({
    input$plot_height_A
  })
  
  output$merged_plot <- renderPlot(width = width_A, height = height_A, {
    parameters <- fromJSON(file = "parameters/parameters.json")

    p <- merged_plot()
    return(p)
 
  })

  output$parameter_error <- renderText({
    parameters <- fromJSON(file = "parameters/parameters.json")

    if (input$feature_select %in% names(parameters)) {

    } else {
      print("Plotting of this parameter is not supported.")
    }

  })

  multi_plot <- reactive({
    "
    function that plots data for every condition in the selected column of the data frame
    "
    parameters <- fromJSON(file = "parameters/parameters.json")
    text_size <- input$text_size

    i <- 1
    #angle_dists <- list()
    condition_names <- list()
    polarity_indices <- list()
    angle_mean_degs <- list()

    #results_all_df <- data_filtered()
    results_all_df <- data_processed()
    
    feature <- input$feature_select
    if (feature %in% names(parameters)) {
      feature <- parameters[input$feature_select][[1]][1]
    }

    condition_col <- input$condition_col
    condition_list <- unlist(unique(results_all_df[condition_col]))

    x_lim <- c(min(results_all_df[feature]), max(results_all_df[feature]))
    
    for (condition in condition_list) {
      #results_df <- subset(results_all_df, results_all_df[condition_col] == condition_name)
      results_df <- results_all_df[results_all_df[condition_col] == condition,]
      #x <- unlist(results_df[feature])
      #angle_dists[[i]] <- x
      condition_names[[i]] <- condition
      i <- i + 1
    }

    n <- length(condition_names)
    nCol <- floor(sqrt(n))

    bin_size <- 360 / input$bins

    plotseries <- function(i) {
      #angle_dist <- angle_dists[[i]]
      condition_name <- condition_names[[i]]

      results_df <- results_all_df[results_all_df[condition_col] == condition_name,]
      
      plot_title <- condition_name

      if (nchar(condition_name) > 37) {
        max_fl <- 17
        condition_name_end <- substr(condition_name, nchar(condition_name) - max_fl + 1, nchar(condition_name))
        condition_name_start <- substr(condition_name, 1, max_fl)
        plot_title <- paste0(condition_name_start, "...", condition_name_end)
      }

      if (input$stats_mode == "directional") {
        statistics <- compute_directional_statistics(results_df, feature, parameters)
        if (input$circ_units == "radians") {
          x_data <- unlist(results_df[feature]) * 180.0 / pi
        } else {
          x_data <- unlist(results_df[feature])
        }
        p <- rose_plot_circular(parameters, input, statistics, x_data, plot_title, i, text_size)
      } else if (input$stats_mode == "axial") {
        x_data <- results_df[feature]
        statistics <- compute_axial_statistics(results_df, feature, parameters)
        if (input$circ_units == "radians") {
          x_data <- unlist(results_df[feature]) * 180.0 / pi
        } else {
          x_data <- unlist(results_df[feature])
        }
        p <- rose_plot_axial(parameters, input, statistics, x_data, plot_title, i, text_size)
      } else {
        x_data <- unlist(results_df[feature])
        statistics <- compute_linear_statistics(results_df, feature, parameters)
        p <- linear_histogram(parameters, input, statistics, x_data, plot_title, i, text_size, min(results_all_df[feature]), max(results_all_df[feature]))
      }
    }


    myplots <- lapply(1:length(condition_names), plotseries)

    return(grid.arrange(grobs = myplots, nrow = nCol)) 
  })

  output$multi_dist_plot <- renderPlot(width = width_A, height = height_A, {

    feature <- input$feature_select
    #if (feature %in% colnames()) {
        multi_plot()
    #} else {
    #    print("Please select a feature")
    #}
  })

  output$downloadData <- downloadHandler(
    filename = function() {

      filename <- "statistics_file.csv"

      return(filename)
    },
    content = function(file) {

      return(write.csv(summaryStatistics(), file, row.names = FALSE))

    }
  )

  # download for merged plot

  output$downloadMergedPlotPDF <- downloadHandler(
    filename <- function() {
      paste(paste0("PolarityJaM_", input$featue_select,"_Merged_"), Sys.time(), ".pdf", sep = "")
    },
    content <- function(file) {
      pdf(file, width = input$plot_width_A / 72, height = input$plot_height_A / 72)
      plot(merged_plot())
      dev.off()
    },
    contentType = "application/pdf" # MIME type of the image
  )

  output$downloadMergedPlotSVG <- downloadHandler(
    filename <- function() {
      paste("PolarityJaM_Merged_", Sys.time(), ".svg", sep = "")
    },
    content <- function(file) {
      svg(file, width = input$plot_width_A / 72, height = input$plot_height_A / 72)
      plot(merged_plot())
      dev.off()
    },
    contentType = "application/svg" # MIME type of the image
  )

  output$downloadMergedPlotEPS <- downloadHandler(
    filename <- function() {
      paste("PolarityJaM_Merged_", Sys.time(), ".eps", sep = "")
    },
    content <- function(file) {
      cairo_ps(file, width = input$plot_width_A / 72, height = input$plot_height_A / 72)
      plot(merged_plot())
      dev.off()
    },
    contentType = "application/eps" # MIME type of the image
  )

  output$downloadMergedPlotPNG <- downloadHandler(
    filename <- function() {
      paste("PolarityJaM_", input$feature_select, "_Merged_", Sys.time(), ".png", sep = "")
    },
    content <- function(file) {
      png(file, width = input$plot_width_A * 4, height = input$plot_height_A * 4, res = 300)
      plot(merged_plot())
      dev.off()
    },
    contentType = "application/png" # MIME type of the image
  )

  # download for multi plot
  # TODO: check why multi plot files have a grid when downloaded, while no grid is displayed in the app

  output$downloadMultiPlotPDF <- downloadHandler(
    filename <- function() {
      paste("PolarityJaM_Multi_", Sys.time(), ".pdf", sep = "")
    },
    content <- function(file) {
      pdf(file, width = input$plot_width_A / 72, height = input$plot_height_A / 72)
      plot(multi_plot())
      dev.off()
    },
    contentType = "application/pdf" # MIME type of the image
  )

  output$downloadMultiPlotSVG <- downloadHandler(
    filename <- function() {
      paste("PolarityJaM_Multi_", Sys.time(), ".svg", sep = "")
    },
    content <- function(file) {
      svg(file, width = input$plot_width_A / 72, height = input$plot_height_A / 72)
      plot(multi_plot())
      dev.off()
    },
    contentType = "application/svg" # MIME type of the image
  )

  output$downloadMultiPlotEPS <- downloadHandler(
    filename <- function() {
      paste("PolarityJaM_Multi_", Sys.time(), ".eps", sep = "")
    },
    content <- function(file) {
      cairo_ps(file, width = input$plot_width_A / 72, height = input$plot_height_A / 72)
      plot(multi_plot())
      dev.off()
    },
    contentType = "application/eps" # MIME type of the image
  )

  output$downloadMultiPlotPNG <- downloadHandler(
    filename <- function() {
      paste("PolarityJaM_", input$feature_select, "_Multi_", Sys.time(), ".png", sep = "")
    },
    content <- function(file) {
      png(file, width = input$plot_width_A * 4, height = input$plot_height_A * 4, res = 300)
      plot(multi_plot())
      dev.off()
    },
    contentType = "application/png" # MIME type of the image
  )


  ### Panel C
  
  observeEvent(input$feature_select_1 != 'none', {
    "
    select data modality directional circular data, axial circular data or linear (non-circular) data,
    auto select if present in parameter file
    "
    
    parameters <- fromJSON(file = "parameters/parameters.json")
    if (input$feature_select_1 %in% names(parameters)) {
      stats_mode_1 <- "linear"
      feature_1 <- input$feature_select_1
      if (feature_1 %in% names(parameters)) {
        stats_mode_1 <- parameters[feature_1][[1]][2]
      }     
      updateSelectInput(session, "stats_mode_1", choices = c("directional", "axial", "linear"), selected = stats_mode_1)
    } else {
      updateSelectInput(session, "stats_mode_1", choices = c("directional", "axial", "linear"), selected = "linear")
    }
  })
  
  observeEvent(input$feature_select_2 != 'none', {
    "
    select data modality directional circular data, axial circular data or linear (non-circular) data,
    auto select if present in parameter file
    "
    
    parameters <- fromJSON(file = "parameters/parameters.json")
    if (input$feature_select_2 %in% names(parameters)) {
      #stats_mode_2 <- parameters[input$feature_select_2][[1]][2]
      stats_mode_2 <- "linear"
      feature_2 <- input$feature_select_2
      if (feature_2 %in% names(parameters)) {
        stats_mode_2 <- parameters[feature_2][[1]][2]
      }    
      updateSelectInput(session, "stats_mode_2", choices = c("directional", "axial", "linear"), selected = stats_mode_2)
    } else {
      updateSelectInput(session, "stats_mode_2", choices = c("directional", "axial", "linear"), selected = "linear")
    }
  })

  plot_correlation <- reactive({
        
    parameters <- fromJSON(file = "parameters/parameters.json")
    
    text_size <- input$text_size_corr
    correlation_data <- data_filtered()

    # feature_1 <- parameters[input$feature_select_1][[1]][1]
    # feature_2 <- parameters[input$feature_select_2][[1]][1]
    
    
    # feature_1_values <- unlist(correlation_data[feature_1])
    # if (input$circ_units_1 == "radians") {
    #   feature_1_values_ <- correlation_data[feature_1] * 180.0 / pi
    # } else {
    #   feature_1_values_ <- correlation_data[feature_1]
    # }
    # #feature_1_values_ <- correlation_data[feature_1] * 180.0 / pi
    # feature_2_values <- unlist(correlation_data[feature_2])
    # if (input$circ_units_2 == "radians") {
    #   feature_2_values_ <- correlation_data[feature_2] * 180.0 / pi
    # } else {
    #   feature_2_values_ <- correlation_data[feature_2]
    # }    
    # #feature_2_values_ <- correlation_data[feature_2] * 180.0 / pi

    # feature_1_name <- parameters[input$feature_select_1][[1]][3]
    # feature_2_name <- parameters[input$feature_select_2][[1]][3]

    # conditions <- correlation_data[input$condition_col]
         
    p <- plot_circular_circular(correlation_data, input, parameters, plot_nr = 0, text_size = 24) 
      
 
  })
  

  multi_corr_plot <- reactive({ 
    
    parameters <- fromJSON(file = "parameters/parameters.json")
    text_size <- input$text_size_corr
    
    correlation_data <- data_filtered() # read.csv(inFileCorrelationData$datapath, header = input$header_correlation)
    
    # feature_1 <- parameters[input$feature_select_1][[1]][1]
    # feature_2 <- parameters[input$feature_select_2][[1]][1]
    
    
    # feature_1_values <- unlist(correlation_data[feature_1])
    # if (input$circ_units_1 == "radians") {
    #   feature_1_values_ <- correlation_data[feature_1] * 180.0 / pi
    # } else {
    #   feature_1_values_ <- correlation_data[feature_1]
    # }
    # #feature_1_values_ <- correlation_data[feature_1] * 180.0 / pi
    # feature_2_values <- unlist(correlation_data[feature_2])
    # if (input$circ_units_2 == "radians") {
    #   feature_2_values_ <- correlation_data[feature_2] * 180.0 / pi
    # } else {
    #   feature_2_values_ <- correlation_data[feature_2]
    # }
    
    # #feature_2_values_ <- correlation_data[feature_2] * 180.0 / pi
    
    # feature_1_name <- parameters[input$feature_select_1][[1]][3]
    # feature_2_name <- parameters[input$feature_select_2][[1]][3]
    
    conditions <- correlation_data[input$condition_col]
    condition_list <- unlist(unique(correlation_data[input$condition_col]))
    plist <- vector("list", length(condition_list))
    
    n <- length(condition_list)
    nCol <- floor(sqrt(n))
    
    bin_size <- 360 / input$bins
    
    plotseries <- function(i) {
      file_name <- condition_list[i]
      plot_title <- file_name 
      
      if (nchar(file_name) > 37) {
        max_fl <- 17
        file_name_end <- substr(file_name, nchar(file_name) - max_fl + 1, nchar(file_name))
        file_name_start <- substr(file_name, 1, max_fl)
        plot_title <- paste0(file_name_start, "...", file_name_end)
      }
      
      data <- subset(correlation_data, correlation_data[input$condition_col] == condition_list[i])
      p <- plot_circular_circular(data, input, parameters, plot_title, plot_nr = i, text_size = text_size) 
    }
    
    myplots <- lapply(1:n, plotseries)
    
    # print(myplots)
    grid.arrange(grobs = myplots, nrow = nCol) # , widths = list(10,10))
    
  })
  
  width_corr <- reactive({
    input$plot_width_corr
  })
  height_corr<- reactive({
    input$plot_height_corr
  })
  
  output$multi_corr_display <- renderPlot(width = width_corr, height = height_corr, {
    multi_corr_plot()
  })
  
  #output$multi_corr_display <- renderPlot(width = width_A, height = height_A, {
  #  multi_corr_plot()
  #})
  
  output$correlation_statistics <- renderTable(
    {
      "
    function that shows the descriptive statistics of the merged data stack in table format
    "
      
      correlation_data <- data_filtered()
      
      #ource(file = paste0(getwd(), "/src/circular_correlations.R"), local = T)
      parameters <- fromJSON(file = "parameters/parameters.json")
      
      condition_col <- input$condition_col
      condition_list <- unlist(unique(correlation_data[condition_col]))
      
      feature_1 <- parameters[input$feature_select_1][[1]][1]
      feature_2 <- parameters[input$feature_select_2][[1]][1]
      
      feature_1_values <- unlist(correlation_data[feature_1])
      
      feature_2_values <- unlist(correlation_data[feature_2])
      
      
      feature_1_name <- parameters[input$feature_select_1][[1]][3]
      feature_2_name <- parameters[input$feature_select_2][[1]][3]
      
      mode_1 <- parameters[input$feature_select_1][[1]][2]
      mode_2 <- parameters[input$feature_select_2][[1]][2]
      
      
      #    threshold <- input$min_nuclei_golgi_dist
      #    if ("organelle_distance" %in% colnames(results_df)){
      #      results_df <- subset(results_df, results_df$distance > threshold)
      #    }
      
      statistics_df <- as.data.frame(matrix(ncol = length(condition_list) + 3, nrow = 0))
      cols <- c("entity")
      for (condition in condition_list) {
        cols <- c(cols, condition)
      }
      cols <- c(cols, "all", "description")
      
      
      colnames(statistics_df) <- cols # c("entity", "value") #, "comment")
      
      res <- compute_correlation(feature_1_values, mode_1, feature_2_values, mode_2) 
      
      ind <- 1
      statistics_df[ind, 1] <- "pearson r-value"
      if ( (mode_1 == "linear") | (mode_2 == "linear") ) {
        statistics_df[ind, "all"] <- res
      } else {
        statistics_df[ind, "all"] <- res$r
      }
      
      
      print("Colnames")
      print(colnames(statistics_df))

        for (condition in condition_list) {
          condition_data <- subset(correlation_data, correlation_data[condition_col] == condition)
          
          feature_1_values <- unlist(condition_data[feature_1])
          feature_2_values <- unlist(condition_data[feature_2])
          
          res <- compute_correlation(feature_1_values, mode_1, feature_2_values, mode_2) 
          
          ind <- 1
          statistics_df[ind, 1] <- "pearson r-value"
          if ( (mode_1 == "linear") | (mode_2 == "linear") ) {
            statistics_df[ind, condition] <- res
          } else {
            statistics_df[ind, condition] <- res$r
          }
          
        }
      statistics_df
      #statistics_df <- summaryStatistics()
      #statistics_df
    },
    digits = 3
  )

  width <- reactive({
    input$plot_width_corr
  })
  height <- reactive({
    input$plot_height_corr
  })

  output$correlation_plot <- renderPlot(width = width, height = height, {
    if (input$corr_plot_option == "spoke plot") {
      p <- spoke_plot_correlation()
    } else {
      p <- plot_correlation()
    }
    p
  })

  spoke_plot_correlation <- reactive({
    parameters <- fromJSON(file = "parameters/parameters.json")

    text_size <- input$text_size_corr

    correlation_data <- data_filtered()

    if (input$spoke_subsample_n > 1) {
      N <- nrow(correlation_data) %/% input$spoke_subsample_n
      if (nrow(correlation_data) > N) {
        correlation_data <- correlation_data[sample(nrow(correlation_data), N), ]
      }
    }

    feature_1 <- input$feature_select_1
    feature_1_name <- feature_1
    if (feature_1 %in% names(parameters)) {
      feature_1 <- parameters[feature_1][[1]][1]
      feature_1_name <- parameters[input$feature_select_1][[1]][3]
    }
    feature_2 <- input$feature_select_2
    feature_2_name <- feature_2
    if(feature_2 %in% names(parameters)) {
      feature_2 <- parameters[feature_2][[1]][1]
      feature_2_name <- parameters[input$feature_select_2][[1]][3]
    }

    #feature_1 <- parameters[input$feature_select_1][[1]][1]
    #feature_2 <- parameters[input$feature_select_2][[1]][1]
    feature_1_values <- unlist(correlation_data[feature_1])
    feature_2_values <- unlist(correlation_data[feature_2])


    #feature_2_name <- parameters[input$feature_select_2][[1]][3]

    # res = circ.cor(feature_1_values, feature_2_values, test=TRUE)

    # reg_coeff <- res$r
    # p_value <- res$p.value

    if (input$circ_units_1 == "radians") {
      feature_1_values_deg <- unlist(correlation_data[feature_1]) * 180.0 / pi
    } else {
      feature_1_values_deg <- unlist(correlation_data[feature_1])
    }
    if (input$circ_units_2 == "radians") {
      feature_2_values_deg <- unlist(correlation_data[feature_2]) * 180.0 / pi
    } else {
      feature_2_values_deg <- unlist(correlation_data[feature_2])
    }
    #feature_1_values_deg <- unlist(correlation_data[feature_1]) * 180.0 / pi
    #feature_2_values_deg <- unlist(correlation_data[feature_2]) * 180.0 / pi

    feature_1_x_a <- list()
    feature_1_y_a <- list()
    feature_1_x_b <- list()
    feature_1_y_b <- list()


    feature_2_x_a <- list()
    feature_2_y_a <- list()
    feature_2_x_b <- list()
    feature_2_y_b <- list()


    print("until here")
    for (i in 1:length(feature_1_values)) {
      #    print(i)
      #    print(feature_1_values[i])
      feature_1_x_a[i] <- 0.5 * cos(feature_1_values[i])
      feature_1_y_a[i] <- 0.5 * sin(feature_1_values[i])
      feature_1_x_b[i] <- 0.5 * cos(feature_1_values[i] + pi)
      feature_1_y_b[i] <- 0.5 * sin(feature_1_values[i] + pi)


      # dist_a <- abs(feature_1_values[i] - feature_2_values[i])
      # dist_b <- abs(feature_1_values[i] - feature_2_values[i] - pi)

      dist_a <- (cos(feature_1_values[i]) - cos(feature_2_values[i])) * (cos(feature_1_values[i]) - cos(feature_2_values[i]))
      dist_a <- dist_a + (sin(feature_1_values[i]) - sin(feature_2_values[i])) * (sin(feature_1_values[i]) - sin(feature_2_values[i]))

      dist_b <- (cos(feature_1_values[i]) - cos(feature_2_values[i] + pi)) * (cos(feature_1_values[i]) - cos(feature_2_values[i] + pi))
      dist_b <- dist_b + (sin(feature_1_values[i]) - sin(feature_2_values[i] + pi)) * (sin(feature_1_values[i]) - sin(feature_2_values[i] + pi))


      if (dist_a < dist_b) {
        feature_2_x_a[i] <- cos(feature_2_values[i])
        feature_2_y_a[i] <- sin(feature_2_values[i])
        feature_2_x_b[i] <- cos(feature_2_values[i] + pi)
        feature_2_y_b[i] <- sin(feature_2_values[i] + pi)
      } else {
        feature_2_x_a[i] <- cos(feature_2_values[i] + pi)
        feature_2_y_a[i] <- sin(feature_2_values[i] + pi)
        feature_2_x_b[i] <- cos(feature_2_values[i])
        feature_2_y_b[i] <- sin(feature_2_values[i])
      }
    }


    f_1 <- data.frame(x1 = unlist(feature_1_x_a), y1 = unlist(feature_1_y_a))
    f_2 <- data.frame(x2 = unlist(feature_2_x_a), y2 = unlist(feature_2_y_a))
    f_1 <- data.frame(x1 = unlist(feature_1_x_b), y1 = unlist(feature_1_y_b))
    f_2 <- data.frame(x2 = unlist(feature_2_x_b), y2 = unlist(feature_2_y_b))


    print(head(f_1))

    p <- ggplot()
    # p <- p + geom_point(aes(x = list(feature_2_x), y = list(feature_2_y), size = 3))
    # p <- p + geom_point(aes(x = list(feature_1_x), y = list(feature_1_y), size = 3))
    # p <- p + geom_point(aes(x = x1, y = y1, size = 3))
    # p <- p + geom_point(aes(x = x2, y = y2, size = 3))
    p <- p + geom_point(aes(x = unlist(feature_2_x_a), y = unlist(feature_2_y_a), size = 3))
    p <- p + geom_point(aes(x = unlist(feature_1_x_a), y = unlist(feature_1_y_a), size = 3))
    p <- p + geom_point(aes(x = unlist(feature_2_x_b), y = unlist(feature_2_y_b), size = 3))
    p <- p + geom_point(aes(x = unlist(feature_1_x_b), y = unlist(feature_1_y_b), size = 3))



    p <- p + geom_segment(aes(x = unlist(feature_1_x_a), y = unlist(feature_1_y_a), xend = unlist(feature_2_x_a), yend = unlist(feature_2_y_a), size = 0.1, color = "red"))
    p <- p + geom_segment(aes(x = unlist(feature_1_x_b), y = unlist(feature_1_y_b), xend = unlist(feature_2_x_b), yend = unlist(feature_2_y_b), size = 0.1, color = "red"))

    p <- p + xlim(-1.0, 1.0)
    p <- p + ylim(-1.0, 1.0)

    p <- p + xlab(feature_1_name) + ylab(feature_2_name)
    p <- p + theme(aspect.ratio = 3 / 3)
    p <- p + geom_point(color = "black", size = input$marker_size_corr)
    # p <- p + theme_minimal(panel.background = element_blank(), base_size = text_size)
    p <- p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
    # p <- ggplot()
    # p <- p + geom_point(aes(x = feature_2_values_deg, y = 1, size = 3))
    # p <- p + geom_point(aes(x = feature_1_values_deg, y = 0.5, size = 3))

    # p <- p + geom_segment(aes(x=feature_1_values_deg, y=0.5, xend=feature_2_values_deg, yend=1.0, size = 0.1, color="red"))

    #        p <- p + ggtitle("spoke_plot") +
    #            theme(plot.title = element_text(size = 18, face = "bold")) +
    #            theme(axis.text.x = element_text(size = 18)) +
    #            coord_polar(start = -pi/2.0, direction = -1) +
    #            scale_x_continuous(limits = c(0, 180),
    #                       breaks = (c(0, 45, 90, 135))) +
    #            scale_x_continuous(limits = c(0, 360),
    #                       breaks = (c(0, 90, 180, 270))) +
    #            scale_y_continuous(limits = c(0, 1.1)) +
    #            theme_minimal(base_size = text_size)
    p
  })

  output$spoke_plot <- renderPlot({
    p <- spoke_plot_correlation()
    p
  })

  output$downloadMergedCorrPlotPDF <- downloadHandler(
    filename <- function() {
      paste("PolarityJaM_Correlation_", Sys.time(), ".pdf", sep = "")
    },
    content <- function(file) {
      pdf(file, width = input$plot_width_corr / 72, height = input$plot_height_corr / 72)
      plot(plot_correlation())
      dev.off()
    },
    contentType = "application/pdf" # MIME type of the image
  )

  output$downloadMergedCorrPlotSVG <- downloadHandler(
    filename <- function() {
      paste("PolarityJaM_Correlation_", Sys.time(), ".svg", sep = "")
    },
    content <- function(file) {
      svg(file, width = input$plot_width_corr / 72, height = input$plot_height_corr / 72)
      plot(plot_correlation())
      dev.off()
    },
    contentType = "application/svg" # MIME type of the image
  )

  output$downloadMergedCorrPlotEPS <- downloadHandler(
    filename <- function() {
      paste("PolarityJaM_Correlation_", Sys.time(), ".eps", sep = "")
    },
    content <- function(file) {
      cairo_ps(file, width = input$plot_width_corr / 72, height = input$plot_height_corr / 72)
      plot(plot_correlation())
      dev.off()
    },
    contentType = "application/eps" # MIME type of the image
  )

  output$downloadMergedCorrPlotPNG <- downloadHandler(
    filename <- function() {
      paste("PolarityJaM_Correlation_", Sys.time(), ".png", sep = "")
    },
    content <- function(file) {
      png(file, width = input$plot_width_corr * 4, height = input$plot_height_corr * 4, res = 300)
      # if (input$data_form != "dataaspixel") plot(plot_data())
      # else plot(plot_map())
      plot(plot_correlation())
      dev.off()
    },
    contentType = "application/png" # MIME type of the image
  )

  output$downloadCorrPlotPDF <- downloadHandler(
    filename <- function() {
      paste("PolarityJaM_Correlation_", Sys.time(), ".pdf", sep = "")
    },
    content <- function(file) {
      pdf(file, width = input$plot_width_corr / 72, height = input$plot_height_corr / 72)
      plot(multi_corr_plot())
      dev.off()
    },
    contentType = "application/pdf" # MIME type of the image
  )
  
  output$downloadCorrPlotSVG <- downloadHandler(
    filename <- function() {
      paste("PolarityJaM_Correlation_", Sys.time(), ".svg", sep = "")
    },
    content <- function(file) {
      svg(file, width = input$plot_width_corr / 72, height = input$plot_height_corr / 72)
      plot(multi_corr_plot())
      dev.off()
    },
    contentType = "application/svg" # MIME type of the image
  )
  
  output$downloadCorrPlotEPS <- downloadHandler(
    filename <- function() {
      paste("PolarityJaM_Correlation_", Sys.time(), ".eps", sep = "")
    },
    content <- function(file) {
      cairo_ps(file, width = input$plot_width_corr / 72, height = input$plot_height_corr / 72)
      plot(multi_corr_plot())
      dev.off()
    },
    contentType = "application/eps" # MIME type of the image
  )
  
  output$downloadCorrPlotPNG <- downloadHandler(
    filename <- function() {
      paste("PolarityJaM_Correlation_", Sys.time(), ".png", sep = "")
    },
    content <- function(file) {
      png(file, width = input$plot_width_corr * 4, height = input$plot_height_corr * 4, res = 300)
      # if (input$data_form != "dataaspixel") plot(plot_data())
      # else plot(plot_map())
      plot(multi_corr_plot())
      dev.off()
    },
    contentType = "application/png" # MIME type of the image
  )


  ### Panel C

  observe({
    condition_col <- input$condition_col
    condition_col <- "filename"

    data <- data_filtered()

    # if (is.data.frame(data)) {
    #TODO: check if condition_col and numeric columns are present in data
    if (length(colnames(data)) > 3) {
      print("Unique condition names: ")
      # print(data[condition_col])
      condition_list <- unique(data[condition_col])
      print(condition_list)
      updateSelectInput(session, "control_condition", choices = condition_list, selected = "filename")
    }
  })



  comparison_plot <- reactive({

    parameters <- fromJSON(file = "parameters/parameters.json")
    text_size <- 12

    datapath <- stack_data_info$datapath
    print(datapath)

    file_list <- list.files(datapath)
    print(file_list)

    i <- 1
    angle_dists <- list()
    file_names <- list()
    polarity_indices <- list()
    angle_mean_degs <- list()

    results_all_df <- data_filtered()

    # feature <- parameters[input$feature_select][[1]][1]
    feature <- parameters[input$feature_comparison][[1]][1]
    condition_col <- input$condition_col

    condition_list <- unlist(unique(results_all_df[condition_col]))
    # plist <- vector('list', length(unique(results_all_df$filename)))
    plist <- vector("list", length(condition_list))
    print("length of plot list")
    print(plist)
    print("list of unique entries")
    print(unlist(unique(results_all_df[condition_col])))

    x_lim <- c(min(results_all_df[feature]), max(results_all_df[feature]))
    # for(file_name in unique(results_all_df$filename)) {
    #  results_df <- subset(results_all_df, results_all_df$filename == file_name )
    for (file_name in condition_list) {
      results_df <- subset(results_all_df, results_all_df[condition_col] == file_name)

      # values <- compute_polarity_index(results_df)


      # print(values)
      # polarity_index <- values[["polarity_index"]]
      # angle_mean_deg <- values[["angle_mean_deg"]]

      x <- unlist(results_df[feature])
      angle_dists[[i]] <- x


      # if (parameters[input$feature_select][[1]][2] == "linear") {
      #
      #    if (x_lim[0] > min(x))
      #        x_lim[0] <- min(x)
      #    if (x_lim[1] < max(x))
      #        x_lim[1] <- max(x)
      #
      #       }

      file_names[[i]] <- file_name

      i <- i + 1
    }



    n <- length(angle_dists)
    nCol <- floor(sqrt(n))

    bin_size <- 360 / input$bins

    plotseries <- function(i) {
      angle_dist <- angle_dists[[i]]
      file_name <- file_names[[i]]
      # polarity_index <- polarity_indices[[i]]
      # angle_mean_deg <- angle_mean_degs[[i]]

      # results_df <- subset(results_all_df, results_all_df$filename == file_name)
      results_df <- subset(results_all_df, results_all_df[condition_col] == file_name)

      plot_title <- file_name

      if (nchar(file_name) > 37) {
        max_fl <- 17
        file_name_end <- substr(file_name, nchar(file_name) - max_fl + 1, nchar(file_name))
        file_name_start <- substr(file_name, 1, max_fl)
        plot_title <- paste0(file_name_start, "...", file_name_end)
      }
      # if (nchar(file_name) > 15) {
      #    plot_title <- paste0("image #",toString(i))
      #    print(paste0("filename: ",file_name," too long, will be replaced by",plot_title))
      # }


      if (input$stats_mode == "directional") {
        statistics <- compute_directional_statistics(results_df, feature, parameters)
        # statistics <- compute_polarity_index(unlist(results_df[feature]))
        if (input$circ_units == "radians") {
          x_data <- unlist(results_df[feature]) * 180.0 / pi
        } else {
          x_data <- unlist(results_df[feature])
        }
        #x_data <- unlist(results_df[feature]) * 180.0 / pi
        
        print(paste0("Length of filename", toString(i)))

        p <- rose_plot_circular(parameters, input, statistics, x_data, plot_title, i, text_size)
      } else if (input$stats_mode == "axial") {
        
        statistics <- compute_axial_statistics(results_df, feature, parameters)

        x_data <- results_df[feature]
        if (input$circ_units == "radians") {
          x_data <- unlist(transform_axial(input, x_data)) * 180.0 / pi
        }
        else {
          x_data <- unlist(transform_axial(input, x_data))
        }
        

        p <- rose_plot_axial(parameters, input, statistics, x_data, plot_title, i, text_size)
      } else {
        x_data <- unlist(results_df[feature])
        statistics <- compute_linear_statistics(results_df, feature, parameters)
        # plot_title <- file_name
        # p <- linear_histogram(parameters, input, statistics, x_data,  plot_title, i, text_size, x_lim[0], x_lim[1])
        p <- linear_histogram(parameters, input, statistics, x_data, plot_title, i, text_size, min(results_all_df[feature]), max(results_all_df[feature]))
      }
    }


    myplots <- lapply(1:length(angle_dists), plotseries)

    # print(myplots)
    grid.arrange(grobs = myplots, nrow = nCol) # , widths = list(10,10))
  })


  output$comparison_plot <- renderPlot({

    # source(file = paste0(getwd(),"/src/plot_functions.R"), local=T)
    # source(file = paste0(getwd(),"/src/circular_statistics.R"), local=T)

    # parameters <- fromJSON(file = "parameters/parameters.json")
    # text_size <- as.integer(parameters["text_size_merged_plot"])
    comparison_plot()
  })


  comparisonStatistics <- reactive({
    "
        reactive function that reads a stack of spreadsheet and returns a data frame 
        with descriptive statistics including circular mean, circular standard deviation 
        and nearest neighbours for the merged stack of data
        "

    data <- data_filtered()

    condition_col <- input$condition_col
    control_condition <- input$control_condition
    condition_list <- unlist(unique(data[condition_col]))

    #source(file = paste0(getwd(), "/src/plot_functions.R"), local = T)
    #source(file = paste0(getwd(), "/src/circular_statistics.R"), local = T)

    parameters <- fromJSON(file = "parameters/parameters.json")


    feature <- parameters[input$feature_comparison][[1]][1]

    res <- data.frame(matrix(ncol = length(condition_list) + 1, nrow = 0))
    cols <- c("Test", "Control")

    for (condition in condition_list) {
      if (condition == control_condition) {
        next
      }
      cols <- c(cols, condition)
    }
    # cols <- append(c("Test"),condition_list)

    colnames(res) <- cols
    res[1, "Test"] <- "WatsonU2"
    res[1, "Control"] <- control_condition

    print("data frame")
    print(res)
    # colnames(res) <- append(c("Test"),condition_list)

    # res[1,"Test"] <- "WatsonU2"


    for (condition in condition_list) {
      if (condition == control_condition) {
        next
      }

      print("Control condition:")
      print(control_condition)
      print("Condition:")
      print(condition)


      condition_data <- subset(data, data[condition_col] == condition)
      control_data <- subset(data, data[condition_col] == control_condition)
      print("Output Watson test object")
      # print(watson.two(condition_data$organelle_orientation_rad, control_data$organelle_orientation_rad, alpha=0.05, plot=TRUE))

      # print("Struct of Watson test object")
      # print(str(watson.two(condition_data$organelle_orientation_rad, control_data$organelle_orientation_rad, alpha=0.05, plot=TRUE)))
      condition_values <- unlist(condition_data[feature])
      control_values <- unlist(control_data[feature])
      # watson1 <- watson.two.test(condition_data$organelle_orientation_rad, control_data$organelle_orientation_rad)
      watson1 <- watson.two.test(condition_values, control_values)
      out <- capture.output(watson.two.test(condition_values, control_values))
      print(out)
      p_value <- out[5]
      res[1, condition] <- p_value
      print("The resulting data frame:")
      print(res)
      # watson1 <- array(as.matrix(unlist(watson1)), dim=c(5, 1))
      # res <- if (watson1[1,]>0.187) (0.04) else (0.06)  # Critical Value: 0.187 #this is just to have number higher and one lower than 0.05 (see coding below)
      # print("Extract")
      # print(watson1)
      # print(res)
    }

    # inFileCondition_1 <- input$condition_1
    # inFileCondition_2 <- input$condition_2

    # if (is.null(inFileCondition_1))
    #    return(NULL)
    # if (is.null(inFileCondition_2))
    #    return(NULL)

    # print(inFileCondition_1$datapath)
    # print(inFileCondition_2$datapath)
    # cond1_data <- read.csv(inFileCondition_1$datapath, header = input$header_cond1)
    # cond2_data <- read.csv(inFileCondition_2$datapath, header = input$header_cond2)

    # print(watson.two(cond1_data$organelle_orientation_rad, cond2_data$organelle_orientation_rad, alpha=0.05, plot=TRUE))

    # watson.two(data1, data2)
    # watson.two(cond1_data$organelle_orientation_rad, cond2_data$organelle_orientation_rad)

    # print("Structure")
    # print(str(watson.two(cond1_data$organelle_orientation_rad, cond2_data$organelle_orientation_rad)))

    # statistics_df <- data.frame()
    # statistics_df
    res
  })

  output$CDFPlot <- renderPlot({
    "
      function that shows the descriptive statistics in table format
      "
    inFileCondition_1 <- input$condition_1
    inFileCondition_2 <- input$condition_2

    if (is.null(inFileCondition_1)) {
      return(NULL)
    }
    if (is.null(inFileCondition_2)) {
      return(NULL)
    }

    print(inFileCondition_1$datapath)
    print(inFileCondition_2$datapath)
    cond1_data <- read.csv(inFileCondition_1$datapath, header = input$header_cond1)
    cond2_data <- read.csv(inFileCondition_2$datapath, header = input$header_cond2)

    watson.two(cond1_data$organelle_orientation_rad, cond2_data$organelle_orientation_rad, alpha = 0.05, plot = TRUE)
  })


  output$comparison_statistics <- renderTable({
    "
    function that shows the descriptive statistics in table format
    "

    statistics_df <- comparisonStatistics()
    statistics_df
  })

  ### END OF COMPARISON TAB ###

  ### Panel D: Terms of Use ###

  output$terms_of_use_text <- renderUI({
    if ((input$data_upload_form == "upload data") & (input$terms_of_use == FALSE)) {
      tags$iframe(src = "about/Terms-of-Use.html", height = "500px", width = "100%")
    } else {
      tags$div(style = "display:none")
    }
  })

  output$terms_of_use_text_all <- renderUI({
    "
    function that the merged stack of polarity data and angles in table format
    "
    tags$iframe(src = "about/Terms-of-Use.html", height = "500px", width = "100%")
    #includeHTML("Terms-of-Use.html")
  })

  ### Panel E: About ##

  #' @returns logo, used for About panel
  output$support_logo <- renderImage({

    filename <- normalizePath(file.path('collaboration_logo_small.png'))

    # Return a list containing the filename
    list(src = filename,
         #width = 608, height = 67,
         alt = "supported by DZHK, Helmholtz Imaging, Leducq Foundation and Max Delbrück Center",
         deleteFile = FALSE
         )
  }, deleteFile = FALSE)
}

# Run the application
shinyApp(ui = ui, server = server, options = list(port = opt$p, host = "127.0.0.1"))
