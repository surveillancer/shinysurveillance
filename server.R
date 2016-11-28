
library(shiny)
library(surveillance)
library(ggplot2)
library(plotly)
library(dplyr)
library(purrr)

sts_to_df <- function(algorithm, sts) {
  data.frame(
    algorithm,
    epoch = epoch(sts, as.Date = TRUE),
    observed = as.numeric(observed(sts)),
    alarm = as.logical(alarms(sts)),
    state = as.logical(sts@state),
    upperbound = as.numeric(upperbound(sts))
  )
}

shinyServer(function(input, output) {

  algo_range <- reactive({
    (nrow(time_series()) - as.numeric(input$range_min)):nrow(time_series())
  })

  algorithms <- list(
    "ears" = function(sts) {
      surveillance::earsC(time_series(), control = list(alpha = input$ears_alpha,
                                                        method = input$ears_method,
                                                        range = algo_range()))
    },
    "farringtonflexible" = function(sts) {
      surveillance::farringtonFlexible(time_series(), control = list(
        alpha = input$farringtonflexible_alpha,
        b = input$farringtonflexible_b,
        w = input$farringtonflexible_w,
        range = algo_range()
      ))
    },
    "glrnb" = function(sts) {
      surveillance::glrnb(time_series(), control = list(
        "c.ARL" = input$glrnb_c_ARL,
        ret = "cases",
        theta = 1.2,
        range = algo_range()
      ))
    }
  )

  # data
  time_series <- reactive({
    ds_env <- new.env()
    data(list = input$dataset, envir = ds_env)
    dataset <- as.list(ds_env)[[input$dataset]]
    if (class(dataset) == "disProg") {
      surveillance::disProg2sts(dataset)
    } else {
      dataset
    }
  })

  surv_ts <- reactive({
    base_data <- sts_to_df("tmp", time_series()) %>%
      distinct(epoch, observed)
    purrr::map(input$algorithms, function(algo) {
      base_data %>% left_join(sts_to_df(algo, algorithms[[algo]]()),
                              by = c("epoch", "observed")) %>%
        mutate(algorithm = algo)
    }) %>% bind_rows
  })

  output$mainPlot <- plotly::renderPlotly({
    plot_data <- surv_ts()
    alarm_data <- plot_data %>%
      filter(alarm)
    outbreak_data <- plot_data %>%
      filter(state == TRUE)
    saveRDS(plot_data, file = "plot_data.rds")
    plotly::ggplotly(ggplot(plot_data, aes(x = epoch, y = observed)) +
      geom_bar(stat = "identity") +
      geom_line(aes(y = upperbound), color = "lightblue", linetype = "dotted") +
        facet_wrap(~ algorithm) +
        #geom_point(data = outbreak_data, na.rm = TRUE, y = 0, color = "black", shape = "cross") +
        geom_point(data = alarm_data, color = "red", na.rm = TRUE, y = 0)
    )
  })

})
