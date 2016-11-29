
library(shiny)
library(surveillance)
library(ggplot2)
library(plotly)
library(dplyr)
library(purrr)
library(viridis)

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
    min_range <- pmax(0, nrow(time_series()) - as.numeric(input$range_min))
    min_range:nrow(time_series())
  })

  algorithms <- reactiveValues()
  log <- reactiveValues()

  output$errors <- renderText({
    if (!is.null(log[["error"]])) {
      log[["error"]]
    }
  })

  # ears
  observeEvent({input$update_algos; time_series()}, {
    result <- purrr::safely(~surveillance::earsC(time_series(), control = list(alpha = input$ears_alpha,
                                                      method = input$ears_method,
                                                      range = algo_range())))()
    if (!is.null(result$result)) {
      algorithms[["ears"]] <- result$result
    } else {
      algorithms[["ears"]] <- time_series()[1, ]
      log[["error"]] <- result$error$message
      log[["error"]] <- paste("EARSC:", result$error$message)
    }
  })

  # farringtonflexible
  observeEvent({input$update_algos; time_series()}, {
    result <- purrr::safely(~surveillance::farringtonFlexible(time_series(), control = list(
      alpha = input$farringtonflexible_alpha,
      b = input$farringtonflexible_b,
      w = input$farringtonflexible_w,
      pastWeeksNotIncluded = input$farringtonflexible_pastWeeksNotIncluded,
      powertrans = input$farringtonflexible_powertrans,
      range = algo_range()
    )))()
    if (!is.null(result$result)) {
      algorithms[["farringtonflexible"]] <- result$result
    } else {
      algorithms[["farringtonflexible"]] <- time_series()[1, ]
      log[["error"]] <- paste("farringtonflexible:", result$error$message)
    }
  })

  # glrnb
  observeEvent({input$update_algos; time_series()}, {
    result <- purrr::safely(~surveillance::glrnb(time_series(), control = list(
      "c.ARL" = input$glrnb_c_ARL,
      ret = "cases",
      theta = 1.2,
      mu0 = list(S = 1, trend = input$glrnb_trend),
      range = algo_range()
    )))()
    if (!is.null(result$result)) {
      algorithms[["glrnb"]] <- result$result
    } else {
      algorithms[["glrnb"]] <- time_series()[1, ]
      log[["error"]] <- paste("glrnb:", result$error$message)
    }
  })

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
      base_data %>% left_join(sts_to_df(algo, algorithms[[algo]]),
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
    plotly::ggplotly(ggplot(plot_data, aes(x = epoch, y = observed)) +
      geom_bar(stat = "identity", color = "#123456") +
      geom_line(aes(y = upperbound), color = "orange") +
        facet_wrap(~ algorithm) +
        xlab("ISO week") +
        ylab("# cases") +
        #geom_point(data = outbreak_data, na.rm = TRUE, y = 0, color = "black", shape = "cross") +
        geom_point(data = alarm_data, color = "red", na.rm = TRUE, y = 0)
    )
  })

})
