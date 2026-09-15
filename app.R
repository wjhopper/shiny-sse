library(shiny)
library(ggplot2)

ui <- fluidPage(
  
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
     #data table, #total {
        font-size: 2em !important;
      }")),
    tags$link(rel="shortcut icon", href="favicon.png")
  ),
  
  titlePanel("Modeling The Central Tendency"),
  
  column(6,
         plotOutput("hist", click = "plot_click"),
         plotOutput("error")
         ),
  column(6,
         tableOutput("data"),
         textOutput("total")
         )
)

server <- function(input, output, session) {
  
  dataset <- read.csv("ncbirths.csv")
  dataset <- dataset[!is.na(dataset$weight), ]
  
  weight_sd <- sd(dataset$weight)
  
  display_data <- dataset[c(1:9, nrow(dataset)), "weight", drop=FALSE]
  display_data$`Estimate` <- ""
  display_data$`Error` <- ""
  display_data$`Squared Error` <- ""
  display_data$`Observation #` <- c(1:9, nrow(dataset))
  display_data <- display_data[, c(5,1,2,3,4)]
  
  state <- reactiveValues()
  state$guesses <- numeric(0)
  state$errors <- numeric(0)
  
  error_metric <- function(x) {
    (dataset$weight - x)^2 
  }
  
  observe({
    state$guesses <- c(isolate(state$guesses), input$plot_click$x)
    
    if (!is.null(input$plot_click$x)) { 
      state$errors <- c(isolate(state$errors),
                        sum( error_metric(input$plot_click$x) )
                        )
      }
  })
  
  output$hist <- renderPlot({

    n_guesses <- length(state$guesses)
    
    ggplot(data = dataset,
           mapping = aes(x = weight)
           ) +
      geom_histogram(bins = 30) +
      # geom_histogram(aes(y = after_stat(density)), bins = 30) +
      geom_vline(xintercept = state$guesses[n_guesses],
                 color = "red"
                 ) +
      # geom_function(fun = \(x) {
      #                 dnorm(x,
      #                       mean = if (n_guesses == 0) { NA } else {state$guesses[n_guesses] },,
      #                       sd = weight_sd
      #                       )
      #               },
      #               color = "red"
      #               ) +
      scale_x_continuous(
        breaks = seq(2, 12, by = 2),
        limits = range(dataset$weight) + c(-1, 1),
        oob = function(x, limits) { x }
        ) +
      scale_y_continuous(
        "\nCount",
        breaks = c(       0,         50,        100,      150),
        labels = c("      0", "     50",  "    100", "    150")
      ) + 
      theme_gray(22) # +
      # theme(
      #   axis.title.y = element_blank(),
      #   axis.text.y = element_blank(),
      #   axis.ticks.y = element_blank()
      # )
  })
  
  output$error <- renderPlot({
    
    ggplot(data = data.frame(x = state$guesses, y = state$errors),
           mapping = aes(x = x, y = y)
           ) +
      geom_point(size = 2) +
      geom_line() +
      scale_x_continuous(limits = range(dataset$weight) + c(-1, 1),
                         breaks = seq(2, 12, by = 2)
                         ) +
      scale_y_continuous("Total Squared Error\nBetween Estimate and Data",
                         limits = c(sum(error_metric(mean(dataset$weight))),
                                    max(sum(error_metric(min(dataset$weight) - 1)),
                                        sum(error_metric(max(dataset$weight) + 1))
                                        )
                                    ),
                         labels = scales::comma_format()
                         ) +
      # xlab(expression(paste("Estimate of ", mu))) +
      xlab("Estimate of Birth Weight Central Tendency") +
      theme_gray(22)
  })
  
  output$data <- renderTable({
    
    if (length(state$guesses) != 0) {
      display_data[[3]] <- round(state$guesses[length(state$guesses)], 3)
      display_data[[4]] <- round(display_data[[2]] - display_data[[3]], 3)
      display_data[[5]] <- round(display_data[[4]]^2, 3)
    }
    
    ellipses <- as.list(rep("...", ncol(display_data)))
    names(ellipses) <- names(display_data)
    
    rbind(display_data[c(1:9), ],
          ellipses,
          display_data[10, ]
          )
    }
  )
  
  output$total <- renderText({
    i <- length(state$errors)
    if (i != 0) { 
      x <- state$errors[i]
    } else {
      x <- ""
    }
    paste("Total Squared Error =", format(x, digits=6, big.mark=","))
  })
}

shinyApp(ui, server) 
