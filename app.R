library(shiny)
library(ggplot2)

ui <- fluidPage(
  
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
     #data table, #total {
        font-size: 2em !important;
      }"))
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
  
  display_data <- dataset[c(1:9, nrow(dataset)), "weight", drop=FALSE]
  display_data$`Model Value` <- ""
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

    ggplot(data = dataset,
           mapping = aes(x = weight)
           ) +
      geom_histogram(bins = 30) +
      geom_vline(xintercept = state$guesses[length(state$guesses)],
                 color = "red"
                 ) +
      scale_x_continuous(limits = range(dataset$weight) + c(-1, 1),
                         oob = function(x, limits) { x }
                         ) +
      theme_gray(22)
  })
  
  output$error <- renderPlot({
    
    ggplot(data = data.frame(x = state$guesses, y = state$errors),
           mapping = aes(x = x, y = y)
           ) +
      geom_point() +
      geom_line() +
      scale_x_continuous("Model Value",
                         limits = range(dataset$weight) + c(-1, 1)
                         ) +
      scale_y_continuous("Squared Error",
                         limits = c(sum(error_metric(mean(dataset$weight))),
                                    max(sum(error_metric(min(dataset$weight) - 1)),
                                        sum(error_metric(max(dataset$weight) + 1))
                                        )
                                    )
                         ) +
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
    paste("Total Error =", format(x, digits=6, big.mark=","))
  })
}

shinyApp(ui, server) 
