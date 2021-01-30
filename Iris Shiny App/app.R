# Load packages ----
library(shiny)
library(tree)

# Data -------------
data(iris)
head(iris)

set.seed(42)

indexes <- sample(
  x = 1:150,
  size = 100
)

indexes

train <- iris[indexes, ]
test <- iris[-indexes, ]

model <- tree(
  #formula = Species ~ .,
  formula = Species ~ Petal.Length + Petal.Width,
  data = train
)

speciesOptions = unique(iris$Species)

# User interface ----
ui <- fluidPage(
  titlePanel("Know Your Irisses"),
  
  sidebarLayout(

    sidebarPanel(
      
      h3("Iris Species Predictor"),
      p("Select length and width of the petals"),
      sliderInput(
        inputId = "petal.length",
        label = "Petal Length (cm)",
        min = 1,
        max = 8,
        step = 0.5,
        value = 4
      ),
      sliderInput(
        inputId = "petal.width",
        label = "Petal Width (cm)",
        min = 0.0,
        max = 2.5,
        step = 0.5,
        value = 1.5
      ),
      textOutput(outputId = "prediction"),
      plotOutput(outputId = "plot")
    ),
    
    mainPanel(
      selectInput("selectedSpecies", "Select the Species", speciesOptions),
      h1(textOutput(outputId = "header")),
      imageOutput("speciesImage"),
      varSelectInput("feature", "Select the feature", Filter(is.numeric, iris), multiple = FALSE),
      plotOutput("histogram"),
      plotOutput(outputId = "scatterplot")
      
    )
  )
)

# Server logic ----------
server <- function(input, output) {
  output$plot = renderPlot({
    plot(
      x = iris$Petal.Length,
      y = iris$Petal.Width,
      pch = 19,
      col = palette()[as.numeric(iris$Species)],
      main = "Iris Petal Length vs Width",
      xlab = "Petal Length (cm)",
      ylab = "Petal Width (cm)",
    )
    points(
      x = input$petal.length,
      y = input$petal.width,
      col = "red",
      pch = 4,
      cex = 2,
      lwd = 2
    )
  })
  
  prediction <- reactive({
    #Create predictors
    predictors <- data.frame(
      Petal.Length = input$petal.length,
      Petal.Width = input$petal.width,
      Sepal.Length = 0,
      Sepal.Width = 0
    )
    # make predictions
    predict(
      object = model,
      newdata = predictors,
      type = "class"
    )
  })
  output$prediction = renderText({
    paste("The predicted species is Iris", as.character(prediction()))
    
  })

  output$header = renderText({
    paste(
      "Iris ",
      as.character(input$selectedSpecies)
    )
  })
  
  output$speciesImage = renderImage({
    
    list(src = paste("images/", input$selectedSpecies, ".jpg", sep = ""),
         contentType = 'image/jpg',
         width = 400,
         alt = paste("Iris", input$selectedSpecies))
  }, deleteFile=FALSE)
  
  speciesSubset <- reactive({
    return(subset(iris, Species == input$selectedSpecies))
    
  })
  
  featureNumber <- reactive({
    return(which( colnames(iris)== input$feature))
    
  })
  
  output$histogram <- renderPlot({
    hist(as.numeric(speciesSubset()[,featureNumber()]), 
         col = "#75AADB", 
         border = "white", 
         xlab = paste(input$feature, "(cm)"), 
         main = paste("Histogram of", input$feature, "of", input$selectedSpecies)
         )
  }) 

  output$scatterplot = renderPlot({
    plot(
      x = speciesSubset()$Sepal.Length,
      y = speciesSubset()$Petal.Length,
      pch = 19,
      main = "Iris Petal Length vs Sepal Length",
      xlab = "Sepal Length (cm)",
      ylab = "Petal Length (cm)",
    )
    model <- lm(Petal.Length ~ Sepal.Length, data=speciesSubset())
    abline(model, col="red", lwd=3)
  })  
    
}

# Run the app -------------------
shinyApp(ui, server)
