shinyUI(bootstrapPage(

  sliderInput(inputId = "beta0",
      label = "Ordenada al origen (beta0)",
      min=-5, max=5, step=0.1,
      value = -3),

   sliderInput(inputId = "beta",
      label = "Pendiente (beta1)",
      min=-3, max=5, step=0.05,
      value = 0),
 
verbatimTextOutput("rss"),
  plotOutput(outputId = "main_plot", height = "200px"),
    plotOutput(outputId = "contour", height = "200px")


))