

library(shiny)
library(ggplot2)

score_clf = function( y, pred_prob ) {
  y = y[ order(pred_prob, decreasing = T)]
  pred_prob = pred_prob[ order(pred_prob, decreasing = T)]
  #TPR and FPR
  tpr = cumsum(y)/sum(y)
  fpr = cumsum(!y)/sum(!y)
  precision = cumsum(y)/(1:length(y))
  #calculate ROC
  tpr_tmp = tpr[ order(fpr) ]
  fpr_tmp = fpr[ order(fpr) ]
  dx = diff( fpr_tmp )
  y = 0.5*( tpr_tmp[-1]+tpr_tmp[-length(tpr_tmp)] )
  auc = sum( dx*y )
  
  f1 = 2*tpr*precision/(tpr+precision)
  
  ibest_f1 = which.max( f1 )
  best_threshold_f1 = pred_prob[ibest_f1]
  #return score
  score = list( f1=f1, tpr=tpr, fpr=fpr, auc=auc,
                pred_prob=pred_prob, y=y, precision=precision, recall=tpr,
                best_threshold_f1=best_threshold_f1, best_f1=f1[ibest_f1],
                best_recall=tpr[ibest_f1], best_precision=precision[ibest_f1])
  score
}


# Define UI for application that draws a histogram
ui = fluidPage(# Application title
  titlePanel("ROC Simulator"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "nrow",
        "Data Size",
        choices = list( '100'=100, '1000'=1000, '10000'=10000 ),
        selected = 1000
      ),
      numericInput(
        "inc_rate",
        "Incident Rate",
        min=0,
        max=1,
        step=0.002,
        value = 0.05
      ),
      sliderInput(
        "target_mu",
        "Location of Target Distribution",
        min = 0,
        max = 1,
        value = 1
      ),
      sliderInput(
        "target_sd",
        "SD of Target Distribution",
        min = 0,
        max = 1,
        value = 0.2
      )
    , width=4 ),
    
    # Show a plot of the generated distribution
    mainPanel( fillRow( plotOutput("plot_dist", width=300, height=280),
                  plotOutput("plot_roc", width=300, height=280), flex=c(1,1), height=280 ),
               fillRow( plotOutput("plot_pr", width=300, height=280),
                  plotOutput("plot_pr_pred", width=300, height=280), flex=c(1,1), height=280 )
    , width=8 )
  ))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  datainput = reactive({
    validate(
      need(input$inc_rate<=1, 'ERROR: Incident Rate<=1.0'),
      need(input$inc_rate>0, 'ERROR: Incident Rate<=0')
    )

    set.seed(1)
    n = as.integer( input$nrow )
    inc_rate = input$inc_rate
    ninc = floor( n*inc_rate )
    predprob = seq(0, 1, length = n)
    p = dnorm(predprob, mean = input$target_mu, sd = input$target_sd)
    ind = sample(1:n, ninc, prob = p)
    y = rep(0,n)
    y[ind] = 1
    res = score_clf( y, predprob )
    # print( paste(n, ninc, tail(res$precision,1) ) )
    res
  })
  
  output$plot_dist = renderPlot({
    validate(
      need(input$inc_rate<=1, 'ERROR: Incident Rate>1'),
      need(input$inc_rate>0, 'ERROR: Incident Rate<=0')
    )
    x = seq(0, 1, length = as.integer( input$nrow ) )
    y = dnorm(x, mean = input$target_mu, sd = input$target_sd)
    ggplot() + geom_line(aes(x = x, y = y)) +
      scale_x_continuous(name = "Predicted Probability",limits = c(0, 1) ) +
      scale_y_continuous(name="Density",limits = c(0, max(y) ) )
  } )
  output$plot_pr = renderPlot({
    res = datainput()
    ggplot() + geom_line(aes(x = res$recall, y = res$precision) ) +
      geom_hline( yintercept=tail(res$precision,1), color='red'  ) +
      scale_x_continuous(name="Recall",limits = c(0, 1) ) +
      scale_y_continuous(name="Precision",limits = c(0, 1) )
  } )
  output$plot_roc = renderPlot({
    res = datainput()
    ggplot() + geom_line( aes(x = res$fpr, y = res$tpr) ) +
      geom_line( aes(x = c(0,1), y = c(0,1) ), color='lightgrey' ) +
      scale_x_continuous(name=paste("FPR (AUC:", format(res$auc, digits=4), ")" ), limits = c(0, 1) ) +
      scale_y_continuous(name="TPR",limits = c(0, 1) )
  } )
  output$plot_pr_pred = renderPlot({
    res = datainput()
    ggplot() + geom_line(aes(x = res$pred_prob, y = res$precision), color='skyblue' ) +
      geom_line(aes(x = res$pred_prob, y = res$recall), color='green' ) +
      scale_x_continuous(name="Predicted Probability",limits = c(0, 1) ) +
      scale_y_continuous(name="Recall/Precision",limits = c(0, 1) )
  } )
}

# Run the application
shinyApp(ui = ui, server = server)
