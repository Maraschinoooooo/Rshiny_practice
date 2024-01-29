library(shiny)
library(DESeq2)
library(ggplot2)

ui <- fluidPage(
  titlePanel("RNA-seq Analysis Tool"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose RNA-seq File', accept = c('.csv', '.txt')),
      # Assuming the user provides a separate file for colData
      fileInput('colDataFile', 'Choose Column Data File', accept = c('.csv', '.txt')),
      textInput('designFormula', 'Design Formula', value = "~ condition"),
      actionButton("submit", "Analyze")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("MA Plot", plotOutput("maPlot")),
        tabPanel("Volcano Plot", plotOutput("volcanoPlot")),
        tabPanel("PCA Plot", plotOutput("pcaPlot")),
        tabPanel("Results Table", dataTableOutput("resultsTable"))
      )
    )
  )
)

server <- function(input, output) {
  
  # Reactive expression for dataset
  datasetInput <- reactive({
    req(input$file1)
    inFile <- input$file1
    read.csv(inFile$datapath, row.names = 1)
  })
  
  # Reactive expression for column dataset
  colDataInput <- reactive({
    req(input$colDataFile)
    inFile <- input$colDataFile
    read.csv(inFile$datapath)
  })
  
  # DESeq2 analysis upon button click
  observeEvent(input$submit, {
    res <- reactive({
      req(datasetInput(), colDataInput())
      dds <- DESeqDataSetFromMatrix(countData = datasetInput(), 
                                    colData = colDataInput(), 
                                    design = as.formula(input$designFormula))
      dds <- DESeq(dds)
      results(dds)
    })
    
    # MA Plot
    output$maPlot <- renderPlot({
      req(res())
      ggplot(as.data.frame(res()), aes(x = baseMean, y = log2FoldChange)) +
        geom_point(aes(color = padj < 0.05), alpha = 0.4) +
        scale_color_manual(values = c("black", "red")) +
        labs(title = "MA Plot", x = "Average Expression", y = "Log2 Fold Change") +
        theme_minimal()
    })
    
    # Volcano Plot
    output$volcanoPlot <- renderPlot({
      req(res())
      ggplot(as.data.frame(res()), aes(x = log2FoldChange, y = -log10(pvalue))) +
        geom_point(aes(color = padj < 0.05), alpha = 0.4) +
        scale_color_manual(values = c("black", "red")) +
        labs(title = "Volcano Plot", x = "Log2 Fold Change", y = "-Log10 P-Value") +
        theme_minimal()
    })
    
    # PCA Plot
    output$pcaPlot <- renderPlot({
      rld <- rlog(res(), blind = TRUE)
      pcaData <- plotPCA(rld, intgroup = c("condition"), returnData = TRUE)
      percentVar <- round(100 * attr(pcaData, "percentVar"))
      
      ggplot(pcaData, aes(x = PC1, y = PC2, color = condition)) +
        geom_point(size = 3) +
        xlab(paste0("PC1: ", percentVar[1], "% variance")) +
        ylab(paste0("PC2: ", percentVar[2], "% variance")) +
        ggtitle("PCA Plot") +
        theme_bw() +
        theme(legend.position = "bottom")
    })
    
    # Results Table
    output$resultsTable <- renderDataTable({
      req(res())
      as.data.frame(res())
    })
  })
}

shinyApp(ui = ui, server = server)
