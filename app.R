library(shiny)
library(visNetwork)

# Define the barplot creation function
create_barplot <- function(adj_matrix, i) {
  # Extract the ith row (i's relationships with others)
  row_data <- adj_matrix[i, -i]
  
  # Create a data frame for ggplot
  data <- data.frame(
    variable = colnames(adj_matrix)[-i],
    coefficient = as.numeric(row_data)
  )
  
  # Create the ggplot bar plot
  ggplot(data, aes(x = variable, y = coefficient)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    geom_text(aes(label = round(coefficient, 2)), vjust = -0.5, color = "black", size = 5) +
    labs(
      title = paste("Relationships from", colnames(adj_matrix)[i]),
      y = "Coefficient",
      x = "Variables"
    ) +
    ylim(-1, 1) +  # Adjust y-axis limits
    theme_minimal()
}



ui <- fluidPage(
  visNetworkOutput("network"),
  actionButton("get_positions", "Get Node Positions"),
  # ggplot Barplot for clicked node
  plotOutput("barplot")  # This is where the ggplot2 plot will appear
)



server <- function(input, output, session) {
  # Define nodes and edges
  nodes <- data.frame(id = 1:3, label = c("A", "B", "C"))
  edges <- data.frame(from = c(1, 1), to = c(2, 3))
  
  # Create the adjacency matrix with coefficients
  adj_matrix <- matrix(c(
    0, 0.5, -0.3,  # A to A, A to B, A to C
    0, 0, 0,     # B to A, B to B, B to C
    0, 0, 0        # C to A, C to B, C to C
  ), nrow = 3, ncol = 3, byrow = TRUE)
  
  # Assign row and column names to the matrix
  rownames(adj_matrix) <- c("A", "B", "C")
  colnames(adj_matrix) <- c("A", "B", "C")
  output$network <- renderVisNetwork({
    visNetwork(nodes, edges) %>%
      visEdges(arrows = 'to') %>%  # Add arrows to indicate direction
      visEvents(click = "function(event) {
      var pos = event.pointer.canvas;
      Shiny.setInputValue('mouse_position', pos);
      var nodeId = event.nodes[0];  // Get clicked node ID
      if (nodeId) {
        Shiny.setInputValue('clicked_node', nodeId);
      }
    }")
  })
  
  # Display the positions of the nodes
  observeEvent(input$get_positions, {
    network_positions <- input$mouse_position
    print(network_positions)
  })
  
  # Render clicked node info
  output$clicked_node <- renderPrint({
    adj_matrix[input$clicked_node,-input$clicked_node]
  })
  
  output$barplot <- renderPlot(    
    {node_id <- input$clicked_node
    if (!is.null(node_id)) {
      create_barplot(adj_matrix, node_id)
    }  })
  
  # Render mouse position info
  output$positions <- renderPrint({
    input$mouse_position
  })
}

shinyApp(ui, server)
