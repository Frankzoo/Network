library(shiny)
library(visNetwork)
library(ggplot2)
library(dplyr)

# Function to find all paths between a cause and effect node
find_paths <- function(edges, cause, effect) {
  adj_list <- split(edges$to, edges$from)
  
  dfs <- function(current, path) {
    path <- c(path, current)
    
    if (current == effect) {
      return(list(paste(path, collapse = " → ")))
    }
    
    all_paths <- list()
    
    if (!is.null(adj_list[[current]])) {
      for (neighbor in adj_list[[current]]) {
        if (!(neighbor %in% path)) {  # Avoid cycles
          all_paths <- c(all_paths, dfs(neighbor, path))
        }
      }
    }
    
    return(all_paths)
  }
  
  return(dfs(cause, c()))
}

# Define the barplot creation function
create_barplot_effect <- function(adj_matrix, i) {
  # Extract the ith row (i's relationships with others)
  row_data <- adj_matrix[i, -i]
  
  # Filter out zero coefficients
  non_zero_indices <- which(row_data != 0)
  if (length(non_zero_indices) == 0) {
    return(NULL)  # Return nothing if all coefficients are zero
  }
  
  # Create a data frame for ggplot and sort by coefficient value
  data <- data.frame(
    variable = colnames(adj_matrix)[-i][non_zero_indices],
    coefficient = as.numeric(row_data[non_zero_indices])
  ) %>%
    dplyr::arrange(desc(coefficient))  # Sort in descending order
  
  # Create the ggplot bar plot
  ggplot(data, aes(x = reorder(variable, -coefficient), y = coefficient)) +
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

# Define the barplot creation function
create_barplot_cause <- function(adj_matrix, i) {
  # Extract the ith row (i's relationships with others)
  col_data <- adj_matrix[-i, i]
  
  # Filter out zero coefficients
  non_zero_indices <- which(col_data != 0)
  if (length(non_zero_indices) == 0) {
    return(NULL)  # Return nothing if all coefficients are zero
  }
  
  # Create a data frame for ggplot and sort by coefficient value
  data <- data.frame(
    variable = colnames(adj_matrix)[-i][non_zero_indices],
    coefficient = as.numeric(col_data[non_zero_indices])
  ) %>%
    dplyr::arrange(desc(coefficient))  # Sort in descending order
  
  # Create the ggplot bar plot
  ggplot(data, aes(x = reorder(variable, -coefficient), y = coefficient)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    geom_text(aes(label = round(coefficient, 2)), vjust = -0.5, color = "black", size = 5) +
    labs(
      title = paste("Relationships to", colnames(adj_matrix)[i]),
      y = "Coefficient",
      x = "Variables"
    ) +
    ylim(-1, 1) +  # Adjust y-axis limits
    theme_minimal()
}


adj_matrix_strength*adj_matrix_causal_pc

# Define your adjacency matrix
adj_matrix <-read.csv("adj_martix.csv")

node_names <- c("Gender","Age","Education","Growing up in happy family","Father's education","Mother's education","Family well off from age 6-16","Lived with both parent from age 6-16","Health status from age 6-16","Experienced violent event from age 6-16","Witnessed violent event from age 6-16",
                "Cognitive ability","Depression","Anxiety","Loneliness","Sleep quality","Testosterone","DHEA","Estradiol","Progesterone","Systolic blood pressure","Diastolic blood pressure","C-reactive protein DBS", "Total hemoglobin",
                "Smoke frequency","Drink frequency","Asleep time","Wake up time","Sex frequency","Phsical activitiy frequency","Friend support","Family support","Partner support","Coummunity solidarity","Neighborhood unsafety","Someone close died in past 5 years","Age at first pregnancy or fathered child","Number of children")


colnames(adj_matrix) <- node_names
rownames(adj_matrix) <- node_names

# Define your nodes
nodes <- data.frame(
  id = 1:nrow(adj_matrix),
  label = rownames(adj_matrix)
)
nodes
# Define your edges
edges <- as.data.frame(which(adj_matrix_causal_pc == 1, arr.ind = TRUE))

edges_withlabel <- data.frame(
  from = rownames(adj_matrix)[edges$row],
  to = rownames(adj_matrix)[edges$col]
)

edges <- data.frame(
  from = edges$row,
  to = edges$col
)

# Define UI
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("cause", "Select Cause:", choices = node_names),
      selectInput("effect", "Select Effect:", choices = node_names),
      actionButton("find_paths", "Find Paths"),
      verbatimTextOutput("path_results")
    ),
    mainPanel(
      fluidRow(
        column(6, visNetworkOutput("network", height = "1000px")),
        column(6, 
               plotOutput("barplot_effect", height = "500px"),
               plotOutput("barplot_cause", height = "500px")
        )
      ),
      actionButton("get_positions", "Get Node Positions")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  output$network <- renderVisNetwork({
    visNetwork(nodes, edges) %>%
      visNodes(size = 20) %>%  # Set smaller node size
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)%>%
      visPhysics(
        solver = "barnesHut",
        barnesHut = list(
          gravitationalConstant = -10000, 
          centralGravity = 0.1,           
          springLength = 300              
        )
      )%>%
      visEdges(arrows = 'to') %>%
      visEvents(click = "function(event) {
      var nodeId = event.nodes[0];
      if (nodeId) {
        Shiny.setInputValue('clicked_node', nodeId, {priority: 'event'});
      }
    }")
  })
  
  output$barplot_effect <- renderPlot({
    node_id <- input$clicked_node
    if (!is.null(node_id)) {
      create_barplot_effect(adj_matrix, node_id)
    }
  })
  
  output$barplot_cause <- renderPlot({
    node_id <- input$clicked_node
    if (!is.null(node_id)) {
      create_barplot_cause(adj_matrix, node_id)
    }
  })
  
  observeEvent(input$find_paths, {
    cause <- input$cause
    effect <- input$effect
    
    if (cause != effect) {
      paths <- find_paths(edges_withlabel, cause, effect)
      output$path_results <- renderText({
        if (length(paths) == 0) {
          "No causal paths found."
        } else {
          paste("Causal Paths:\n", paste(paths, collapse = "\n"))
        }
      })
    } else {
      output$path_results <- renderText("Cause and Effect must be different nodes.")
    }
  })
}

# Run the app
shinyApp(ui, server)
