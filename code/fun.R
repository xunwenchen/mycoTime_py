# Function to plot network for a given time
plot_network_for_time <- function(data, time) {
  # Filter data for the specified time and Insample = 1
  net <- subset(data, time == time & Insample == 1)
  net_v1_6 <- net[, c("V1", "V2", "V3", "V4", "V5", "V6")]
  
  # Convert to matrix
  net_m <- as.matrix(net_v1_6)
  
  # Build directed graph
  g <- graph_from_adjacency_matrix(
    net_m,
    mode = "directed",
    weighted = TRUE,
    diag = TRUE  # Keep self-loops
  )
  
  g_f <- simplify(g, remove.multiple = FALSE, remove.loops = TRUE)
  
  # Customize plot using ring style, then remove self-loops
  plot(g_f,
       edge.width = abs(E(g)$weight) * 3,  # Scale edge width
       edge.color = ifelse(E(g)$weight < 0, "darkred", "darkgreen"),
       edge.arrow.size = 0.6,
       vertex.size = 50,
       vertex.color = "lightgray",
       vertex.label.cex = 0,
       vertex.label.color = "black",
       layout = layout_in_circle(g),  # Alternative: layout_with_fr(g)
       main = paste("Time =", time, "(Red = Neg., Green = Pos.)"))
}


                               
