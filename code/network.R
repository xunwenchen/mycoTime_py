# using the jcof dataset, we can plot network for each time

net.df <- smap.demo[['jcof']]
# Time 1 network ----
# take time = 1, Insample = 1, and V1-V6
net1 <- subset(data, time == 1 & Insample == 1)
net1_v1_6 <- net1[, c("V1", "V2", "V3", "V4", "V5", "V6")]
# Convert to matrix
net1_m <- as.matrix(net1_v1_6)

# Build directed graph
g <- graph_from_adjacency_matrix(
  net1_m,
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
     main = "Time = 1 (Red = Neg., Green = Pos.)")

# Time 2 network ----
# take time = 2, Insample = 1, and V1-V6
net2 <- subset(net.df, time == 2 & Insample == 1)
net2_v1_6 <- net2[, c("V1", "V2", "V3", "V4", "V5", "V6")]
# Convert to matrix
net2_m <- as.matrix(net2_v1_6)
# Build directed graph
g2 <- graph_from_adjacency_matrix(
  net2_m,
  mode = "directed",
  weighted = TRUE,
  diag = TRUE  # Keep self-loops
)
g2_f <- simplify(g2, remove.multiple = FALSE, remove.loops = TRUE)
# Customize plot using ring style, then remove self-loops
plot(g2_f,
     edge.width = abs(E(g2)$weight) * 3,  # Scale edge width
     edge.color = ifelse(E(g2)$weight < 0, "darkred", "darkgreen"),
     edge.arrow.size = 0.6,
     vertex.size = 50,
     vertex.color = "lightgray",
     vertex.label.cex = 0,
     vertex.label.color = "black",
     layout = layout_in_circle(g2),  # Alternative: layout_with_fr(g)
     main = "Time = 2 (Red = Neg., Green = Pos.)")

# Time 3 network ----
# take time = 3, Insample = 1, and V1-V6
net3 <- subset(net.df, time == 3 & Insample == 1)
net3_v1_6 <- net3[, c("V1", "V2", "V3", "V4", "V5", "V6")]
# Convert to matrix
net3_m <- as.matrix(net3_v1_6)
# Build directed graph
g3 <- graph_from_adjacency_matrix(
  net3_m,
  mode = "directed",
  weighted = TRUE,
  diag = TRUE  # Keep self-loops
)
g3_f <- simplify(g3, remove.multiple = FALSE, remove.loops = TRUE)
# Customize plot using ring style, then remove self-loops
plot(g3_f,
     edge.width = abs(E(g3)$weight) * 3,  # Scale edge width
     edge.color = ifelse(E(g3)$weight < 0, "darkred", "darkgreen"),
     edge.arrow.size = 0.6,
     vertex.size = 50,
     vertex.color = "lightgray",
     vertex.label.cex = 0,
     vertex.label.color = "black",
     layout = layout_in_circle(g3),  # Alternative: layout_with_fr(g)
     main = "Time = 3 (Red = Neg., Green = Pos.)")


# Time 4 network ----
# take time = 4, Insample = 1, and V1-V6
net4 <- subset(net.df, time == 4 & Insample == 1)
net4_v1_6 <- net4[, c("V1", "V2", "V3", "V4", "V5", "V6")]
# Convert to matrix
net4_m <- as.matrix(net4_v1_6)
# Build directed graph
g4 <- graph_from_adjacency_matrix(
  net4_m,
  mode = "directed",
  weighted = TRUE,
  diag = TRUE  # Keep self-loops
)
g4_f <- simplify(g4, remove.multiple = FALSE, remove.loops = TRUE)
# Customize plot using ring style, then remove self-loops
plot(g4_f,
     edge.width = abs(E(g4)$weight) * 3,  # Scale edge width
     edge.color = ifelse(E(g4)$weight < 0, "darkred", "darkgreen"),
     edge.arrow.size = 0.6,
     vertex.size = 50,
     vertex.color = "lightgray",
     vertex.label.cex = 0,
     vertex.label.color = "black",
     layout = layout_in_circle(g4),  # Alternative: layout_with_fr(g)
     main = "Time = 4 (Red = Neg., Green = Pos.)")

# Time 5 network ----
# take time = 5, Insample = 1, and V1-V6
net5 <- subset(net.df, time == 5 & Insample == 1)
net5_v1_6 <- net5[, c("V1", "V2", "V3", "V4", "V5", "V6")]
# Convert to matrix
net5_m <- as.matrix(net5_v1_6)
# Build directed graph
g5 <- graph_from_adjacency_matrix(
  net5_m,
  mode = "directed",
  weighted = TRUE,
  diag = TRUE  # Keep self-loops
)
g5_f <- simplify(g5, remove.multiple = FALSE, remove.loops = TRUE)
# Customize plot using ring style, then remove self-loops
plot(g5_f,
     edge.width = abs(E(g5)$weight) * 3,  # Scale edge width
     edge.color = ifelse(E(g5)$weight < 0, "darkred", "darkgreen"),
     edge.arrow.size = 0.6,
     vertex.size = 50,
     vertex.color = "lightgray",
     vertex.label.cex = 0,
     vertex.label.color = "black",
     layout = layout_in_circle(g5),  # Alternative: layout_with_fr(g)
     main = "Time = 5 (Red = Neg., Green = Pos.)")
# Time 6-47 are not plotted.



