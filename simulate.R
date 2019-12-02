library(RSiena)

network0 <- read.csv("network0.csv")

# convert row in network CSV to adjacency matrix
# adj_row: vector from slicing data frame
# n: # players (treat LAPTOP as extra player P0)
# player_names: used for indexing into network CSV
row_to_adj_mat <- function(adj_row, n, player_names) {
  stopifnot(n+1 == length(player_names))
  
  adj_mat <- matrix(0, nrow = n+1, ncol = n+1, 
                    dimnames = list(player_names, player_names))
  
  for (row_player in player_names) {
    if (row_player == "LAPTOP")  # no LAPTOP_TO_P* observations
      next
    for (col_player in player_names) {
      adj_row_var <- paste(row_player, "TO", col_player, sep = "_")  # column name in CSV file
      adj_mat[row_player, col_player] <- adj_row[1, adj_row_var]
    }
  }
  
  return(adj_mat)
}

# testing
n <- 7
player_names <- c("LAPTOP", "P1", "P2", "P3", "P4", "P5", "P6", "P7")
adj_mats <- array(NA, dim = c(n+1, n+1, nrow(network0)),
                  dimnames = list(player_names, player_names, NULL))
for (i in 1:nrow(network0)) {
  adj_mats[,,i] <- row_to_adj_mat(network0[i,], n, player_names)
}

network0_siena <- sienaDependent(adj_mats) 
data_siena <- sienaDataCreate(network0_siena)
