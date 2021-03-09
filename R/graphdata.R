#' @title Graph Data
#' @description Creates an object of class \code{"iGraph"}
#' @param data dataframe of purchase history
#' @param id column name identifying the user id
#' @param product_id column name identifying the id of the objects purchased
#' @return an object of class \code{"iGraph"}
#' @export

graph_data <- function(data = df, id = "user_id", product_id = "product_id") {
  #attach(data)
  #id <-  data[, id]
  #product_id <- data[, product_id]
  data_no_missing <- na.omit(data)

  grouped <- group_by(data_no_missing, across(id))
  #head(grouped)
  #length(unique(grouped$id))
  user_df <- summarize(grouped, across(product_id, list))
  user_df <- rename(user_df, nodes = product_id)
  #colnames(user_df)[]
  #user_df

  user_df$num_nodes <- sapply(user_df$nodes, length)
  user_df$unique <- sapply(lapply(user_df$nodes, unique), length)
  #sapply(sapply(user_df$nodes, unique), length)
  #user_df

  non_repeated <- subset(user_df, user_df$num_nodes > 1 & user_df$unique > 1)
  rm(user_df)
  non_repeated

  verts <- c()
  for(i in 1:nrow(non_repeated)){
    cat(round(i / nrow(non_repeated) * 100, 2), "%    \r")
    node_list <- non_repeated$nodes[[i]]
    nodes <- c(node_list[1])
    len <- length(node_list)
    for(j in 2:len){
      node <- node_list[j]
      prior <- node_list[j - 1]
      if(node == prior){
        next
      } else if(j == len | (length(unique(node_list[j:len]))) == 1){
        nodes <- c(nodes, node)
      } else{
        nodes <- c(nodes, node, node)
      }
    }
    verts <- c(verts, nodes)
  }
  #rm(i, j, len, node, nodes, num_nodes)

  verts <- as.character(verts)
  g = make_graph(verts)
  g
}


