#' @title Graph Data
#' @description Creates an object of class \code{"iGraph"} by transforming event level data into network data.
#' @param data dataframe of purchase history
#' @param id column name identifying the user id
#' @param product_id column name identifying the id of the objects purchased
#' @return an object of class \code{"iGraph"}
#' @importFrom dplyr across group_by summarize rename
#' @importFrom  igraph make_graph
#' @export

graph_data <- function(data = df, id = "user_id", product_id = "product_id") {
  check_inputs(data, id, product_id)

  data_no_missing <- na.omit(data)

  grouped <- dplyr::group_by(data_no_missing, dplyr::across(id))
  user_df <- dplyr::summarize(grouped, dplyr::across(product_id, list))
  user_df <- dplyr::rename(user_df, nodes = product_id)

  user_df$num_nodes <- sapply(user_df$nodes, length)
  user_df$unique <- sapply(lapply(user_df$nodes, unique), length)

  non_repeated <- subset(user_df, user_df$num_nodes > 1 & user_df$unique > 1)

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
      } else if(length(unique(node_list[j:len])) == 1){
        nodes <- c(nodes, node)
      } else{
        nodes <- c(nodes, node, node)
      }
    }
    verts <- c(verts, nodes)
  }
  verts <- as.character(verts)
  g = igraph::make_graph(verts)
  g
}

#Ensure inputs are specified
check_inputs <- function(data, id, product_id) {
  if(missing(data) | is.null(data)){
    stop("\n'data' must be a dataframe")
  }
  if(missing(id) | is.null(id)){
    stop("\n'id' must be a vector of user ids")
  }
  if(missing(product_id) | is.null(product_id)){
    stop("\n'product_id' myst be a vector of product ids")
  }
  TRUE
}



