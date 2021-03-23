#' @title Graph Data
#' @description Creates an object of class \code{"iGraph"} by transforming event level data into network data.
#' @param data dataframe of purchase history
#' @param id column name identifying the user id
#' @param product_id column name identifying the id of the objects purchased
#' @return an object of class \code{"iGraph"}
#' @importFrom dplyr across group_by summarize rename
#' @importFrom  igraph make_graph
#' @export

graph_data <- function(data, id, product_id){
  check_inputs(data, id, product_id) #Verify that the inputs are present

  data_no_missing <- na.omit(data) #Remove missing values from the data

  grouped <- dplyr::group_by(data_no_missing, dplyr::across(id)) #Group the data by user
  user_df <- dplyr::summarize(grouped, dplyr::across(product_id, list)) #Create one row per user
  user_df <- dplyr::rename(user_df, nodes = product_id)

  user_df$num_nodes <- sapply(user_df$nodes, length) #Count the number of nodes for each user
  user_df$unique <- sapply(lapply(user_df$nodes, unique), length) #Count the number of unique nodes for each user

  non_repeated <- subset(user_df, user_df$unique > 1) #Only keep data where the number of unique nodes is greater than 1

  verts <- c() #Initialize an empty vector for the vertices
  for(i in 1:nrow(non_repeated)){ #Loop through every individual
    node_list <- non_repeated$nodes[[i]] #Set the list of nodes for individual i
    nodes <- c(node_list[1]) #Initiate the vector of nodes that point to each other with the first item
    len <- length(node_list)
    for(j in 2:len){ #Loop through all nodes after the first one
      node <- node_list[j]
      prior <- node_list[j - 1]
      if(node == prior){ #Ignore repeats, nodes cannot point to themselves
        next
      } else if(length(unique(node_list[j:len])) == 1){ #If at the end or the last unique node, only add it to the vector of nodes twice
        nodes <- c(nodes, node)
      } else{ #Add all other nodes twice, once for the previous node to point to it, and once for it to point to the next node in the list
        nodes <- c(nodes, node, node)
      }
    }
    verts <- c(verts, nodes) #Add the nodes of individual i to the greater vector of vertices
    cat(round(i / nrow(non_repeated) * 100, 2), "%    \r") #Print the percent of users who have been looped through
  }
  verts <- as.character(verts) #Convert the vertices to a vector
  g = igraph::make_graph(verts) #Generate an igraph object from the vertices
  g #Return the igraph object
}

#Ensure inputs are specified
check_inputs <- function(data, id, product_id){
  if(missing(data) | !is.data.frame(data)){ #Check if the data is missing and a dataframe
    stop("\n'data' must be a dataframe")
  }
  if(missing(id)){ #Check if the vector of user ids is missing
    stop("\n'id' must be a vector of user ids")
  }
  if(missing(product_id)){ #Check if the vector of product ids is missing
    stop("\n'product_id' must be a vector of product ids")
  }
  TRUE
}



