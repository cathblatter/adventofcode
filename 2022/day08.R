# pkg
library(dplyr)
library(purrr)

# get input
input <- readLines("2022/day08-input.txt") |> 
  strsplit(split = "")

# modify input
input <- as.data.frame(do.call(rbind, input)) |> 
  mutate(across(.cols = everything(), 
                .fns = ~as.numeric(.)))

# count dimensions
ncols <- ncol(input)
nrows <- nrow(input)

# calculate number of outside and inside tress
outside_trees <- (2*nrows)+((ncols-2)*2)
inside_trees <- prod(dim(input)) - outside_trees

# set_up position-table for tree lookup
row_pos <- 2:(nrows-1)
col_pos <- 2:(ncols-1)

tree_pos <- tidyr::expand_grid(row_pos, col_pos)

# cleanup the environment
rm(row_pos, col_pos, ncols, nrows)


#' Check tree value by its position and get the values of its surrounding trees

#' @description Function to check every tree position and map the trees 
#' at each of its sides. This function is used as backend for the following
#' functions
#' 
#' @param i integer row index (tree position)
#' @param j integer col index (tree position)
#' @param dat default = input
#'
#' @return a list with five elements (tar, tar_top, tar_bot, tar_left, tar_right)
#' @export
#'
check_position <- function(i, j, dat = input){
  
  tar = dat[i, j]
  tar_top = rev(dat[1:(i-1), j])
  tar_bot = dat[(i+1):nrow(dat), j]
  tar_left = rev(as.integer(dat[i, 1:(j-1)]))
  tar_right = as.integer(dat[i, (j+1):ncol(dat)])
  
  tibble::lst(tar, tar_top, tar_bot, tar_left, tar_right)
  
}


#' Check visibility of a tree from all sides by comparing their values
#' 
#' @description Function to check the visibility of tree - 
#' a tree is visible from a given side, if all( .x < .y), meaning
#' all trees on whatever side (top, bottom, left, right) are smaller
#' than the given tree
#'
#' @param i integer row index (tree position)
#' @param j integer col index (tree position)
#' @param dat default = input
#'
#' @return
#' @export
#'
#' @examples
check_vis <- function(i, j, dat = input){
  
  pos = check_position(i, j, dat = dat) %>% 
    map2(.x = ., .y = .$tar, ~all(.x < .y))

  map_dfr(pos[-1], sum) %>% 
    mutate(vis = rowSums( . > 0))

  }

# check_vis(4,2)



## Part 1 ## 

# map check_vis() for all tree indices, count number of trees that are visible...
visisble_inside_trees <- 
  map2_dfr(tree_pos$row_pos, tree_pos$col_pos, check_vis) %>% 
  filter(vis > 0) %>% nrow()

# ...and sum together with the outside trees
sum(outside_trees, visisble_inside_trees) # result 1785


## Part 2

#' Check the visual distance of a tree (scenic score = prod of all views)
#'
#' @description check_position remains the same, but check_vis needs to be adapted
#'  to have a number of the visual distance of each tree.
#'  the visual distance is the distance until you reach a tree that is ≥ than the
#'  index tree - 
#'  thus by putting the min(which(x)) or - if this is integer(0)//infitnite putting the length of .x
#'  then calculate the product
#'
#'
#' @param i integer row index (tree position)
#' @param j integer col index (tree position)
#'
#' @return
#' @export
#'
check_vis_distance <- function(i, j, dat = input){
  
  vis_dist = check_position(i, j, dat = dat) %>% 
    map2(.x = ., .y = .$tar, 
      ~ifelse(
        is.infinite(min(which(.x >= .y))),
          length(.x), 
            min(which(.x >= .y)))
      )
  
  # vis_dist[-1] to omit the $tar value (val of the index tree)
  # calculate the scenic score as product of all distances per tree
  prod(do.call(rbind, vis_dist[-1]))
  
}

# finally, get the hightest visible scenic score possible
max(map2_dbl(tree_pos$row_pos, tree_pos$col_pos, check_vis_distance)) #345'168
