### Function to convert co-ordinates to bottom left corner to allow dataframes to be merged together
# function from Fiona Bell
mroundOG <- function(x,base){
  base*floor(x/base)
}