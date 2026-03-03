# Install pacman if not already installed
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
# Use p_load to install and load other packages
pacman::p_load(openxlsx2,sf,rmapshaper,
               tidycensus,tigris,lehdr,
               units,eia,DescTools,dplyr,car,ggplot2,insight)
