#' @keywords internal
"_PACKAGE"
# install the latest version of the Epi R Handbook package
pacman::p_install_gh("appliedepi/epirhandbook")
# load the package for use
pacman::p_load(epirhandbook)
# download the offline handbook to your computer
download_book()
# download all the example data into a folder on your computer
get_data("all")

# download only the linelist example data into a folder on your computer
get_data(file = "linelist_cleaned.rds")

library(readr)
likert_data <- read_csv("data/likert_data.csv")
View(likert_data)
