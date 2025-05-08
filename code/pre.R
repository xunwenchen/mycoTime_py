# load packages ----
rm(list = ls())
# install the following packages if not already installed
# install.packages(c("dplyr", "ggplot2", "agricolae", "ggpubr", "vegan",
#                    "rEDM", "igraph", "quantreg", "doParallel", "parallel",
#                    "foreach", "Kendall", "MASS", "glmnet"))
# Since rEDM v1.2.3 is needed for reproducibility. We install it from local directory. rEDM v1.2.3 can be downloaded, but we uploaded to folder 'packages'.
# install rEDM v1.2.3 (rEDM_1.2.3.tar.gz) from local directory 'packages' if you have not done so.
# install.packages("packages/rEDM_1.2.3.tar.gz", repos = NULL, type = "source")


my_packages <- c("dplyr", "ggplot2", "agricolae", "ggpubr", "vegan", "rEDM", 
               "igraph", "quantreg", "doParallel", "parallel", "foreach", 
               "Kendall", "MASS", "glmnet")
lapply(my_packages, require, character.only = TRUE)

# source own functions if any ----
source("code/fun.R")

# set ggplot theme ----
theme_set(theme_bw())

# set sequential colors for plotting ----
two_colors <- c("darkgray", "indianred")
four_colors <- c("darkgray", "indianred", "lightblue", "lightgreen")

# The following lines switch on or off whether save/do the analyses or not.
# Set all to TRUE if run for the first time.
save <- F # set to 'FALSE' if you don't want to save the plots
do.CCM <- T           # set if do CCM
do.multiview <- T     # set if do multiview
do.MDR.CV <- T        # set if do MDR CV
