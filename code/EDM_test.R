# This 'EDM_test.R' file quickly go through the whole process of running rEDM on a test dataset, before using the real data. The test dataset is from Chang et al. 2021 Ecol Lett (Ricker model). Please refer to Chang et al. on how the data set is generated. The test dataset used is in 'data/result20191024_0_0_0_.csv'.  
# For real data, the analysis is conducted in "EDM.R" file
# check version of the installed rEDM package
packageVersion("rEDM") # note Chang et al. 2021 Ecol Lett use v1.2.3. Other versions may be incompatible. 

seed <- 49563
set.seed(seed)

# load intact functions from Chang et al. 
source('code/Demo_MDR_function.R')  

# load subset data of 'result20191024_0_0_0_.csv' from Chang et al. 2021. The original data contain 201 rows and 1001 columns (time steps and number of species). The subset data contains 50 rows and 6 columns only for testing purpose: 50 time steps (801-850) and 6 variables (V1-V6, species abundance). First column is time.

# load original dataset
do <- read.csv('data/result20191024_0_0_0_.csv',header=T,stringsAsFactors = F)
dim(do)

da.range <- 1:50 # Subsample for data analysis
cv.unit <- 0.025 
ptype <- 'aenet'
out.sample <- T # T/F for out-of-sample forecast
nout <- 2  # number of out-of-sample. 


dot <- do[da.range,1] # get time column
do <- do[da.range,-1] # remove the first column (time) from the data frame

# take only the first 6 columns (species abundance) for testing purpose
do <- do[,1:6] 

dto <- apply(do,1,sum) # sum of each row (time step) in the data frame
dpo <- do*repmat(dto^-1,1,ncol(do)) # normalize the rows of the matrix do so that each row sums to 1(?) This is often done in data processing to scale the data and make comparisons across different rows more meaningful.

# check and shall see all values in the rowsum column equal to 1. 
as.data.frame(dpo) %>% mutate(rowsum = rowSums(across(everything())))


# Exclusion of rare species. 
# Here we skip this step otherwise all species will be omitted.
# We shall consider the followings for real dataset. Threshold is upon decision.
# pcri <- 0;bcri <- 10^-3;
# doind2 <- (apply(dpo,2,mean,na.rm=T)>(bcri))&((apply(do>0,2,sum,na.rm=T)/nrow(do))>pcri)
# exsp2 <- setdiff(1:ncol(do),which(doind2))

# do <- do[,-exsp2]

nsp <- ncol(do) # number of species
ndo <- nrow(do) # number of time steps
nin <- ndo-nout # number of in-sample time steps

# Mean and SD of each node
do.mean <- apply(do[1:nin,],2,mean,na.rm=T) # mean of each column (species abundance) in the first 48 rows (in-sample)
do.sd <- apply(do[1:nin,],2,sd,na.rm=T) # sd of each column (species abundance) in the first 48 rows (in-sample)

# scale the data
dosdM <- repmat(c(do.sd)^-1,1,nsp)*repmat(c(do.sd),nsp,1) # diagonal matrix of the inverse of the standard deviation of each column (species abundance) in the first 48 rows (in-sample)


