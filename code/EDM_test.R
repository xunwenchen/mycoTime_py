# This 'EDM_test.R' file quickly go through the whole process of running rEDM on a test dataset, before using the real data. The test dataset is from Chang et al. 2021 Ecol Lett (Ricker model). Please refer to Chang et al. on how the data set is generated. The test dataset used is in 'data/result20191024_0_0_0_.csv'.  
# For real data, the analysis is conducted in "EDM.R" file

# Preparation ----
# check version of the installed rEDM package
packageVersion("rEDM") # note Chang et al. 2021 Ecol Lett use v1.2.3. Other versions may be incompatible. 

seed <- 49563
set.seed(seed)

# load intact functions from Chang et al. 
source('code/Demo_MDR_function.R')  

# load subset data of 'result20191024_0_0_0_.csv' from Chang et al. 2021. The original data contain 201 rows and 1001 columns (time steps and number of species). The subset data contains 50 rows and 6 columns only for testing purpose: 50 time steps (801-850) and 6 variables (V1-V6, species abundance). First column is time.


# Load original dataset ----
da.range <- 1:50 # Subsample for data analysis
out.sample <- T # T/F for out-of-sample forecast
if(out.sample){nout <- 2}else{nout <- 0}  # number of out-of-sample

# First look at the first 6 species over 200 time steps ----
df <- read.csv('data/result20191024_0_0_0_.csv',header=T,stringsAsFactors=F)
ggplot(data.frame(df), aes(x=1:nrow(df))) +
  geom_line(aes(y=V1, color="V1")) +
  geom_line(aes(y=V2, color="V2")) +
  geom_line(aes(y=V3, color="V3")) +
  geom_line(aes(y=V4, color="V4")) +
  geom_line(aes(y=V5, color="V5")) +
  geom_line(aes(y=V6, color="V6")) +
  labs(title = "Rick model (first 200*6)",
       x = "Time",
       y = "Abundance") +
  scale_color_manual(name = "Species", values = c("V1" = "red", "V2" = "blue", "V3" = "green", "V4" = "purple", "V5" = "orange", "V6" = "pink"))



(da.name <- 'model1024_0_0_0')
do <- read.csv('data/result20191024_0_0_0_.csv',header=T,stringsAsFactors=F)
dot <- do[da.range,1] # get time column
do <- do[da.range,-1] # remove the first column (time) from the data frame
ndo <- nrow(do) # number of time steps
nin <- ndo-nout # library sample size



# take only the first 6 columns (species abundance) for testing purpose
do <- do[,1:6] 

# ~~~~ plot logistic map ----
# plot the normalized in-sample dataset at time t. Include V1 to V6.
ggplot(data.frame(do), aes(x=1:nrow(do))) +
  geom_line(aes(y=V1, color="V1")) +
  geom_line(aes(y=V2, color="V2")) +
  geom_line(aes(y=V3, color="V3")) +
  geom_line(aes(y=V4, color="V4")) +
  geom_line(aes(y=V5, color="V5")) +
  geom_line(aes(y=V6, color="V6")) +
  labs(title = "Ricker model (first 50*6)",
       x = "Time",
       y = "Abundance") +
  # change legend title to "Species"
  scale_color_manual(name = "Species", values = c("V1" = "red", "V2" = "blue", "V3" = "green", "V4" = "purple", "V5" = "orange", "V6" = "pink")) 

# For real data, we may have 30 time steps and 1000 'species' for each treatment with the same matrix structure. The 'species' can be ASV/OTU ID, taxon (at phylum, order, class, family, genus, or species level), depending on nature of data and research objectives. Also, we may focus on overlapping 'species' across treatments, if we want to compare the results across treatments.

dto <- apply(do,1,sum) # sum of each row (time step) in the data frame
dpo <- do*repmat(dto^-1,1,ncol(do)) # normalize the rows of the matrix 'do' so that each row sums to 1(?). This is often done in data processing to scale the data and make comparisons across different rows more meaningful. Similar to using rarefied 16S rRNA gene sequencing data(?).
# repmat() is the own function by sourcing 'Demo_MDR_function.R'. 
# repmat() makes a matrix with repeated column or row.

# check and shall see all values in the rowsum column equal to 1. 
as.data.frame(dpo) %>% mutate(rowsum = rowSums(across(everything())))


# Exclusion of rare species ----
# Here we skip this step otherwise all species will be omitted.
# We shall consider the followings for real dataset.
# *Threshold is upon decision*.
# pcri <- 0;bcri <- 10^-3; # criteria for selecting species based on proportion of presnece (pcri) and mean relative abundance (bri) 
# doind2 <- (apply(dpo,2,mean,na.rm=T)>(bcri))&((apply(do>0,2,sum,na.rm=T)/nrow(do))>pcri) # index for selected species 
# exsp2 <- setdiff(1:ncol(do),which(doind2))   # index for rare species 
# do <- do[,-exsp2]                            # Dataset excluded rare species


(nsp <- ncol(do)) # number of species
ndo <- nrow(do) # number of time steps
nin <- ndo-nout # number of in-sample time steps

# Mean and SD of each node/species ----
do.mean <- apply(do[1:nin,],2,mean,na.rm=T) # mean of each column (species abundance) in the first 48 rows (in-sample)
do.sd <- apply(do[1:nin,],2,sd,na.rm=T) # sd of each column (species abundance) in the first 48 rows (in-sample)

# Construct a sd(i,j) matrix ----
# (dimension = nsp*nsp, i.e., 6*6 here).
# if sd(i,j)>1, meaning j varies more than i
# if sd(i,j)<1, meaning i varies more than j
# if sd(i,j)=1, meaning i and j vary the same
dosdM <- repmat(c(do.sd)^-1,1,nsp)*repmat(c(do.sd),nsp,1) 

# check the sd(i,j)
dosdM; dim(dosdM)

# In-sample ----
d <- do[1:(nin-1),] # In-sample dataset at time t (time 1-47)
d_tp1 <- do[2:(nin),] # In-sample dataset at time t+1 (time 2-48)

# ~~ normalization (z-score) ----
ds <- (d-repmat(do.mean,nrow(d),1))*repmat(do.sd,nrow(d),1)^-1 # Normalized in-sample dataset at time t (i.e., *z-score = (x-mean)/sd)
ds_tp1 <- (d_tp1-repmat(do.mean,nrow(d_tp1),1))*repmat(do.sd,nrow(d_tp1),1)^-1 # Normalized in-sample dataset at time t+1 (i.e., z-score)

# ~~~~ plot logistic map ----
# plot the normalized in-sample dataset at time t. Include V1 to V6.
ggplot(data.frame(ds), aes(x=1:nrow(ds))) +
  geom_line(aes(y=V1, color="V1")) +
  geom_line(aes(y=V2, color="V2")) +
  geom_line(aes(y=V3, color="V3")) +
  geom_line(aes(y=V4, color="V4")) +
  geom_line(aes(y=V5, color="V5")) +
  geom_line(aes(y=V6, color="V6")) +
  labs(title = "Normalized In-sample Dataset at Time t",
       x = "Time",
       y = "Normalized Abundance") +
  scale_color_manual(name = "Species", values = c("V1" = "red", "V2" = "blue", "V3" = "green", "V4" = "purple", "V5" = "orange", "V6" = "pink")) 

# we can do t+1 as well, but since the pattern is the same so we skip. 
# but we can do lagged point plot for t and t+1, for V1
plot(d[,1],d_tp1[,1],xlab='t',ylab='t+1',main='Lagged Point Plot for V1/species 1')


# Out-sample ----
if(out.sample|nout!=0){
  d.test <- do[nin:(ndo-1),]                 # Out-of-sample dataset at time t 
  dt_tp1 <- do[(nin+1):ndo,]                 # Out-of-sample dataset at time t+1
  ds.test <- (d.test-repmat(do.mean,nrow(d.test),1))*repmat(do.sd,nrow(d.test),1)^-1 # Normalized out-of-sample dataset at time t
  dst_tp1 <- (dt_tp1-repmat(do.mean,nrow(dt_tp1),1))*repmat(do.sd,nrow(dt_tp1),1)^-1 # Normalized out-of-sample dataset at time t+1
}else{d.test <- dt_tp1 <- dst_tp1 <- ds.test <- NULL}

# Compiled data at time t -> '1-47' + '48-49'
ds.all <- rbind(ds,ds.test)
dim(ds.all) # 49 rows and 6 columns. Since we need to have lagged dataset, number of time steps of ds.all is 49, although ndo = 50.

# Finding optimal embedding dimension (Ed) and nonlinearity parameter ----
# Note that this is not a cross-validation (CV). CV is using in-sample data to forecast out-sample data then check with real out-sample data. Here is to explore Ed and nonlinearity. CV will be later.
#############################################################
# Find the optimal embedding dimension & nonlinearity parameter for each variable 
# based on univariate simplex projection and S-map, respectively

# Univariate simplex projection
Emax <- 10
cri <- 'rmse' # model selection 
Ed <- NULL
forecast_skill_simplex <- NULL
for(i in 1:ncol(ds)){
  spx.i <- simplex(ds[,i], E=2:Emax) 
  # not a CV, so lib and pred is not set
  Ed <- c(Ed,spx.i[which.min(spx.i[,cri])[1],'E'])
  forecast_skill_simplex <- c(forecast_skill_simplex,spx.i[which.min(spx.i[,cri])[1],'rho'])
}
Ed # The optimal embedding dimension for each variable
forecast_skill_simplex # Forecast skills for each variable based on simplex projection

######################################################################
# Finding causal variables by CCM ----
# Find causal variables by CCM analysis for multiview embedding
# Warning: It is time consuming for calculating the causation for each node
# CCM causality test for all node pairs 
# Note that ccm() without setting lib and pred is useful for seeing if rho increase with lib size, whether this is stable with diff Ed; useful for checking nonlinear coupling; useful for choosing parameters (Ed and lib size to see convergence) for formal analysis.
# do.CCM <- F 
if(do.CCM){ 
  ccm.out <- ccm.fast.demo(ds,Epair=T,cri=cri,Emax=Emax)
  ccm.sig <- ccm.out[['ccm.sig']]
  ccm.rho <- ccm.out[['ccm.rho']]
  if(save){
# To avoid overwrite the original files, we save them with different names, 'XXX_NEW'.
    write.csv(ccm.sig, file.path("out", paste("ccm_sig_", da.name, "_nin", nin, "_demo_NEW.csv", sep="")), row.names=FALSE)
    
    write.csv(ccm.rho, file.path('out', paste('ccm_rho_',da.name,'_nin',nin,'_demo_NEW.csv',sep='')),row.names=F)
  }
}

ccm.sig <- read.csv(file.path('out',paste('ccm_sig_',da.name,'_nin',nin,'_demo.csv',sep='')),header=T,stringsAsFactors = F)
ccm.rho <- read.csv(file.path('out',paste('ccm_rho_',da.name,'_nin',nin,'_demo.csv',sep='')),header=T,stringsAsFactors = F)

# ~~~~ meaning of the two tables? ----


######################################################################
# Multiview embedding ----
# Perform multiview embedding analysis for each node/species
# Warning: It is time consuming for running multview embedding for each node/species. 
# ** ~ 1 min for one node using intel CORE i7, ThinkPad P1 in balanced mode. 
# do.multiview <- F
if(do.multiview){
  esele_lag <- esim.lag.demo(ds,ccm.rho,ccm.sig,Ed,kmax=10000,kn=100,max_lag=3,Emax=Emax)
# To avoid overwrite the original files, we save them with different names, 'XXX_NEW'.
if(save){write.csv(esele_lag,file.path('out', paste('eseleLag_',da.name,'_nin',nin,'_demo_NEW.csv',sep='')),row.names=F)}
}

esele <- read.csv(file.path('out', paste('eseleLag_',da.name,'_nin',nin,'_demo.csv',sep='')),header=T)
                    

# So far so good ----


####################################################
## The computation of multiview distance
dmatrix.mv <- mvdist.demo(ds,ds.all,esele)
dmatrix.train.mvx <- dmatrix.mv[['dmatrix.train.mvx']]
dmatrix.test.mvx <- dmatrix.mv[['dmatrix.test.mvx']]



# Cross-validation for optimal parameters (theta etc) ----
######## Leave-one-out cross-validation for finding the optimal parameters for MDR S-map analysis
### Warning: The cross-validation is the *most time-consuming step* in MDR S-map requiring massive computations.  
### Thus, we recommend dividing job into smaller parts (sub.da>1) or used parallel computation (parall=T, ncore>=1)
### The example showing below divided the parameter space into five parts and ran independently (sub.da=5).

### *cv.unit determines the precision of selected parameters and strongly influences computation time.
### cv.unit=0.025, 0.05, or 0.1. The smaller the more precise estimations.
### Selection is depending on how sensitive the results to parameter precision.

cv.unit <- 0.1                            # use 0.1 for fast test
alpha.so <- seq(0, 1, cv.unit);            # Sequence of alpha
sub.da <- 5                                # Divide the computation job into five parts 
afsp <- eqsplit(1:length(alpha.so),sub.da) # Divide the parameter space based on alpha parameter

# ~~~~~ alf = 1 ----
alf <- 1                                   # Run CV in the first parameter subset. we need to change alf=2,3,4, and finally the value of sub.da and re-run the CV several times

# do.MDR.CV <- F
# Cross-validation of MDR analysis    
if(do.MDR.CV){
  alpha.s <- alpha.so[afsp[alf,1]:afsp[alf,2]] # Subset parameter pace
  # Below, we can set 'parall=T' and 'ncore>=1' for faster computation
  cv.ind <- cv.MDR.demo(ds, ds_tp1, dmatrix.list=dmatrix.train.mvx, 
                        parall=T, ncore=12, keep_intra=T,alpha.seq=alpha.s)
  # 12th Gen Intel Core i7-12700H (2.30 GHz) has 14 cores. I use 12 cores here
  # To avoid overwrite the original files, we save them with different names, 'XXX_NEW'.
  if(save){
    write.csv(cv.ind,
              file.path('out', 
                        paste(da.name,
                              '_nin',nin,
                              '_cvunit',cv.unit,
                              '_alph',alpha.s[1]*100,
                              '_cvout_Nmvx_Rallx.csv',sep='')),
              row.names=F)}
  }
# *Repeat the CV under different parameter subsets by changing alf=2,3..,sub.da # *Here sub.da = 5, so we need to change alf=2,3,4, or 5 and re-run the CV  

# did you repeat? pls do so
# set alf <- 2, then do cv.MDR.demo()
# set alf <- 3, then do cv.MDR.demo()
# ...
# set alf <- sub.da, then do cv.MDR.demo()
# then we have sub.da (we set 5 here) newly constructed .csv files in 'out' folder.
# then we can compile the results together and select the optimal parameters.

# the followings are copied from above but with changed alf (1,2,3,...,sub.da) for easy run
################################################################################
################################################################################

# ~~~~~ alf = 2 ----
alf <- 2  
if(do.MDR.CV){
  alpha.s <- alpha.so[afsp[alf,1]:afsp[alf,2]] # Subset parameter pace
  # Below, we can set 'parall=T' and 'ncore>=1' for faster computation
  cv.ind <- cv.MDR.demo(ds, ds_tp1, dmatrix.list=dmatrix.train.mvx, 
                        parall=T, ncore=12, keep_intra=T,alpha.seq=alpha.s)
  # 12th Gen Intel Core i7-12700H (2.30 GHz) has 14 cores. I use 12 cores here
  # To avoid overwrite the original files, we save them with different names, 'XXX_NEW'.
  if(save){
    write.csv(cv.ind,
              file.path('out', 
                        paste(da.name,
                              '_nin',nin,
                              '_cvunit',cv.unit,
                              '_alph',alpha.s[1]*100,
                              '_cvout_Nmvx_Rallx.csv',sep='')),
              row.names=F)}
}

# ~~~~~ alf = 3 ----
alf <- 3  
if(do.MDR.CV){
  alpha.s <- alpha.so[afsp[alf,1]:afsp[alf,2]] # Subset parameter pace
  # Below, we can set 'parall=T' and 'ncore>=1' for faster computation
  cv.ind <- cv.MDR.demo(ds, ds_tp1, dmatrix.list=dmatrix.train.mvx, 
                        parall=T, ncore=12, keep_intra=T,alpha.seq=alpha.s)
  # 12th Gen Intel Core i7-12700H (2.30 GHz) has 14 cores. I use 12 cores here
  # To avoid overwrite the original files, we save them with different names, 'XXX_NEW'.
  if(save){
    write.csv(cv.ind,
              file.path('out', 
                        paste(da.name,
                              '_nin',nin,
                              '_cvunit',cv.unit,
                              '_alph',alpha.s[1]*100,
                              '_cvout_Nmvx_Rallx.csv',sep='')),
              row.names=F)}
}

# ~~~~~ alf = 4 ----
alf <- 4  
if(do.MDR.CV){
  alpha.s <- alpha.so[afsp[alf,1]:afsp[alf,2]] # Subset parameter pace
  # Below, we can set 'parall=T' and 'ncore>=1' for faster computation
  cv.ind <- cv.MDR.demo(ds, ds_tp1, dmatrix.list=dmatrix.train.mvx, 
                        parall=T, ncore=12, keep_intra=T,alpha.seq=alpha.s)
  # 12th Gen Intel Core i7-12700H (2.30 GHz) has 14 cores. I use 12 cores here
  # To avoid overwrite the original files, we save them with different names, 'XXX_NEW'.
  if(save){
    write.csv(cv.ind,
              file.path('out', 
                        paste(da.name,
                              '_nin',nin,
                              '_cvunit',cv.unit,
                              '_alph',alpha.s[1]*100,
                              '_cvout_Nmvx_Rallx.csv',sep='')),
              row.names=F)}
}

# ~~~~~ alf = 5 ----
alf <- 5  
if(do.MDR.CV){
  alpha.s <- alpha.so[afsp[alf,1]:afsp[alf,2]] # Subset parameter pace
  # Below, we can set 'parall=T' and 'ncore>=1' for faster computation
  cv.ind <- cv.MDR.demo(ds, ds_tp1, dmatrix.list=dmatrix.train.mvx, 
                        parall=T, ncore=12, keep_intra=T,alpha.seq=alpha.s)
  # 12th Gen Intel Core i7-12700H (2.30 GHz) has 14 cores. I use 12 cores here
  # To avoid overwrite the original files, we save them with different names, 'XXX_NEW'.
  if(save){
    write.csv(cv.ind,
              file.path('out', 
                        paste(da.name,
                              '_nin',nin,
                              '_cvunit',cv.unit,
                              '_alph',alpha.s[1]*100,
                              '_cvout_Nmvx_Rallx.csv',sep='')),
              row.names=F)}
}
################################################################################
################################################################################

################################################################################
# Compiled the CV results tested under different parts of parameter space
CompileCV=T
if(CompileCV){
  cv.ind <- NULL; for(alf in 1:nrow(afsp)){
    cv.ind <- rbind(cv.ind,
                    read.csv(file.path('out',
                                       paste(da.name,
                                             '_nin',nin,
                                             '_cvunit',cv.unit,
                                             '_alph',alpha.so[afsp[alf,1]]*100,
                                             # now the alf should be the val of sub.da
                                             '_cvout_Nmvx_Rallx.csv',
                                             sep='')),header=T))
    }
  # Select the optimal parameter set with the minimal MSE
  paracv.demo <- secv.demo(cv.ind)
  write.csv(paracv.demo,
            file.path('out',paste(da.name,'_nin',nin,'_cvunit',cv.unit,
                              '_OptimalCV_Nmvx_Rallx_NEW.csv',sep='')),
            row.names = F)
}



# CHECK POINT -------------------------------------------
############################################################
# Fitting MDR S-map based on the parameters selected by CV

cv.unit <- 0.1                           
ptype <- 'aenet'            # enet:elastic-net or msaenet: adaptive elastic-net

# Select the optimal parameter set with the minimal MSE
paracv.demo <- read.csv(file.path('out',paste(da.name,'_nin',nin,'_cvunit',cv.unit,'_OptimalCV_Nmvx_Rallx.csv',sep='')))

# do.MDR <- F
if(do.MDR){
  # Fitting the MDR S-map
  smap.demo <- MDRsmap.demo(paracv=paracv.demo,ptype=ptype,keep_intra=T,out.sample=T,
                            ds,ds_tp1,ds.test,dst_tp1,
                            dmatrix.list=dmatrix.train.mvx,
                            dmatrix.test.list=dmatrix.test.mvx)
  
  # Save forecast skills
  nr.out <- smap.demo[['nr.out']];
  # To avoid overwrite the original files, we save them with different names, 'XXX_NEW'.
  if(save){
    write.csv(nr.out,file.path('out', paste(da.name,'_nin',nin,'_cvunit',cv.unit,'_',ptype,'_nrout_Nmvx_Rallx_demo_NEW.csv',sep='')),row.names=F)
    # Save interaction Jacobian matrices at all time points
    write.csv(smap.demo[['jcof']],file.path('out', paste(da.name,'_nin',nin,'_cvunit',cv.unit,'_',ptype,'_jcof_Nmvx_Rallx_demo_NEW.csv',sep='')),row.names=F)
  }
}


# END -------
