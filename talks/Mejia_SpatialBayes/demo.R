
### PREREQUISITES --------------------------------------------------------------

# Install latest versions of R (this code tested on 4.3.1) and RStudio
# Install latest testing version of INLA
# If any INLA dependencies cause issues, install with dep=FALSE and just install the dependencies sp and foreach
# Install ciftiTools, BayesfMRI, and other packages used below from CRAN

# install.packages('ciftiTools')
# install.packages('BayesfMRI')
# install.packages('manipulateWidget')
# install.packages('ggplot2')
# install.packages('ggthemes')
# install.packages('reshape2')

# install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)
# library(INLA)


### LIBRARIES AND SETUP --------------------------------------------------------

library(ciftiTools)
ciftiTools.setOption('wb_path','/Applications') #where your Connectome Workbench installation is located
library(INLA)
library(BayesfMRI)
library(ggplot2)
library(ggthemes)
library(reshape2)

main_dir <- '/path/to/files'
data_dir <- file.path(main_dir, 'data')
subjects <- list.dirs(data_dir, full.names=FALSE, recursive = FALSE)


### PREPARE FOR RUNNING MODELS -------------------------------------------------

# HPF level to perform via DCT regressors
hpf <- 0.01

# TR in seconds
TR <- 0.72

# which session to analyze
sess <- 'tfMRI_WM_LR'

# File names for onsets & durations
fnames_events <- list(
  body_0bk = "EVs/0bk_body.txt",
  faces_0bk = "EVs/0bk_faces.txt",
  places_0bk = "EVs/0bk_places.txt",
  tools_0bk = "EVs/0bk_tools.txt",
  body_2bk = "EVs/2bk_body.txt",
  faces_2bk = "EVs/2bk_faces.txt",
  places_2bk = "EVs/2bk_places.txt",
  tools_2bk = "EVs/2bk_tools.txt")

# File name for nuisance regressors
fname_nuis <- 'Movement_Regressors.txt'

### LOOP OVER SUBJECTS  --------------------------------------------------------


for(subi in subjects){

  print(paste0('~~~~~~~~~~~~~~~~ SUBJECT ',subi,' ~~~~~~~~~~~~~~~~ '))

  ## Check that all data exists ---------------------------------------------

  setwd(file.path(data_dir, subi, sess))
  cifti_fname <- paste0(sess, '_Atlas_MSMAll.dtseries.nii')
  if(!file.exists(cifti_fname)) next() #fMRI missing
  if(sum(!sapply(fnames_events, file.exists)) > 0) next() #any events missing
  if(!file.exists(fname_nuis)) next() #motion regressors missing

  ## Read and Organize Data  ---------------------------------------------------

  #read in fMRI data
  BOLD_xii <- read_cifti(cifti_fname)
  nT <- ncol(BOLD_xii)

  #read in each events table, set column names and event names
  onsets <- lapply(fnames_events, read.table, header=FALSE)
  names(onsets) <- names(fnames_events)
  onsets

  #read in nuisance regressors
  nuisance <- as.matrix(read.table(fname_nuis, header=FALSE))

  #split time series into first and second halves (4 tasks per half) for computational efficiency
  start_times <- sort(sapply(onsets, function(x) x[1,1]))
  events1 <- start_times[1:4]
  events2 <- start_times[5:8]
  split_at <- floor(events2[1]/.72) #which volume to split timeseries at
  BOLD_xii1 <- select_xifti(BOLD_xii, 1:split_at)
  BOLD_xii2 <- select_xifti(BOLD_xii, split_at:nT)
  onsets1 <- onsets[names(events1)]
  onsets2 <- onsets[names(events2)]
  onsets2 <- lapply(onsets2, function(x){ x[1] <- x[1] - split_at*.72 ; x }) #adjust start times of 2nd part
  nuisance1 <- nuisance[1:split_at,]
  nuisance2 <- nuisance[split_at:nT,]

  ## Estimate Spatial Bayesian GLM   -------------------------------------------

  help(BayesGLM)
  help(BayesGLM_cifti)

  # Note: Multiple sessions can be jointly analyzed to obtain session-specific
  # results or cross-session average results.  See help(BayesGLM_cifti) for details

  # # Bayesian GLM (10k resolution) -- 7 minutes to compute
  # system.time(glm_ss <- BayesGLM_cifti(BOLD_xii1,
  #                                      brainstructures = 'left',
  #                                      onsets = onsets1,
  #                                      TR = TR,
  #                                      nuisance = nuisance1,
  #                                      dHRF = 0,
  #                                      hpf = hpf,
  #                                      resamp_res = 10000,
  #                                      Bayes = TRUE, #default
  #                                      ar_order = 6, #default
  #                                      ar_smooth = 5, #default
  #                                      num.threads = 8,
  #                                      verbose = 1))

  # Bayesian GLM (20k resolution) -- 30 min to compute
  system.time(glm_ss <- BayesGLM_cifti(BOLD_xii1,
                                       brainstructures = 'left',
                                       onsets = onsets1,
                                       TR = TR,
                                       nuisance = nuisance1,
                                       dHRF = 0,
                                       hpf = hpf,
                                       resamp_res = 20000,
                                       Bayes = TRUE, #default
                                       ar_order = 6, #default
                                       ar_smooth = 5, #default
                                       num.threads = 8,
                                       verbose = 1))
  saveRDS(glm_ss, file='result1_20k.rds')
  # glm_ss <- readRDS(file='result1_20k.rds')

  # plots estimates of activation amplitude
  tasks <- gsub('_HRF','',glm_ss$task_names)
  plot(glm_ss$task_estimates_xii$Bayes$single_session, idx=1:4, zlim=c(-2,2), title=paste0('Subject ', subi), fname=paste0('est_',tasks))

  # # Bayesian GLM (32k resolution) -- 7 hours to compute
  # system.time(glm_ss <- BayesGLM_cifti(BOLD_xii1,
  #                                      brainstructures = 'left',
  #                                      onsets = onsets1,
  #                                      TR = TR,
  #                                      nuisance = nuisance1,
  #                                      dHRF = 0,
  #                                      hpf = hpf,
  #                                      resamp_res = NULL,
  #                                      Bayes = TRUE, #default
  #                                      ar_order = 6, #default
  #                                      ar_smooth = 5, #default
  #                                      num.threads = 8,
  #                                      verbose = 1))
  # saveRDS(glm_ss, file='result1.rds')

  ## Detect and plot Activations  -----------------------------------------------

  act_ss_Bayes_05 <- id_activations(glm_ss, tasks = 1:4, alpha = 0.05, threshold = 0.5) #detect activations significantly over 0.5% signal change
  saveRDS(act_ss_Bayes_05, file='activations.rds')
  #act_ss_Bayes_05 <- readRDS(file='activations.rds')
  plot(act_ss_Bayes_05, idx=1:4, title=paste0('Activations (>0.5%), Subject ', subi), fname=paste0('act_05_',tasks))

  #add up activations across subjects
  if(subi == subjects[1])
    act_Bayes_05_all <- act_ss_Bayes_05$activations_xii$single_session
  else
    act_Bayes_05_all <- act_Bayes_05_all + act_ss_Bayes_05$activations_xii$single_session
}

saveRDS(act_Bayes_05_all, file=file.path(main_dir,'activations_all.rds'))


### EXPLORE RESULTS FOR ONE SUBJECT --------------------------------------------

subi <- subjects[1]
setwd(file.path(data_dir, subi, sess))

#read in result
glm_ss <- readRDS(file='result1.rds')
tasks <- gsub('_HRF','',glm_ss$task_names)

glm_ss
class(glm_ss)
names(glm_ss) #contents of BayesGLM_cifti object
glm_ss$task_estimates_xii$Bayes$single_session #a xifti object

## Visualize Bayesian and Classical estimates

# Note: plot() is an alias for ciftiTools::view_xifti_surface()

#interactive RGL windows (one image at a time)
k <- 1 #first task
plot(glm_ss$task_estimates_xii$Bayes$single_session, idx=k, zlim=c(-2,2), title=paste0('Bayesian, ',tasks[k]))
plot(glm_ss$task_estimates_xii$classical$single_session, idx=k, zlim=c(-2,2), title=paste0('Classical, ',tasks[k]))

#interactive widget with slider (multiple images)
plot(glm_ss$task_estimates_xii$Bayes$single_session, idx=1:4, zlim=c(-2,2), title=paste0('Bayesian, ',tasks))
plot(glm_ss$task_estimates_xii$classical$single_session, idx=1:4, zlim=c(-2,2), title=paste0('Classical, ',tasks))

#save to PNG (specify fname)
plot(glm_ss$task_estimates_xii$Bayes$single_session, idx=1:4, zlim=c(-2,2), title=paste0('Bayesian, ',tasks),
     fname=paste0('estimates_Bayesian_',tasks))
plot(glm_ss$task_estimates_xii$classical$single_session, idx=1:4, zlim=c(-2,2), title=paste0('Classical, ',tasks),
     fname=paste0('estimates_classical_',tasks))

## Visualize Design Matrix

X <- as.data.frame(glm_ss$design[[1]])
X$volume <- 1:nrow(X)
X_long <- melt(X, id.vars = 'volume', value.name = 'HRF', variable.name = 'task')
X_long$time <- X_long$volume*0.72
ggplot(X_long, aes(x=time, y=HRF)) + geom_line(aes(group=task, color=task), size=1) +
  theme_few() + scale_color_brewer(palette='Set2')


### IDENTIFY SUBJECT-LEVEL ACTIVATIONS --------------------------------------------

help(id_activations)

# Activation Threshold = 0 (traditional hypothesis testing appraoch)

act_ss_Bayes_00 <- id_activations(glm_ss, tasks = 1, alpha = 0.05, threshold = 0) # > 0% local signal change -- 15 min / task
act_ss_classical_00 <- id_activations(glm_ss, tasks = 1, alpha = 0.05, threshold = 0, method='classical', correction='FDR') # > 0% local signal change -- 15 min / task

# Scientifically Meaningful Activation Thresholds

# Bayesian activations via spatial posterior distribution
act_ss_Bayes_05 <- id_activations(glm_ss, tasks = 1:4, alpha = 0.05, threshold = 0.5) # > 0.5% local signal change -- 4 min / task
act_ss_Bayes_1 <- id_activations(glm_ss, tasks = 1:4, alpha = 0.05, threshold = 1) # > 1% local signal change -- 3 min / task
act_ss_Bayes_2 <- id_activations(glm_ss, tasks = 1:4, alpha = 0.05, threshold = 2) # > 2% local signal change -- 3 min / task

save(act_ss_Bayes_00, act_ss_classical_00, act_ss_Bayes_05, act_ss_Bayes_1, act_ss_Bayes_2, file='activations.RData')
load(file='activations.RData')

names(act_ss_Bayes_00)
class(act_ss_Bayes_00$activations_xii$single_session)

#compare Bayesian and classical activations at 0% threshold
plot(act_ss_Bayes_00, title='Bayesian')
plot(act_ss_classical_00, title='classical')

#look at Bayesian activations at 0.5% and 1% and 2%
act_ss_Bayes_05$activations_xii$single_session
act_combined <- act_ss_Bayes_05$activations_xii$single_session + act_ss_Bayes_1$activations_xii$single_session + act_ss_Bayes_2$activations_xii$single_session
act_combined <- ciftiTools:::convert_to_dlabel(act_combined, values = c(0,1,2,3))
plot(act_combined, colors = c('white','blue','turquoise','yellow'), title=tasks[1])


### PLOT ACTIVATIONS RESULTS ACROSS ALL SUBJECTS -------------------------------------

setwd(main_dir)

#this object contains the number of subjects that show significant activation at each vertex
act_Bayes_05_all <- readRDS(file='activations_all.rds')

#convert counts to proportions
act_Bayes_05_all <- act_Bayes_05_all/length(subjects)

#set 0 to NA
act_Bayes_05_all_mat <- as.matrix(act_Bayes_05_all)
act_Bayes_05_all_mat[act_Bayes_05_all_mat==0] <- NA
act_Bayes_05_all <- newdata_xifti(act_Bayes_05_all, act_Bayes_05_all_mat)

#plot it
k <- 4
plot(act_Bayes_05_all, zlim=c(0,0.5), idx=k, title=paste0('Proportion of Subjects Active (>0.5%), ',tasks[k]), colors='viridis')


### PERFORM GROUP AVERAGES AND CONTRASTS --------------------------------------------

help(BayesGLM2)

setwd(main_dir)
results <- file.path('data',subjects, sess, 'result1_20k.rds')

#about 8 minutes per subject
glm2 <- BayesGLM2(results,
                  excursion_type = '>',
                  gamma = 0, #detect any effect size
                  alpha = 0.05,
                  num_cores = 8,
                  verbose = 2)
glm2B <- BayesGLM2(results,
                  excursion_type = '>',
                  gamma = 0.5, #detect effect sizes over 0.5% signal change
                  alpha = 0.05,
                  num_cores = 8,
                  verbose = 2)
saveRDS(glm2, file='glm2.rds')
saveRDS(glm2B, file='glm2B.rds')
#glm2 <- readRDS(file='glm2.rds')
#glm2B <- readRDS(file='glm2B.rds')

#plot the group average activations significantly greater than 0% and 0.5% signal change
k <- 4
plot(glm2$contrast_estimates_xii, idx=k, zlim=c(-0.5,0.5), title=paste0('Average Activation Amplitude, ',tasks[k]))
plot(glm2$activations_xii, idx=k, title=paste0('Group Average Activation (>0%), ',tasks[k]))
plot(glm2B$activations_xii, idx=k, title=paste0('Group Average Activation (>0.5%), ',tasks[k]))





