## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## ---- message=FALSE, warning=FALSE, eval=FALSE, echo=FALSE--------------------
## RECORDING_SESSION_LABEL
## TRIAL_INDEX
## AVERAGE_IN_BLINK
## TIMESTAMP
## AVERAGE_PUPIL_SIZE
## IP_START_TIME
## SAMPLE_MESSAGE


## ---- message=FALSE, warning=FALSE--------------------------------------------
devtools::install_github("dmirman/gazer")


## -----------------------------------------------------------------------------
pupil_path <- system.file("extdata", "Pupil_file1.xls", package = "gazer")
pupil_sub1<-read.table(pupil_path)


## -----------------------------------------------------------------------------
pupil_files<-merge_pupil_files(file_list)


## -----------------------------------------------------------------------------
behave_data<-behave_pupil(pupil_files, omiterrors = FALSE, behave_colnames = c("subject","script","alteration", "trial", "target","accuracy","rt", "block", "cb"))



## -----------------------------------------------------------------------------
pup_missing<-count_missing_pupil(pupil_files1, missingthresh = .2)


## -----------------------------------------------------------------------------
pup_extend<- pup_missing %>% group_by(subject, trial) %>% 
  mutate(extendblink=extend_blinks(pupil, fillback=100, fillforward=100))


## -----------------------------------------------------------------------------
pup_interp<-interpolate_pupil(pup_outliers, extendblinks=TRUE, type="linear")


## -----------------------------------------------------------------------------
max_pup<-pup_interp%>% 
  group_by(subject, trial) %>% 
  mutate(speed=speed_pupil(interp,time))

mad_pup<-max_pup %>% 
  group_by(subject, trial) %>%
  mutate(MAD=calc_mad(speed))

mad_removal<-mad_pup %>% 
  filter(speed < MAD)

mad_removal<-as.data.frame(mad_removal)


## ---- echo=FALSE--------------------------------------------------------------
rolling_mean_pupil_average<-mad_removal %>% 
        dplyr::mutate(movingavgpup= movingaverage(interp,n=5))


## ---- echo=FALSE--------------------------------------------------------------
baseline_pupil<-baseline_correction_pupil(timebins1, baseline_window=c(500,1000))


## ----echo=FALSE, warning=FALSE------------------------------------------------

baseline_pupil_onset<-baseline_pupil %>% 
  dplyr::group_by(subject, trial) %>%  
  dplyr::mutate(time_zero=onset_pupil(time, sample_message, event=c("target"))) %>%
  ungroup() %>% 


## ---- echo=FALSE--------------------------------------------------------------
timebins1<-downsample_pupil(baseline_pupil_onset, bin.length =  200)

