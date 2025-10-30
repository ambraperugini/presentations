## Commented R scripts for the analyses presented in the paper
#
rm(list=ls()) # Clear the workspace
#
## Loading packages for analyses
library(readxl) # to read data
library(metafor) # maximum likelihood meta-analysis
library(bayesmeta) # bayesian meta-analysis
#
## Import the data "dataset_meta.xlsx" in R
d <- as.data.frame( read_excel("dataset_meta.xlsx") )
d$TYPE_OF_STIMULI <- factor(d$TYPE_OF_STIMULI)
# recode "type of stimuli" (see Pairwise comparison below)
d$TYPE_OF_STIMULIc <- relevel(d$TYPE_OF_STIMULI,"VISUOSPATIAL") 
#
## Summary of data
str(d) # type of data
nrow(d) # number of tasks
summary(d) # descriptive statistics
table(d$MEMORY_SYSTEM) # number of tasks by memory system
table(d$TYPE_OF_STIMULI) # number of tasks by type of stimuli
# number of tasks by memory system and type of stimuli
table(d$MEMORY_SYSTEM,d$TYPE_OF_STIMULI) 
#
## Building 3 datasets, one for each memory system
data_long <- d[d$MEMORY_SYSTEM=="LONG TERM",]
data_short<-d[d$MEMORY_SYSTEM=="SHORT TERM",]
data_wm<-d[d$MEMORY_SYSTEM=="WM",]
#
## Random-effects meta-analysis (one for each memory system)
##  via restricted maximum likelihood (REML) 
l<-rma(data_long$Hedges_g,data_long$Variance)
s<-rma(data_short$Hedges_g,data_short$Variance)
w<-rma(data_wm$Hedges_g,data_wm$Variance)
summary(l) # long term memory
summary(s) # short term memory
summary(w) # working memory
#
## Random-effects meta-analysis (one for each memory system)
##  via bayesian approach
unifp<-function (x) {dunif(x,0,3)} # prior for tau: Uniform(0,3)
# prior for mu: Normal(mu=0,sigma=10)
lb <- bayesmeta(y=data_long$Hedges_g,
                sigma=sqrt(data_long$Variance),
                mu.prior.mean=0, mu.prior.sd=10,
                tau.prior=unifp)
sb <- bayesmeta(y=data_short$Hedges_g,
                sigma=sqrt(data_short$Variance),
                mu.prior.mean=0, mu.prior.sd=10,
                tau.prior=unifp)
wb <- bayesmeta(y=data_wm$Hedges_g,
                sigma=sqrt(data_wm$Variance),
                mu.prior.mean=0, mu.prior.sd=10,
                tau.prior=unifp)
summary(lb) # long term memory
summary(sb) # short term memory
summary(wb) # working memory
#
## Evaluation of publication bias via trim and fill method
trimfill(l) # long term memory
trimfill(s) # short term memory
trimfill(w) # working memory
#
## Check robustness of results via Leave-One-Out Diagnostics
# long term memory
print(leave1out(l))
summary(leave1out(l)$estimate)
sd(leave1out(l)$estimate)
# short term memory
print(leave1out(s))
summary(leave1out(s)$estimate)
sd(leave1out(s)$estimate)
# working memory
print(leave1out(w))
summary(leave1out(w)$estimate)
sd(leave1out(w)$estimate)
#
## FOREST PLOTS (basic syntax)
forest(l,slab=paste(data_long$AUTHORS,data_long$YEAR_OF_PUBLICATION,sep=", "),
       xlab="Hedges' g",mlab=" ",xlim=c(-4,4),alim=c(-1,3)) # long term memory
forest(s,slab=paste(data_short$AUTHORS,data_short$YEAR_OF_PUBLICATION,sep=", "),
       xlab="Hedges' g",mlab=" ",xlim=c(-4,4),alim=c(-1,3)) # short term memory
forest(w,slab=paste(data_wm$AUTHORS,data_wm$YEAR_OF_PUBLICATION,sep=", "),
       xlab="Hedges' g",mlab=" ",xlim=c(-4,4),alim=c(-1,3)) # working memory
## FUNNEL PLOTS (basic syntax)
funnel(trimfill(l),xlab="Hedges' g",ylim=c(0,.5)) # long term memory
funnel(trimfill(s),xlab="Hedges' g",ylim=c(0,.5)) # short term memory
funnel(trimfill(w),xlab="Hedges' g",ylim=c(0,.5)) # working memory
## Note: Starting from the basic syntax presented above,
#   figures in the paper were created using the R package "tikzDevice".
#   Complete scripts can be provided on request.
#
## Analysis of type of stimuli, as moderator, for each memory system
l2<-rma(Hedges_g,Variance,mods=~TYPE_OF_STIMULI,data=data_long)
s2<-rma(Hedges_g,Variance,mods=~TYPE_OF_STIMULI,data=data_short)
w2<-rma(Hedges_g,Variance,mods=~TYPE_OF_STIMULI,data=data_wm)
summary(l2) # long term memory
summary(s2) # short term memory
summary(w2) # working memory
#
## Estimation of mean and 95%CI of effect size by type of stimuli
long21<-rma(Hedges_g,Variance,mods=~-1+TYPE_OF_STIMULI,data=data_long)
short21<-rma(Hedges_g,Variance,mods=~-1+TYPE_OF_STIMULI,data=data_short)
working21<-rma(Hedges_g,Variance,mods=~-1+TYPE_OF_STIMULI,data=data_wm)
summary(long21) # long term memory
summary(short21) # short term memory
summary(working21) # working memory
#
## Pairwise comparisons with FDR correction between types of stimuli 
# long term memory
p1<-l2$pval[2]
p2<-l2$pval[3]
p3<-rma(Hedges_g,Variance,mods=~TYPE_OF_STIMULIc,data=data_long)$pval[3]
# p1: Tonal vs Verbal ; p2: Tonal vs Visuospatial ; p3: Visuospatial vs Verbal
p.adjust(c(p1,p2,p3),method="BH") # FDR correction
#
# short term memory
p1<-s2$pval[2]
p2<-s2$pval[3]
p3<-rma(Hedges_g,Variance,mods=~TYPE_OF_STIMULIc,data=data_short)$pval[3]
# p1: Tonal vs Verbal ; p2: Tonal vs Visuospatial ; p3: Visuospatial vs Verbal
p.adjust(c(p1,p2,p3),method="BH") # FDR correction
#
# working memory
p1<-w2$pval[2]
p2<-w2$pval[3]
p3<-rma(Hedges_g,Variance,mods=~TYPE_OF_STIMULIc,data=data_wm)$pval[3]
# p1: Tonal vs Verbal ; p2: Tonal vs Visuospatial ; p3: Visuospatial vs Verbal
p.adjust(c(p1,p2,p3),method="BH") # FDR correction
