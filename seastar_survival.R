##survival analysis
#right censored data

library(survival)
library(ggplot2)
library(readr)
library(ggfortify)
library(ggthemes)
library(survminer)
library(extrafont)
library(Design)
library(rms)
library(scales) # For the percent_format() function
loadfonts(device = "pdf")


###death data####
time2death <- read.csv("~/Documents/OneDrive - Western Washington University/MASTERS_AT_WWU/Thesis/Data/CSVs_for_r/time2death.csv")
View(time2death)

###infection data###
time2infection <- read.csv("~/Documents/OneDrive - Western Washington University/MASTERS_AT_WWU/Thesis/Data/CSVs_for_r/time2infection.csv")
View(time2infection)

infection<-as.data.frame(time2infection)
attach(infection)

surv_inf<- (Surv(infection$time, infection$infection_t_f, type ="right"))
inf_model<-survfit(surv_inf~tmt, data = infection) #Kaplan meier
autoplot(inf_model)

## COX (REGRESSION) ARE MORE USEFUL FOR ASSESSING IMPACT OF SEVERAL VARIABLES ON SURVIVAL TIME
## KAPLAN MEIER (LOG RANK TESTS) ARE BETTER FOR COMPARING TWO GROUPS' SURVIVAL TIMES

inf_model_CPH<-coxph(surv_inf~tmt + (1|experiment), data = infection) 
#COX PH instead, but I don't understand the output
#need to incorporate experiment as a random blocked factor... how?
cox.zph(inf_model_CPH) #testing assumption of proportional hazards
ggadjustedcurves(inf_model_CPH, data = infection)

survdiff(surv_inf~tmt, rho=0) #rho = 0 makes it a log rank test of whether or not the survival times are sig different
survdiff(surv_inf~experiment)
summary(inf_model)#$table
#chisq is doing what??? be able to write about this in detail
# chi sq is actually calc if there is a difference using the mantel haenszel test
#Kaplan Meier non parametric analysis of curve fit

#try again with cox proportional hazard

ggsurvplot(inf_model)
survdiff(surv_inf~tmt)
print(inf_model, print.rmean=TRUE)

#bar graph of two groups and average time to event
#back calculate avg values and error
#data frame for horiz barplot

######################################################################################
mean <- c(8.67, 7.47)
treatment <- c("Control", "Arms Removed")
se <- c(1.43, 1.44)
bar_inf<-data.frame(treatment, mean, se)
ggplot(bar_inf, aes(y=mean, x=treatment)) +
  geom_bar(stat='identity', width = 0.4) +
  scale_y_continuous(expand = c(0, 0))+
  annotate("blank", y = 17) +
  ylab("Days to Infection")+
  xlab("Treatment")+
  #theme(text=element_text(family="Times New Roman"))+
  theme_bw()+
  scale_color_grey()+
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        axis.text = element_text(size = 18,family = "Times New Roman"),
        axis.title.x=element_text(vjust=-1, size=18, family="Times New Roman"),
        axis.title.y=element_text(vjust=-0.25, size=18, family="Times New Roman"),)+
  geom_errorbar(aes(ymax = mean+se, ymin = mean-se),width = 0.2)+
  coord_flip()
################ this figure is in FINAL FORM and does not need to be edited ############

ggplot(data, aes(y= "real y", x = "real x"))+
  geom_bar(stat = "identity")+
  scale_y_continuous(expand = c(0, 0))+ #this stars graph at 0,0
  #whatever other layers
  coord_flip()

death<-as.data.frame(time2death)
attach(death)
surv_death <-(Surv(death$time, death$death_t_f, type = "right"))
death_model <-survfit(surv_death~tmt, data = death)
survdiff(surv_death ~ tmt)


######################################################################################
autoplot(death_model)+
  ylab("Survival")+
  xlab("Days to Death")+
  theme_bw()+
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        legend.text = element_text(size = 18, family = "Times New Roman"),
        legend.title = element_text(size = 18, family = "Times New Roman"),
        axis.text = element_text(size = 18,family = "Times New Roman"),
        axis.title.x=element_text(vjust=-1, size=18, family="Times New Roman"),
        axis.title.y=element_text(vjust=-0.25, size=18, family="Times New Roman"),)+
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank())+
  scale_colour_grey(name="Treatment",
                      labels=c("Control", "Arm Removed"))+
  scale_fill_grey(name="Treatment",
                           labels=c("Control", "Arm Removed"))
################ this figure is in FINAL FORM and does not need to be edited #########



print(death_model, print.rmean=TRUE)

mean_death <- c(15.9, 16.3)
treatment_death <- c("Control", "Arms Removed")
se_death <- c(1.03, 0.644)
bar_death<-data.frame(treatment_death, mean_death, se_death)


######################################################################################
ggplot(bar_death, aes(y=mean_death, x=treatment_death)) +
  geom_bar(stat='identity', width = 0.4) +
  scale_y_continuous(expand = c(0, 0))+
  annotate("blank", y = 18) +
  ylab("Days to Death")+
  xlab("Treatment")+
  theme(axis.title=element_text(family="Times"))+
  theme_bw()+
  scale_color_grey()+
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        legend.text = element_text(size = 18, family = "Times New Roman"),
        legend.title = element_text(size = 18, family = "Times New Roman"),
        axis.text = element_text(size = 18,family = "Times New Roman"),
        axis.title.x=element_text(vjust=-1, size=18, family="Times New Roman"),
        axis.title.y=element_text(vjust=-0.25, size=18, family="Times New Roman"),)+
  geom_errorbar(aes(ymax = mean_death+se_death, ymin = mean_death-se_death),width = 0.2)+
  coord_flip()
################ this figure is in FINAL FORM and does not need to be edited #########

Surv(time, death_t_f)
class(survdiff(Surv(time, death_t_f) ~ tmt))

#back calculate se from survfit model
#horz bar graph with avg time to event


################################################################################################# EXPERIMENT 1 #########################################################################################################
death_exp1 <- read_csv("~/Documents/OneDrive - Western Washington University/MASTERS_AT_WWU/Thesis/Data/CSVs_for_r/death_exp1.csv")
death_exp1 <- as.data.frame(death_exp1)

infection_exp1 <- read_csv("~/Documents/OneDrive - Western Washington University/MASTERS_AT_WWU/Thesis/Data/CSVs_for_r/infection_exp1.csv")
infection_exp1 <- as.data.frame(infection_exp1)

infection_exp1 <- as.data.frame(infection_exp1)
surv_inf1<- (Surv(infection_exp1$time, infection_exp1$infection_t_f, type ="right"))
print(inf_model1<-survfit(surv_inf1~infection_exp1$tmt, data = infection_exp1), print.rmean = TRUE)

## test assumptions of PH using log-log plot
 
plot(inf_model1, fun = "cloglog")
# output should give you a graph with the two curves being parallel IF THE ASSUMPTION IS MET
#looks like it does in this case?? so few events it's hard to tell

survdiff(surv_inf1 ~ infection_exp1$tmt)





######################################################################################
autoplot(inf_model1, conf.int = FALSE)+
  ylab("Percent Uninfected")+
  xlab("Days to Infection")+
  scale_y_continuous(labels = percent_format(), limits=c(0,1))+
  theme_bw()+
  aes(linetype = strata)+
  scale_linetype_discrete(name = "Region", breaks=c("Control","Arms Removed"))+
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(), 
        legend.text = element_text(size = 30, family = "Times New Roman"),
        legend.title = element_text(size = 30, family = "Times New Roman"),
        axis.text = element_text(size = 30,family = "Times New Roman"),
        axis.title.x=element_text(vjust=-1, size=30, family="Times New Roman"),
        axis.title.y=element_text(vjust=-0.25, size=30, family="Times New Roman"))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
 
  #scale_colour_discrete(name="Treatment",
                        #labels=c("Control", "Arm Removed"))
  #scale_fill_grey(name="Treatment",
                      #labels=c("Control", "Arm Removed"))+
  

#ggsave("surv_curv.png", width = 10, height = 10)


################ this figure is in FINAL FORM and does not need to be edited #########


surv_death1 <-(Surv(death_exp1$time, death_exp1$death_t_f, type = "right"))
print(death_model1 <-survfit(surv_death1~tmt, data = death_exp1), print.rmean = TRUE)
autoplot(death_model1)
survdiff(surv_death1 ~ death_exp1$tmt)


################################################################################################# EXPERIMENT 2 #########################################################################################################

infection_exp2 <- read_csv("~/Documents/OneDrive - Western Washington University/MASTERS_AT_WWU/Thesis/Data/CSVs_for_r/infection_exp2.csv")

infection_exp2 <- as.data.frame(infection_exp2)

surv_inf2<- (Surv(infection_exp2$time, infection_exp2$infection_t_f, type ="right"))
print(inf_model2<-survfit(surv_inf2~infection_exp2$tmt, data = infection_exp2), print.rmean = TRUE)

## test assumptions of PH using log-log plot

plot(inf_model2, fun = "cloglog")
# output should give you a graph with the two curves being parallel IF THE ASSUMPTION IS MET
#looks like it does in this case?? so few events it's hard to tell

survdiff(surv_inf2 ~ infection_exp2$tmt)

#####################################################################################
autoplot(inf_model2, conf.int = FALSE)+
  ylab("Percent Uninfected")+
  xlab("Days to Infection")+
  theme_bw()+
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(), 
        legend.text = element_text(size = 30, family = "Times New Roman"),
        legend.title = element_text(size = 30, family = "Times New Roman"),
        axis.text = element_text(size = 30,family = "Times New Roman"),
        axis.title.x=element_text(vjust=-1, size=30, family="Times New Roman"),
        axis.title.y=element_text(vjust=-0.25, size=30, family="Times New Roman"),)+
  scale_colour_grey(name="Treatment",
                        labels=c("Control", "Arm Removed"))+
  scale_fill_grey(name="Treatment",
                      labels=c("Control", "Arm Removed"))
################ this figure is in FINAL FORM and does not need to be edited #########



################################################################################################# EXPERIMENT 3 #########################################################################################################

infection_exp3 <- read_csv("~/Documents/OneDrive - Western Washington University/MASTERS_AT_WWU/Thesis/Data/CSVs_for_r/infection_exp3.csv")

infection_exp3 <- as.data.frame(infection_exp3)

surv_inf3<- (Surv(infection_exp3$time, infection_exp3$infection_t_f, type ="right"))
print(inf_model3<-survfit(surv_inf3~infection_exp3$tmt, data = infection_exp3), print.rmean = TRUE)

## test assumptions of PH using log-log plot

plot(inf_model3, fun = "cloglog")
# output should give you a graph with the two curves being parallel IF THE ASSUMPTION IS MET
#only one curve???!maybe they overlap. this is crazy!


survdiff(surv_inf3 ~ infection_exp3$tmt)

#####################################################################################
autoplot(inf_model3, conf.int = FALSE)+
  ylab("Percent Uninfected")+
  xlab("Days to Infection")+
  scale_y_continuous(labels = percent_format(), limits=c(0,1))+
  theme_bw()+
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(), 
        legend.text = element_text(size = 30, family = "Times New Roman"),
        legend.title = element_text(size = 30, family = "Times New Roman"),
        axis.text = element_text(size = 30,family = "Times New Roman"),
        axis.title.x=element_text(vjust=-1, size=30, family="Times New Roman"),
        axis.title.y=element_text(vjust=-0.25, size=30, family="Times New Roman"),)+
  scale_colour_grey(name="Treatment",
                    labels=c("Control", "Arm Removed"))+
  scale_fill_grey(name="Treatment",
                  labels=c("Control", "Arm Removed"))


##############################################################################
##################### power analysis ########################################
require(powerSurvEpi)
