setwd("~/Desktop/R/TB/data files")

#to clean R memory
rm(list=ls())
#===========================================================================
#In this code I create a data set, df, of 2010-2014 to create models for 
#predicting wins from the hybrid data set. df1 is a data set consisiting of 
#only categorical variables. I also create a test data set by
#combining 2013 PER with 2014 wins called, test_df2. First, I look at
#correlation plots and pick out correlated variables; DB,OL,QB,RB,TE,WR,QB_TE.IPER,
#QB_OL.IPER,off.

#My first model includes all variables. Only DB, OL, RBQB_OL are sig vars; must keep QB

#Then I run step aic, which suggest variables
#QB, off, def, RB, DB; this turns out to be better than the following vif step aic.
#After looking at vif which suggests DL, QB, RB, TE, WR, def run a step aic model 
#which keeps QB, RB, DL. Next, I used splines with the simplest splines, s=2, 
#being best. I also tested locally orginated regression, but it failed to improve results.

#In order to regression trees, had to minupulate varis so that input vars same as prediction vars.

#Prediction results: team linear = 56.3%; lm.corr = 71.(5); lm.corr.vif=65.6%; 
#lm.corr.vif.stepaic=62.5%; gam.sp.fullvar=65.6%; gam.sp.2var=65.6%; rt.corr.10node=59.4%;
#rt.corr.7node=62.5% rt.corr.5node=62.5% rf.corr.10node=59.4%
#============================================================================
df<-read.csv("hybrid_Master PER File.csv", header=TRUE, stringsAsFactors = FALSE)
str(df)#160 rows and 19 variables

head(df, n=10)
fix(df)
library(plyr)
sum(is.na(df))

#########creating  test df for predictions
test_df<-subset(df, df$Year==2014, wins)
test_df
head(test_df)

PER_2013<-subset(df, df$Year==2013, select=c(QB, WR, TE, DL, def, RB, DB, off, OL,
        QB_OL.IPER,QB_TE.IPER,TE_WR.IPER, QB_WR.IPER),)
head(PER_2013)

#a df consisiting of 2013PER and 2014wins
test<-cbind(PER_2013, test_df)

head(test)
dim(test)
#################
library(ggplot2)

wp<- ggplot(df, aes(x=off, y=wins))+ geom_point()#view scatterplot
wp
#offense is definitly highly postively correlated with wins

wp2<- ggplot(df, aes(x=def, y=wins))+ geom_point()
wp2
#interestinly defense is not very correlated

#another method of scatterplot
plot(df$QB,df$wins)# QB per and wins highly correlated

#library("corrplot")
#corrplot(cm, method="circle") #alternative method...pretty!

df1<-df[,3:18]#subset data set to eliminate categorical variables
head(df1)

#need a dataset with only quat vars and removing unwated other vars
# for classification trees

head(df)
df3<-df[-c(1:2,17:18)]
head(df3)

cm<-pairs(df1)# generate correlation matrix
#major correlations are between: DB, OL, QB, RB, TE, WR, QB_TE, QB_OL, off

setwd("~/Desktop/R/TB/wins")

pdf(file = "correlation_matrix.pdf", width = 11, height = 8.5)
pairs(df1)
dev.off()

#====================================
#regression models
#====================================
#######team model
wins_tm <- 5.2 * QB + 3.0 * DB + 1.5*RB - 4.9 
summary(wins_tm)

pred_wins_tm<-5.2*test$QB + 3*test$DB + 1.5*test$RB - 4.9
pred_wins_tm

fit_df_tm<-data.frame(wins=test$wins, predicted=pred_wins_tm)

corr_tm<-sum((abs(fit_df_tm$wins -fit_df_tm$predicted)<2.5))
per_corr_tm<-corr_tm/32
per_corr_tm#56.3%

#########correlated vars lm reg

wins<- lm(wins~ DB + OL + QB + RB + TE + WR + QB_TE.IPER + QB_OL.IPER +
        off -1, data=df)
wins
summary(wins)# AdjR^2 = 94%; model sig = very high; var sig = DB,OL,QB,QB_OL
library(fmsb)
VIF(lm(wins~ DB + OL + QB + RB + TE + WR + QB_TE.IPER + 
         QB_OL.IPER + off -1, data=df))
#VIF is 17; snce > 10 multicillinearity is suspected

#to print output to file
sink("wins.txt")
print(summary(wins))
sink()

pred_wins<-predict(wins, newdata=test, se=TRUE)
fit_df<-data.frame(wins=test$wins, predicted=pred_wins$fit)
mean(fit_df$predicted) ##ave wins 7.96
corr<-sum((abs(fit_df$wins - fit_df$predicted)<2.5))
per_corr<-corr/32
per_corr# 62.5%


#######correlated and sig vars
wins1<-lm(wins~ DB+OL+QB+QB_OL.IPER-1, data=df)
summary(wins1)#adjR^2 93.9%
VIF(wins1)# VIF 16
pred_wins1<-predict(wins1, newdata=test, se=TRUE)
fit_df1<-data.frame(wins=test$wins, predicted=pred_wins1$fit)
mean(fit_df1$predicted) #7.98 ave wins
corr1<-sum((abs(fit_df1$wins - fit_df1$predicted)<2.5))
per_corr1<-corr1/32
per_corr1# 65.6%**********************

df_2014<-subset(df, df$Year==2014,)
df_2014
pred_wins_2014<-predict(wins1, newdata=df_2014, se =TRUE)
head(pred_wins_2014)
names(pred_wins_2014)
mean(pred_wins_2014$fit) #ave 8.09 wins
#######step aic model; this uses variables selected by step aic

library(MASS)#step aic requires MASS
wins_stp <- stepAIC(wins, direction="both")
wins_stp# suggests DB, OL, RB, QB_OL as primary predictors
summary(wins_stp)# all vars sign, mod sig, adjr^2=94.1%
VIF(wins_stp)# VIF 17

sink("wins_stp.txt")
print(wins_stp)
sink()


pred_wins_stp<-predict(wins_stp, newdata=test, se=TRUE)
fit_df_stp<-data.frame(wins=test$wins, predicted=pred_wins_stp$fit)
corr_stp<-sum((abs(fit_df_stp$wins - fit_df_stp$predicted)<2.5))
per_corr_stp<-corr_stp/32
per_corr_stp# 59.4%

#####################corr var with VIF
#creating a df with only quant vars
df2<-df[,3:15]
#stepwise VIF function used below
vif_func<-function(in_frame,thresh=10,trace=T,...){
        
        require(fmsb)
        
        if(class(in_frame) != 'data.frame') in_frame<-data.frame(in_frame)
        
        #get initial vif value for all comparisons of variables
        vif_init<-NULL
        for(val in names(in_frame)){
                form_in<-formula(paste(val,' ~ .'))
                vif_init<-rbind(vif_init,c(val,VIF(lm(form_in,data=in_frame,...))))
        }
        vif_max<-max(as.numeric(vif_init[,2]))
        
        if(vif_max < thresh){
                if(trace==T){ #print output of each iteration
                        prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
                        cat('\n')
                        cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
                }
                return(names(in_frame))
        }
        else{
                
                in_dat<-in_frame
                
                #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
                while(vif_max >= thresh){
                        
                        vif_vals<-NULL
                        
                        for(val in names(in_dat)){
                                form_in<-formula(paste(val,' ~ .'))
                                vif_add<-VIF(lm(form_in,data=in_dat,...))
                                vif_vals<-rbind(vif_vals,c(val,vif_add))
                        }
                        max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2])))[1]
                        
                        vif_max<-as.numeric(vif_vals[max_row,2])
                        
                        if(vif_max<thresh) break
                        
                        if(trace==T){ #print output of each iteration
                                prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
                                cat('\n')
                                cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
                                flush.console()
                        }
                        
                        in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
                        
                }
                
                return(names(in_dat))
                
        }
        
}
vif_func(in_frame=df2, thresh=5, trace=T)
#ssugests keeping DL, QB, RB, TE, WR, QB_OL, def


#correlated and vif var model
wins2<- lm(wins~ QB + def + RB + WR + TE + DL + QB_OL.IPER-1, data=df)
wins2
summary(wins2)# AdjR^2 = 93.7%; model sig = very high; 
#var sig = QB, def, DL, QB_OL
pred_wins2<-predict(wins2, newdata=test, se=TRUE)
fit_df2<-data.frame(wins=test$wins, predicted=pred_wins2$fit)
corr2<-sum((abs(fit_df2$wins - fit_df2$predicted)<2.5))
per_corr2<-corr2/32
per_corr2# 62.5%

###correlated, VIF, sig var model
wins3<- lm(wins~ QB + def + DL + QB_OL.IPER-1, data=df)
summary(wins3)#adjR^2=93.%5
pred_wins3<-predict(wins3, newdata=test, se=TRUE)
mean(pred_wins3$fit) #ave 8.02
fit_df3<-data.frame(wins=test$wins, predicted=pred_wins3$fit)
corr3<-sum((abs(fit_df3$wins - fit_df3$predicted)<2.5))
per_corr3<-corr3/32
per_corr3# 62.5%

sink("wins-best.txt")
summary(wins3)
sink()

pdf(file = "best lm wins model.pdf", width = 11, height = 8.5)
plot(wins3, se=TRUE, col="blue")
dev.off()

####polynomial model & log model exploration

wins.poly=lm(wins~poly(QB,3) -1, data=df)# fits a polynomial model up to degree 5
summary(wins.poly)#adjR^2 = 4.5%; only first degree significant

wins.log<-lm(log(wins)~QB+DL+def+QB_OL.IPER-1, data=df)
summary(wins.log) #adj R^2 = 97.1%
pred_wins_log<-predict(wins.log, newdata=test, se=TRUE)
pred_wins_log_removed<-2.71828^(pred_wins_log$fit)

fit_df_log<-data.frame(wins=test$wins, predicted=pred_wins_log_removed)
corr_log<-sum((abs(fit_df_log$wins - fit_df_log$predicted)<2.5))
per_corr_log<-corr_log/32
per_corr_log# 53.1%

#############correlated, vif and step aic vars

wins4<-stepAIC(wins2, direction="both")#uses only QB, RB, def, TE DL
wins4
summary(wins4)#AdjR^2 = 93.7 all vars sig; model sig
pred_wins4<-predict(wins4, newdata=test, se=TRUE)
fit_df4<-data.frame(wins=test$wins, predicted=pred_wins4$fit)
corr4<-sum((abs(fit_df4$wins - fit_df4$predicted)<2.5))
per_corr4<-corr4/32
per_corr4# 59.4%

#===========================
#General Additive Modles spline models
#===========================
library(gam)
#previous model
wins_gams<-gam(wins~s(QB,2)+s(def,2)+s(RB,2)+s(WR, 3)+s(TE,3), data=df)
wins_gams

pred_gams<-predict(wins_gams, newdata=test)
fit_df_gams<-data.frame(wins=test$wins, predicted=pred_gams)
corr_gams<-sum(abs(fit_df_gams$wins -fit_df_gams$predicted)<2.5)
per_corr_gams<-corr_gams/32
per_corr_gams# 62.5% 

#using corr, vif, sig vars
wins_gams2<-gam(wins~s(QB,2) + s(def,2) + s(DL,2) + s(QB_OL.IPER,2), data=df)
wins_gams2
plot(wins_gams2, se=TRUE, col="blue")
summary(wins_gams2)
pred_gams2<-predict(wins_gams2, newdata=test)
fit_df_gams2<-data.frame(wins=test$wins, predicted=pred_gams2)
corr_gams2<-sum(abs(fit_df_gams2$wins -fit_df_gams2$predicted)<2.5)
per_corr_gams2<-corr_gams2/32
per_corr_gams2# 71.9% 

coefficients(summary.glm(wins_gams2))

sink("gams model summary.txt")
print(summary(wins_gams2))
sink()

sink("gams model coef.txt")
print(coefficients(summary.glm(wins_gams2)))
sink()

pdf(file = "gams model spline plot.pdf", width = 11, height = 8.5)
plot(wins_gams2, se=TRUE, col="blue")
dev.off()

#using corr, vif, and spline recks
wins_gams3<-gam(wins~s(QB,1.7)+s(def,2.5)+s(DL,1.1)+s(QB_OL.IPER,2.3), data=df)
pred_gams3<-predict(wins_gams3, newdata=test)
fit_df_gams3<-data.frame(wins=test$wins, predicted=pred_gams3)
corr_gams3<-sum((abs(fit_df_gams3$wins -fit_df_gams3$predicted)<2.5))
per_corr_gams3<-corr_gams3/32
per_corr_gams3# 68.8% 



#==========================
#regression trees models
#=========================

library(MASS)
library(rpart)
str(df3)#verifying no cat vars and all vars are numeric

#tree with vif selected variables


#very important the var names in forming tree (train) are same as predciting (test) data set
QB<-df3$QB
def<-df3$def
RB<-df3$RB
WR<-df3$WR
TE<-df3$TE
DL<-df3$DL
wins<-df3$wins
QB_TE.IPER<-df3$QB_TE.IPER
OL_RB.IPER<-df3$OL_RB.IPER
TE_WR.IPER<-df3$TE_WR.IPER
QB_OL.IPER<-df3$QB_OL.IPER
QB_WR.IPER<-df3$QB_WR.IPER


#######correlated vars mregression tree
set.seed(1236)
tree.wins<-rpart(wins ~ QB + def + RB + WR + TE + DL + off + 
        DL + QB_TE.IPER + QB_WR.IPER + TE_WR.IPER + QB_OL.IPER,
        data=df3, method="anova")

summary(tree.wins)#variable importance QBOL, QB, off, QBTE, QBWR, TEWR, TE
#DL, def, RB, WR

plot(tree.wins)
text(tree.wins)

tree.wins.pred<-predict(tree.wins, newdata = test, type="vector")
tree.wins.pred
dim(test)
fit_tree_df<-data.frame(wins=test$wins, predicted=tree.wins.pred)
corr_tree<-sum((abs(fit_tree_df$wins - fit_tree_df$predicted)<2.5))
per_corr_tree<-corr_tree/32
per_corr_tree#65.6%

pdf(file = "wins regression tree.pdf", width = 11, height = 8.5)
plot(tree.wins)
text(tree.wins)
dev.off()

sink("regression tree wins.txt")
print(summary(tree.wins))
sink()


set.seed(1111)
printcp(tree.wins)
tree.wins.p<-prune(tree.wins, cp=.024)#view summary and pick cp that minimizes xerror
#in this case cp was chosen allowing for a slight increase in error
#to get more detail in tree
summary(tree.wins.p)
plot(tree.wins.p)
text(tree.wins.p)

pdf(file = "wins.p regression tree.pdf", width = 11, height = 8.5)
plot(tree.wins.p)
text(tree.wins.p)
dev.off()

sink("regression tree wins.p.txt")
print(summary(tree.wins.p))
sink()


predict(tree.wins.p)
tree.wins.pred1<-predict(tree.wins.p, newdata = test, type="vector")
tree.wins.pred1
fit_tree_df1<-data.frame(wins=test$wins, predicted=tree.wins.pred1)
corr_tree1<-sum((abs(fit_tree_df$wins - fit_tree_df$predicted)<2.5))
per_corr_tree1<-corr_tree1/32
per_corr_tree1#65.6%

#================radnom forest
library(randomForest)

head(df)
QB_WR.IPER<-df[,19:19]
df3
df4<-cbind(df2,QB_WR.IPER)
head(df4)

rf_tree.wins<-randomForest(wins ~ QB + def + RB + WR + TE + DL + off + 
        DL + QB_TE.IPER + QB_WR.IPER + TE_WR.IPER + QB_OL.IPER, data=df3)
#drops error due to 2 NA's in DL
sum(is.na(df3))
colSums(is.na(df))
df4<-na.omit(df3)

QB<-df4$QB
def<-df4$def
RB<-df4$RB
WR<-df4$WR
TE<-df4$TE
DL<-df4$DL
wins<-df4$wins
QB_TE.IPER<-df4$QB_TE.IPER
OL_RB.IPER<-df4$OL_RB.IPER
TE_WR.IPER<-df4$TE_WR.IPER
QB_OL.IPER<-df4$QB_OL.IPER
QB_WR.IPER<-df4$QB_WR.IPER
wins<-df3$wins

set.seed(2222)
rf_tree.wins<-randomForest(wins ~ QB + def + RB + WR + TE + DL + off + 
        DL + QB_TE.IPER + QB_WR.IPER + TE_WR.IPER + QB_OL.IPER, data=df4)

print(rf_tree.wins)
importance(rf_tree.wins)# realtive importace, qb, def

rf.wins.pred<-predict(rf_tree.wins, newdata=test)
rf.wins.pred

fit_rf_df<-data.frame(wins=test$wins, predicted=rf.wins.pred)
corr_rf<-sum((abs(fit_rf_df$wins - fit_rf_df$predicted))<2.5)
per_corr<-corr_rf/32
per_corr#59.4%


####### More gression trees: increased cp rt model
tree.wins.p2<-prune(tree.wins, cp=.06)# 7 nodes
summary(tree.wins.p2)

plot(tree.wins.p2)
text(tree.wins.p2)
tree.wins.pred2<-predict(tree.wins.p2, newdata = test, type="vector")
tree.wins.pred2
fit_tree_df2<-data.frame(wins=test$wins, predicted=tree.wins.pred2)
corr_tree_p2<-sum((abs(fit_tree_df2$wins - fit_tree_df2$predicted)<2.5))
per_corr_p2<-corr_tree_p2/32
per_corr_p2#68.8%

pdf(file = "7 node regression tree.pdf", width = 11, height = 8.5)
plot(tree.wins.p2)
text(tree.wins.p2)
dev.off()

sink("7 node regression tree wins.txt")
print(summary(tree.wins.p2))
sink()

######5 node model
tree.wins.p3<-prune(tree.wins, cp=.05)# 5 nodes
summary(tree.wins.p3)#no change over 7 node model
plot(tree.wins.p3)
text(tree.wins.p3)
tree.wins.pred3<-predict(tree.wins.p3, newdata = test, type="vector")
tree.wins.pred3
fit_tree_df3<-data.frame(wins=test$wins, predicted=tree.wins.pred3)
fit_tree_df3
corr_tree_p3<-sum((abs(fit_tree_df3$wins - fit_tree_df3$predicted)<2.5))
per_corr_p3<-corr_tree_p3/32
per_corr_p3#68.8%

#================
#neural networks
#===============
library(NeuralNetTools)
library(neuralnet)
library(nnet)
library(predict.nnet)
head(df)

#full model
##using a neural net with 2 hiiden features; linear output; na's removed
nn_wins<-nnet(wins~QB + def + RB + WR + TE + DL + off + 
        DL + QB_TE.IPER + QB_WR.IPER + TE_WR.IPER + 
        QB_OL.IPER, data=df3,size=2, linout=T, na.action="na.omit")
nn_wins
summary(nn_wins)
nn_pred<-predict(nn_wins, newdata=test, type="raw" )
nn_pred

fit_nn_df<-data.frame(wins=test$wins, predicted=nn_pred)
fit_nn_df
corr_nn<-sum((abs(fit_nn_df$wins - fit_nn_df$predicted))<2.5)
nn_corr<-corr_nn/32
nn_corr#71.9%

library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

pdf(file = "neural net.pdf", width = 11, height = 8.5)
plot.nnet(nn_wins)
dev.off()

sink("neural net wins.txt")
print(summary(nn_wins))
sink()


nn_wins2<-nnet(wins~QB + def + RB + WR + TE + DL + off + 
        DL + QB_TE.IPER + QB_WR.IPER + TE_WR.IPER + 
        QB_OL.IPER, data=df,size=8, linout=T, na.action="na.omit")
nn_wins2
summary(nn_wins2)
nn_pred2<-predict(nn_wins2, newdata=test, type="raw" )
nn_pred2

fit_nn_df2<-data.frame(wins=test$wins, predicted=nn_pred2)
fit_nn_df2
corr_nn2<-sum((abs(fit_nn_df2$wins - fit_nn_df2$predicted)<2.5))
nn_corr2<-corr_nn2/32
nn_corr2#65.6%

#redued vars
nn_wins3<-nnet(wins~QB+def+RB+WR+TE+DL+DB, data=df,size=2, linout=T, na.action="na.omit")
nn_wins3
summary(nn_wins3)
nn_pred3<-predict(nn_wins3, newdata=test, type="raw" )
nn_pred3

fit_nn_df3<-data.frame(wins=test$wins, predicted=nn_pred3)
fit_nn_df3
corr_nn3<-sum((abs(fit_nn_df3$wins - fit_nn_df3$predicted)<2.5))
nn_corr3<-corr_nn3/32
nn_corr3#68.8


###########using caret
require(caret)#automaticaly tunes hyperparameters such as size and decy

nn_wins_4<-train(wins~QB + def + RB + WR + TE + DL + off + 
        DL + QB_TE.IPER + QB_WR.IPER + TE_WR.IPER + 
        QB_OL.IPER, data=df3, method="nnet", 
        linout=TRUE, trace=FALSE, na.action="na.omit", 
        tuneGrid=expand.grid(.size=c(1,5,10),.decay=c(0,0.001,0.1))) 

nn_wins_4
nn_pred4<-predict(nn_wins_4, newdata=test, type="raw" )
nn_pred4

fit_nn_df4<-data.frame(wins=test$wins, predicted=nn_pred4)
fit_nn_df4
corr_nn4<-sum((abs(fit_nn_df4$wins - fit_nn_df4$predicted)<2.5))
nn_corr4<-corr_nn4/32
nn_corr4#71.9

nn_pred4
plot(df3$wins)
lines(nn_pred4, col=2)

pdf(file = "neural net wins caret.pdf", width = 11, height = 8.5)
plot(df3$wins)
lines(nn_pred4, col=2)
dev.off()

sink("neural net wins caret.txt")
print(nn_wins_4)
sink()

sink("neural summary net wins caret.txt")
print(summary(nn_wins_4))
sink()
#==================================
#extending wins regression model for interactions
#===================================
load("/Users/williamclarke/Desktop/R/TB/PEr wins.ws.RData")
setwd("~/Desktop/R/TB/data files")
head(df, n=20)

wins_interaction<- lm(wins~ QB_OL.IPER + QB_TE.IPER + TE_WR.IPER + QB_WR.IPER
        + QB + off + def + RB + WR + OL + DB -1, data=df)
wins_interaction
summary(wins_interaction)# AdjR^2 = 94% and improvement over the wins model;
#model sig = very high; var sig = only DB sign
anova(wins)

wins_interaction2<- lm(wins~ QB_OL.IPER + QB +  def + RB + DB -1, data=df)
wins_interaction2
summary(wins_interaction2)
pred_wins_interactions2<-predict(wins_interaction2, newdata=test_df2, se=TRUE)
pred_wins_interactions2

fit_df11<-data.frame(wins=test_df2$wins, predicted=pred_wins_interactions2$fit)
#counting the number of correct within 2.5 wins
corr11<-sum(abs(fit_df11$wins - fit_df11$predicted)<2.5)
corr11
per_corr11<-corr11/32
per_corr11# 62.5%