#Loading packages
library(tidyr)
library(dplyr)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(ggplot2)
library(ggpubr)

#Import dataset, dataset is comprised of information from original dataset and LMM results
setwd('C:/Working Directory')
data<-read.csv('Untransformed data.csv', na='-', sep=',') #Raw data, used for plotting
var<-read.csv('Data for back transformation.csv', na='-', sep=';') #Data used to carry out back transformation and create formulae to be plotted

#Creating subsets of data based on seperate Weeks of Thaw (defined by Days Since Snowmelt or DSSM)
data1<-data[which(data$DSSM=='1'),] #dataset for first measurements
data2<-data[which(data$DSSM=='7'|data$DSSM=='8'|data$DSSM=='9'|data$DSSM=='10'),]
data3<-data[which(data$DSSM=='14'|data$DSSM=='15'|data$DSSM=='16'),]
data4<-data[which(data$DSSM=='21'|data$DSSM=='22'|data$DSSM=='23'),]
data5<-data[which(data$DSSM=='28'|data$DSSM=='29'|data$DSSM=='30'),]
data6<-data[which(data$DSSM=='35'|data$DSSM=='36'|data$DSSM=='37'),]
data7<-data[which(data$DSSM=='42'|data$DSSM=='43'|data$DSSM=='44'),]
data8<-data[which(data$DSSM=='49'|data$DSSM=='50'|data$DSSM=='51'),]

#Making subsets based on weeks and variables for back transformation
var1_ALT<-var[which(var$Week=='1'&var$Variable=='ALT'),]
var1_MC<-var[which(var$Week=='1'&var$Variable=='MossC'),]
var1_VC<-var[which(var$Week=='1'&var$Variable=='VascC'),]
var1_PC<-var[which(var$Week=='1'&var$Variable=='PlantC'),]
var1_MH<-var[which(var$Week=='1'&var$Variable=='Avg_Moss_height'),]
var1_VH<-var[which(var$Week=='1'&var$Variable=='Avg_Vasc_height'),]
var1_GH<-var[which(var$Week=='1'&var$Variable=='Avg_Gram_height'),]
var1_cTair<-var[which(var$Week=='1'&var$Variable=='cTair'),]
var1_Tair<-var[which(var$Week=='1'&var$Variable=='cTmeanair'),]
var1_SD<-var[which(var$Week=='1'&var$Variable=='Max_snow'),]
var1_DoSM<-var[which(var$Week=='1'&var$Variable=='DoSM'),]

var2_ALT<-var[which(var$Week=='2'&var$Variable=='ALT'),]
var2_MC<-var[which(var$Week=='2'&var$Variable=='MossC'),]
var2_VC<-var[which(var$Week=='2'&var$Variable=='VascC'),]
var2_PC<-var[which(var$Week=='2'&var$Variable=='PlantC'),]
var2_MH<-var[which(var$Week=='2'&var$Variable=='Avg_Moss_height'),]
var2_VH<-var[which(var$Week=='2'&var$Variable=='Avg_Vasc_height'),]
var2_GH<-var[which(var$Week=='2'&var$Variable=='Avg_Gram_height'),]
var2_cTair<-var[which(var$Week=='2'&var$Variable=='cTair'),]
var2_Tair<-var[which(var$Week=='2'&var$Variable=='cTmeanair'),]
var2_SD<-var[which(var$Week=='2'&var$Variable=='Max_snow'),]
var2_DoSM<-var[which(var$Week=='2'&var$Variable=='DoSM'),]

var3_ALT<-var[which(var$Week=='3'&var$Variable=='ALT'),]
var3_MC<-var[which(var$Week=='3'&var$Variable=='MossC'),]
var3_VC<-var[which(var$Week=='3'&var$Variable=='VascC'),]
var3_PC<-var[which(var$Week=='3'&var$Variable=='PlantC'),]
var3_MH<-var[which(var$Week=='3'&var$Variable=='Avg_Moss_height'),]
var3_VH<-var[which(var$Week=='3'&var$Variable=='Avg_Vasc_height'),]
var3_GH<-var[which(var$Week=='3'&var$Variable=='Avg_Gram_height'),]
var3_cTair<-var[which(var$Week=='3'&var$Variable=='cTair'),]
var3_Tair<-var[which(var$Week=='3'&var$Variable=='cTmeanair'),]
var3_SD<-var[which(var$Week=='3'&var$Variable=='Max_snow'),]
var3_DoSM<-var[which(var$Week=='3'&var$Variable=='DoSM'),]

var4_ALT<-var[which(var$Week=='4'&var$Variable=='ALT'),]
var4_MC<-var[which(var$Week=='4'&var$Variable=='MossC'),]
var4_VC<-var[which(var$Week=='4'&var$Variable=='VascC'),]
var4_PC<-var[which(var$Week=='4'&var$Variable=='PlantC'),]
var4_MH<-var[which(var$Week=='4'&var$Variable=='Avg_Moss_height'),]
var4_VH<-var[which(var$Week=='4'&var$Variable=='Avg_Vasc_height'),]
var4_GH<-var[which(var$Week=='4'&var$Variable=='Avg_Gram_height'),]
var4_cTair<-var[which(var$Week=='4'&var$Variable=='cTair'),]
var4_Tair<-var[which(var$Week=='4'&var$Variable=='cTmeanair'),]
var4_SD<-var[which(var$Week=='4'&var$Variable=='Max_snow'),]
var4_DoSM<-var[which(var$Week=='4'&var$Variable=='DoSM'),]

var5_ALT<-var[which(var$Week=='5'&var$Variable=='ALT'),]
var5_MC<-var[which(var$Week=='5'&var$Variable=='MossC'),]
var5_VC<-var[which(var$Week=='5'&var$Variable=='VascC'),]
var5_PC<-var[which(var$Week=='5'&var$Variable=='PlantC'),]
var5_MH<-var[which(var$Week=='5'&var$Variable=='Avg_Moss_height'),]
var5_VH<-var[which(var$Week=='5'&var$Variable=='Avg_Vasc_height'),]
var5_GH<-var[which(var$Week=='5'&var$Variable=='Avg_Gram_height'),]
var5_cTair<-var[which(var$Week=='5'&var$Variable=='cTair'),]
var5_Tair<-var[which(var$Week=='5'&var$Variable=='cTmeanair'),]
var5_SD<-var[which(var$Week=='5'&var$Variable=='Max_snow'),]
var5_DoSM<-var[which(var$Week=='5'&var$Variable=='DoSM'),]

var6_ALT<-var[which(var$Week=='6'&var$Variable=='ALT'),]
var6_MC<-var[which(var$Week=='6'&var$Variable=='MossC'),]
var6_VC<-var[which(var$Week=='6'&var$Variable=='VascC'),]
var6_PC<-var[which(var$Week=='6'&var$Variable=='PlantC'),]
var6_MH<-var[which(var$Week=='6'&var$Variable=='Avg_Moss_height'),]
var6_VH<-var[which(var$Week=='6'&var$Variable=='Avg_Vasc_height'),]
var6_GH<-var[which(var$Week=='6'&var$Variable=='Avg_Gram_height'),]
var6_cTair<-var[which(var$Week=='6'&var$Variable=='cTair'),]
var6_Tair<-var[which(var$Week=='6'&var$Variable=='cTmeanair'),]
var6_SD<-var[which(var$Week=='6'&var$Variable=='Max_snow'),]
var6_DoSM<-var[which(var$Week=='6'&var$Variable=='DoSM'),]

var7_ALT<-var[which(var$Week=='7'&var$Variable=='ALT'),]
var7_MC<-var[which(var$Week=='7'&var$Variable=='MossC'),]
var7_VC<-var[which(var$Week=='7'&var$Variable=='VascC'),]
var7_PC<-var[which(var$Week=='7'&var$Variable=='PlantC'),]
var7_MH<-var[which(var$Week=='7'&var$Variable=='Avg_Moss_height'),]
var7_VH<-var[which(var$Week=='7'&var$Variable=='Avg_Vasc_height'),]
var7_GH<-var[which(var$Week=='7'&var$Variable=='Avg_Gram_height'),]
var7_cTair<-var[which(var$Week=='7'&var$Variable=='cTair'),]
var7_Tair<-var[which(var$Week=='7'&var$Variable=='cTmeanair'),]
var7_SD<-var[which(var$Week=='7'&var$Variable=='Max_snow'),]
var7_DoSM<-var[which(var$Week=='7'&var$Variable=='DoSM'),]

var8_ALT<-var[which(var$Week=='8'&var$Variable=='ALT'),]
var8_MC<-var[which(var$Week=='8'&var$Variable=='MossC'),]
var8_VC<-var[which(var$Week=='8'&var$Variable=='VascC'),]
var8_PC<-var[which(var$Week=='8'&var$Variable=='PlantC'),]
var8_MH<-var[which(var$Week=='8'&var$Variable=='Avg_Moss_height'),]
var8_VH<-var[which(var$Week=='8'&var$Variable=='Avg_Vasc_height'),]
var8_GH<-var[which(var$Week=='8'&var$Variable=='Avg_Gram_height'),]
var8_cTair<-var[which(var$Week=='8'&var$Variable=='cTair'),]
var8_Tair<-var[which(var$Week=='8'&var$Variable=='cTmeanair'),]
var8_SD<-var[which(var$Week=='8'&var$Variable=='Max_snow'),]
var8_DoSM<-var[which(var$Week=='8'&var$Variable=='DoSM'),]


#Create plots:

####Moss height#####

data1$p_ALT<-(-1*var1_ALT$c)+exp(var1_ALT$MIN+var1_ALT$MAX.MIN*(var1_MH$Alpha+var1_MH$Beta*
((log(var1_MH$c+data1$Avg_Moss_height)-var1_MH$MIN)/var1_MH$MAX.MIN)))

w1<-expression(paste(W[OT],"1"))

MH_w1<-ggplot(data1, aes(x=Avg_Moss_height, y=p_ALT))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=Avg_Moss_height,ALT),shape=1)+theme_bw()+
  xlim(0,3.2)+ylim(0,130)+
  labs(x='', y= 'TD')
  

data2$p_ALT<-(-1*var2_ALT$c)+exp(var2_ALT$MIN+var2_ALT$MAX.MIN*(var2_MH$Alpha+var2_MH$Beta*
((log(var2_MH$c+data2$Avg_Moss_height)-var2_MH$MIN)/var2_MH$MAX.MIN)))

w2<-expression(paste(W[OT],"2"))

MH_w2<-ggplot(data2, aes(x=Avg_Moss_height, y=data2$p_ALT))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=Avg_Moss_height,ALT),shape=1)+theme_bw() +
  xlim(0,3.2)+ylim(0,130)+
  labs(x='', y= '')

data3$p_ALT<-(-1*var3_ALT$c)+exp(var3_ALT$MIN+var3_ALT$MAX.MIN*(var3_MH$Alpha+var3_MH$Beta*
((log(var3_MH$c+data3$Avg_Moss_height)-var3_MH$MIN)/var3_MH$MAX.MIN)))

w3<-expression(paste(W[OT],"3"))

MH_w3<-ggplot(data3, aes(x=Avg_Moss_height, y=p_ALT))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=Avg_Moss_height,ALT),shape=1)+theme_bw() +
  xlim(0,3.2)+ylim(0,130)+
  labs(x='', y= '')

data4$p_ALT<-(-1*var4_ALT$c)+exp(var4_ALT$MIN+var4_ALT$MAX.MIN*(var4_MH$Alpha+var4_MH$Beta*
((log(var4_MH$c+data4$Avg_Moss_height)-var4_MH$MIN)/var4_MH$MAX.MIN)))

w4<-expression(paste(W[OT],"4"))

MH_w4<-ggplot(data4, aes(x=Avg_Moss_height, y=p_ALT))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=Avg_Moss_height,ALT),shape=1)+theme_bw() +
  xlim(0,3.2)+ylim(0,130)+
  labs(x='', y= '')

data5$p_ALT<-log(var5_ALT$MIN+var5_ALT$MAX.MIN*(var5_MH$Alpha+var5_MH$Beta*
((log(var5_MH$c+data5$Avg_Moss_height)-var5_MH$MIN)/var5_MH$MAX.MIN)))/var5_ALT$c

w5<-expression(paste(W[OT],"5"))

MH_w5<-ggplot(data5, aes(x=Avg_Moss_height, y=p_ALT))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=Avg_Moss_height,ALT),shape=1)+theme_bw() +
  xlim(0,3.2)+ylim(0,130)+
  labs(x='', y= '')

data6$p_ALT<-log(var6_ALT$MIN+var6_ALT$MAX.MIN*(var6_MH$Alpha+var6_MH$Beta*
((log(var6_MH$c+data6$Avg_Moss_height)-var6_MH$MIN)/var6_MH$MAX.MIN)))/var6_ALT$c

w6<-expression(paste(W[OT],"6"))

MH_w6<-ggplot(data6, aes(x=Avg_Moss_height, y=p_ALT))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=Avg_Moss_height,ALT),shape=1)+theme_bw() +
  xlim(0,3.2)+ylim(0,130)+
  labs(x='', y= '')

data7$p_ALT<-log(var7_ALT$MIN+var7_ALT$MAX.MIN*(var7_MH$Alpha+var7_MH$Beta*
((log(var7_MH$c+data7$Avg_Moss_height)-var7_MH$MIN)/var7_MH$MAX.MIN)))/var7_ALT$c

w7<-expression(paste(W[OT],"7"))

MH_w7<-ggplot(data7, aes(x=Avg_Moss_height, y=p_ALT))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=Avg_Moss_height,ALT),shape=1)+theme_bw() +
  xlim(0,3.2)+ylim(0,130)+
  labs(x='', y= '')

data8$p_ALT<-log(var8_ALT$MIN+var8_ALT$MAX.MIN*(var8_MH$Alpha+var8_MH$Beta*
((log(var8_MH$c+data8$Avg_Moss_height)-var8_MH$MIN)/var8_MH$MAX.MIN)))/var8_ALT$c

w8<-expression(paste(W[OT],"8"))

MH_w8<-ggplot(data8, aes(x=Avg_Moss_height, y=p_ALT))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=Avg_Moss_height,ALT),shape=1)+theme_bw() +
  xlim(0,3.2)+ylim(0,130)+
  labs(x='', y= '')

ggarrange(MH_w1,MH_w2,MH_w3,MH_w4,MH_w5,MH_w6,MH_w7,MH_w8, ncol=8, nrow=1)


####DoSM####
data1$m_DoSM<-(-1*var1_ALT$c)+exp(var1_ALT$MIN+var1_ALT$MAX.MIN*(var1_DoSM$Alpha+var1_DoSM$Beta*
((log(var1_DoSM$c+data1$DoSM)-var1_DoSM$MIN)/var1_DoSM$MAX.MIN)))

DoSM_w1<-ggplot(data1, aes(x=DoSM, y=m_DoSM))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=DoSM,ALT),shape=1)+theme_bw() +
  xlim(0,50)+ylim(0,130)+
  labs(x='', y= 'TD')

data2$m_DoSM<-(-1*var2_ALT$c)+exp(var2_ALT$MIN+var2_ALT$MAX.MIN*(var2_DoSM$Alpha+var2_DoSM$Beta*
 ((log(var2_DoSM$c+data2$DoSM)-var2_DoSM$MIN)/var2_DoSM$MAX.MIN)))

DoSM_w2<-ggplot(data2, aes(x=DoSM, y=m_DoSM))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=DoSM,ALT),shape=1)+theme_bw() +
  xlim(0,50)+ylim(0,130)+
  labs(x='', y= '')
  
data3$m_DoSM<-(-1*var3_ALT$c)+exp(var3_ALT$MIN+var3_ALT$MAX.MIN*(var3_DoSM$Alpha+var3_DoSM$Beta*
  ((log(var3_DoSM$c+data3$DoSM)-var3_DoSM$MIN)/var3_DoSM$MAX.MIN)))

DoSM_w3<-ggplot(data3, aes(x=DoSM, y=m_DoSM))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=DoSM,ALT),shape=1)+theme_bw() +
  xlim(0,50)+ylim(0,130)+
  labs(x='', y= '')

data4$m_DoSM<-(-1*var4_ALT$c)+exp(var4_ALT$MIN+var4_ALT$MAX.MIN*(var4_DoSM$Alpha+var4_DoSM$Beta*
  ((log(var4_DoSM$c+data4$DoSM)-var4_DoSM$MIN)/var4_DoSM$MAX.MIN)))

DoSM_w4<-ggplot(data4, aes(x=DoSM, y=m_DoSM))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=DoSM,ALT),shape=1)+theme_bw() +
  xlim(0,50)+ylim(0,130)+
  labs(x='', y= '')

data5$m_DoSM<-log(var5_ALT$MIN+var5_ALT$MAX.MIN*(var5_DoSM$Alpha+var5_DoSM$Beta*
 ((log(var5_DoSM$c+data5$DoSM)-var5_DoSM$MIN)/var5_DoSM$MAX.MIN)))/var5_ALT$c

DoSM_w5<-ggplot(data5, aes(x=DoSM, y=m_DoSM))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=DoSM,ALT),shape=1)+theme_bw() +
  xlim(0,50)+ylim(0,130)+
  labs(x='', y= '')

data6$m_DoSM<-log(var6_ALT$MIN+var6_ALT$MAX.MIN*(var6_DoSM$Alpha+var6_DoSM$Beta*
  ((log(var6_DoSM$c+data6$DoSM)-var6_DoSM$MIN)/var6_DoSM$MAX.MIN)))/var6_ALT$c

DoSM_w6<-ggplot(data6, aes(x=DoSM, y=m_DoSM))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=DoSM,ALT),shape=1)+theme_bw() +
  xlim(0,50)+ylim(0,130)+
  labs(x='', y= '')

data7$m_DoSM<-log(var7_ALT$MIN+var7_ALT$MAX.MIN*(var7_DoSM$Alpha+var7_DoSM$Beta*
  ((log(var7_DoSM$c+data7$DoSM)-var7_DoSM$MIN)/var7_DoSM$MAX.MIN)))/var7_ALT$c

DoSM_w7<-ggplot(data7, aes(x=DoSM, y=m_DoSM))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=DoSM,ALT),shape=1)+theme_bw() +
  xlim(0,50)+ylim(0,130)+
  labs(x='', y= '')

data8$m_DoSM<-log(var8_ALT$MIN+var8_ALT$MAX.MIN*(var8_DoSM$Alpha+var8_DoSM$Beta*
 ((log(var8_DoSM$c+data8$DoSM)-var8_DoSM$MIN)/var8_DoSM$MAX.MIN)))/var8_ALT$c

DoSM_w8<-ggplot(data8, aes(x=DoSM, y=m_DoSM))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=DoSM,ALT),shape=1)+theme_bw() +
  xlim(0,50)+ylim(0,130)+
  labs(x='', y= '')

f<-ggarrange(DoSM_w1,DoSM_w2,DoSM_w3,DoSM_w4,DoSM_w5,DoSM_w6,DoSM_w7,DoSM_w8, ncol=8, nrow=1)


####cTair####
data1$m_Tair<-(-1*var1_ALT$c)+exp(var1_ALT$MIN+var1_ALT$MAX.MIN*(var1_Tair$Alpha+var1_Tair$Beta*
 ((log(var1_Tair$c+data1$cTmeanair)-var1_Tair$MIN)/var1_Tair$MAX.MIN)))

cTair_w1<-ggplot(data1, aes(x=cTmeanair, y=m_Tair))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=cTmeanair,ALT),shape=1)+theme_bw() +
  xlim(0,30.769)+ylim(0,12.5)+
  labs(x='', y='TD')+
  theme(axis.text=element_text(face='italic'))

data2$m_Tair<-(-1*var2_ALT$c)+exp(var2_ALT$MIN+var2_ALT$MAX.MIN*(var2_Tair$Alpha+var2_Tair$Beta*
  ((exp(var2_Tair$c*data2$cTmeanair)-var2_Tair$MIN)/var2_Tair$MAX.MIN)))

cTair_w2<-ggplot(data2, aes(x=cTmeanair, y=m_Tair))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=cTmeanair,ALT),shape=1)+theme_bw() +
  xlim(0,96.648)+ylim(0,40)+
  labs(x='', y= '')+
  theme(axis.text=element_text(face='italic'))

data3$m_Tair<-(-1*var3_ALT$c)+exp(var3_ALT$MIN+var3_ALT$MAX.MIN*(var3_Tair$Alpha+var3_Tair$Beta*
   ((exp(var3_Tair$c*data3$cTmeanair)-var3_Tair$MIN)/var3_Tair$MAX.MIN)))

cTair_w3<-ggplot(data3, aes(x=cTmeanair, y=m_Tair))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=cTmeanair,ALT),shape=1)+theme_bw() +
  xlim(0,144.972)+ylim(0,60)+
  labs(x='', y= '')+
  theme(axis.text=element_text(face='italic'))

data4$m_Tair<-(-1*var4_ALT$c)+exp(var4_ALT$MIN+var4_ALT$MAX.MIN*(var4_Tair$Alpha+var4_Tair$Beta*
 ((log(var4_Tair$c+data4$cTmeanair)-var4_Tair$MIN)/var4_Tair$MAX.MIN)))

cTair_w4<-ggplot(data4, aes(x=cTmeanair, y=m_Tair))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=cTmeanair,ALT),shape=1)+theme_bw() +
  xlim(0,200)+ylim(10,92.7815)+
  labs(x='', y='')+
  theme(axis.text=element_text(face='italic'))
  

data5$m_Tair<-log(var5_ALT$MIN+var5_ALT$MAX.MIN*(var5_Tair$Alpha+var5_Tair$Beta*
   ((log(var5_Tair$c+data5$cTmeanair)-var5_Tair$MIN)/var5_Tair$MAX.MIN)))/var5_ALT$c

cTair_w5<-ggplot(data5, aes(x=cTmeanair, y=m_Tair))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=cTmeanair,ALT),shape=1)+theme_bw() +
  xlim(25,206.215)+ylim(25,100)+
  labs(x='', y= '')+
  theme(axis.text=element_text(face='italic'))

data6$m_Tair<-log(var6_ALT$MIN+var6_ALT$MAX.MIN*(var6_Tair$Alpha+var6_Tair$Beta*
  ((log(var6_Tair$c+data6$cTmeanair)-var6_Tair$MIN)/var6_Tair$MAX.MIN)))/var6_ALT$c

cTair_w6<-ggplot(data6, aes(x=cTmeanair, y=m_Tair))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=cTmeanair,ALT),shape=1)+theme_bw() +
  xlim(75,250)+ylim(30,102.42778)+
  labs(x='', y= '')+
  theme(axis.text=element_text(face='italic'))

data7$m_Tair<-log(var7_ALT$MIN+var7_ALT$MAX.MIN*(var7_Tair$Alpha+var7_Tair$Beta*
 ((exp(var7_Tair$c*data7$cTmeanair)-var7_Tair$MIN)/var7_Tair$MAX.MIN)))/var7_ALT$c

cTair_w7<-ggplot(data7, aes(x=cTmeanair, y=m_Tair))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=cTmeanair,ALT),shape=1)+theme_bw() +
  xlim(120,320)+ylim(40,123)+
  labs(x='', y= '')+
  theme(axis.text=element_text(face='italic'))

data8$m_Tair<-log(var8_ALT$MIN+var8_ALT$MAX.MIN*(var8_Tair$Alpha+var8_Tair$Beta*
 ((exp(var8_Tair$c*data8$cTmeanair)-var8_Tair$MIN)/var8_Tair$MAX.MIN)))/var8_ALT$c

cTair_w8<-ggplot(data8, aes(x=cTmeanair, y=m_Tair))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=cTmeanair,ALT),shape=1)+theme_bw() +
  xlim(120,320)+ylim(50,133)+
  labs(x='', y= '')+
  theme(axis.text=element_text(face='italic'))

ggarrange(cTair_w1,cTair_w2,cTair_w3,cTair_w4,cTair_w5,cTair_w6,cTair_w7,cTair_w8, ncol=8, nrow=1)


####Snow Depth#####
data1$m_SD<-(-1*var1_ALT$c)+exp(var1_ALT$MIN+var1_ALT$MAX.MIN*(var1_SD$Alpha+var1_SD$Beta*
  ((log(var1_SD$c+data1$Max_snow)-var1_SD$MIN)/var1_SD$MAX.MIN)))

Max_snow_w1<-ggplot(data1, aes(x=Max_snow, y=m_SD))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=Max_snow,ALT),shape=1)+theme_bw() +
  xlim(0,190)+ylim(0,130)+
  labs(x='', y='TD')

data2$m_SD<-(-1*var2_ALT$c)+exp(var2_ALT$MIN+var2_ALT$MAX.MIN*(var2_SD$Alpha+var2_SD$Beta*
 ((log(var2_SD$c+data2$Max_snow)-var2_SD$MIN)/var2_SD$MAX.MIN)))

Max_snow_w2<-ggplot(data2, aes(x=Max_snow, y=m_SD))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=Max_snow,ALT),shape=1)+theme_bw() +
  xlim(0,190)+ylim(0,130)+
  labs(x='', y= '')

data3$m_SD<-(-1*var3_ALT$c)+exp(var3_ALT$MIN+var3_ALT$MAX.MIN*(var3_SD$Alpha+var3_SD$Beta*
   ((log(var3_SD$c+data3$Max_snow)-var3_SD$MIN)/var3_SD$MAX.MIN)))

Max_snow_w3<-ggplot(data3, aes(x=Max_snow, y=m_SD))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=Max_snow,ALT),shape=1)+theme_bw() +
  xlim(0,190)+ylim(0,130)+
  labs(x='', y= '')

data4$m_SD<-(-1*var4_ALT$c)+exp(var4_ALT$MIN+var4_ALT$MAX.MIN*(var4_SD$Alpha+var4_SD$Beta*
    ((log(var4_SD$c+data4$Max_snow)-var4_SD$MIN)/var4_SD$MAX.MIN)))

Max_snow_w4<-ggplot(data4, aes(x=Max_snow, y=m_SD))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=Max_snow,ALT),shape=1)+theme_bw() +
  xlim(0,190)+ylim(0,130)+
  labs(x='', y='')

data5$m_SD<-log(var5_ALT$MIN+var5_ALT$MAX.MIN*(var5_SD$Alpha+var5_SD$Beta*
   ((log(var5_SD$c+data5$Max_snow)-var5_SD$MIN)/var5_SD$MAX.MIN)))/var5_ALT$c

Max_snow_w5<-ggplot(data5, aes(x=Max_snow, y=m_SD))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=Max_snow,ALT),shape=1)+theme_bw() +
  xlim(0,190)+ylim(0,130)+
  labs(x='', y= '')

data6$m_SD<-log(var6_ALT$MIN+var6_ALT$MAX.MIN*(var6_SD$Alpha+var6_SD$Beta*
      ((log(var6_SD$c+data6$Max_snow)-var6_SD$MIN)/var6_SD$MAX.MIN)))/var6_ALT$c

Max_snow_w6<-ggplot(data6, aes(x=Max_snow, y=m_SD))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=Max_snow,ALT),shape=1)+theme_bw() +
  xlim(0,190)+ylim(0,130)+
  labs(x='', y= '')

data7$m_SD<-log(var7_ALT$MIN+var7_ALT$MAX.MIN*(var7_SD$Alpha+var7_SD$Beta*
    ((log(var7_SD$c+data7$Max_snow)-var7_SD$MIN)/var7_SD$MAX.MIN)))/var7_ALT$c

Max_snow_w7<-ggplot(data7, aes(x=Max_snow, y=m_SD))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=Max_snow,ALT),shape=1)+theme_bw() +
  xlim(0,190)+ylim(0,130)+
  labs(x='', y= '')

data8$m_SD<-log(var8_ALT$MIN+var8_ALT$MAX.MIN*(var8_SD$Alpha+var8_SD$Beta*
     ((log(var8_SD$c+data8$Max_snow)-var8_SD$MIN)/var8_SD$MAX.MIN)))/var8_ALT$c

Max_snow_w8<-ggplot(data8, aes(x=Max_snow, y=m_SD))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=Max_snow,ALT),shape=1)+theme_bw() +
  xlim(0,190)+ylim(0,130)+
  labs(x='', y= '')

ggarrange(Max_snow_w1,Max_snow_w2,Max_snow_w3,Max_snow_w4,Max_snow_w5,Max_snow_w6,Max_snow_w7,Max_snow_w8, ncol=8, nrow=1)


####Vasc height:#####
data1$m_VH<-(-1*var1_ALT$c)+exp(var1_ALT$MIN+var1_ALT$MAX.MIN*(var1_VH$Alpha+var1_VH$Beta*
     ((log(var1_VH$c+data1$Avg_Vasc_height)-var1_VH$MIN)/var1_VH$MAX.MIN)))

VH_w1<-ggplot(data1, aes(x=Avg_Vasc_height, y=m_VH))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=Avg_Vasc_height,ALT),shape=1)+theme_bw() +
  xlim(0,6)+ylim(0,130)+
  labs(x='', y= 'TD')

data2$m_VH<-(-1*var2_ALT$c)+exp(var2_ALT$MIN+var2_ALT$MAX.MIN*(var2_VH$Alpha+var2_VH$Beta*
     ((log(var2_VH$c+data2$Avg_Vasc_height)-var2_VH$MIN)/var2_VH$MAX.MIN)))

VH_w2<-ggplot(data2, aes(x=Avg_Vasc_height, y=data2$m_VH))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=Avg_Vasc_height,ALT),shape=1)+theme_bw() +
  xlim(0,6)+ylim(0,130)+
  labs(x='', y= '')

data3$m_VH<-(-1*var3_ALT$c)+exp(var3_ALT$MIN+var3_ALT$MAX.MIN*(var3_VH$Alpha+var3_VH$Beta*
     ((log(var3_VH$c+data3$Avg_Vasc_height)-var3_VH$MIN)/var3_VH$MAX.MIN)))

VH_w3<-ggplot(data3, aes(x=Avg_Vasc_height, y=m_VH))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=Avg_Vasc_height,ALT),shape=1)+theme_bw() +
  xlim(0,6)+ylim(0,130)+
  labs(x='', y= '')

data4$m_VH<-(-1*var4_ALT$c)+exp(var4_ALT$MIN+var4_ALT$MAX.MIN*(var4_VH$Alpha+var4_VH$Beta*
     ((log(var4_VH$c+data4$Avg_Vasc_height)-var4_VH$MIN)/var4_VH$MAX.MIN)))

VH_w4<-ggplot(data4, aes(x=Avg_Vasc_height, y=m_VH))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=Avg_Vasc_height,ALT),shape=1)+theme_bw() +
  xlim(0,6)+ylim(0,130)+
  labs(x='', y= '')

data5$m_VH<-log(var5_ALT$MIN+var5_ALT$MAX.MIN*(var5_VH$Alpha+var5_VH$Beta*
     ((log(var5_VH$c+data5$Avg_Vasc_height)-var5_VH$MIN)/var5_VH$MAX.MIN)))/var5_ALT$c

VH_w5<-ggplot(data5, aes(x=Avg_Vasc_height, y=m_VH))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=Avg_Vasc_height,ALT),shape=1)+theme_bw() +
  xlim(0,6)+ylim(0,130)+
  labs(x='', y= '')

data6$m_VH<-log(var6_ALT$MIN+var6_ALT$MAX.MIN*(var6_VH$Alpha+var6_VH$Beta*
     ((log(var6_VH$c+data6$Avg_Vasc_height)-var6_VH$MIN)/var6_VH$MAX.MIN)))/var6_ALT$c

VH_w6<-ggplot(data6, aes(x=Avg_Vasc_height, y=m_VH))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=Avg_Vasc_height,ALT),shape=1)+theme_bw() +
  xlim(0,6)+ylim(0,130)+
  labs(x='', y= '')

data7$m_VH<-log(var7_ALT$MIN+var7_ALT$MAX.MIN*(var7_VH$Alpha+var7_VH$Beta*
     ((log(var7_VH$c+data7$Avg_Vasc_height)-var7_VH$MIN)/var7_VH$MAX.MIN)))/var7_ALT$c

VH_w7<-ggplot(data7, aes(x=Avg_Vasc_height, y=m_VH))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=Avg_Vasc_height,ALT),shape=1)+theme_bw() +
  xlim(0,6)+ylim(0,130)+
  labs(x='', y= '')

data8$m_VH<-log(var8_ALT$MIN+var8_ALT$MAX.MIN*(var8_VH$Alpha+var8_VH$Beta*
     ((log(var8_VH$c+data8$Avg_Vasc_height)-var8_VH$MIN)/var8_VH$MAX.MIN)))/var8_ALT$c

VH_w8<-ggplot(data8, aes(x=Avg_Vasc_height, y=m_VH))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=Avg_Vasc_height,ALT),shape=1)+theme_bw() +
  xlim(0,6)+ylim(0,130)+
  labs(x='', y= '')


ggarrange(VH_w1,VH_w2,VH_w3,VH_w4,VH_w5,VH_w6,VH_w7,VH_w8, ncol=8, nrow=1)

####VascC:####
data1$m_VC<-(-1*var1_ALT$c)+exp(var1_ALT$MIN+var1_ALT$MAX.MIN*(var1_VC$Alpha+var1_VC$Beta*
                                                                  ((log(var1_VC$c+data1$VascC)-var1_VC$MIN)/var1_VC$MAX.MIN)))

VC_w1<-ggplot(data1, aes(x=VascC, y=m_VC))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=VascC,ALT),shape=1)+theme_bw() +
  xlim(0,120)+ylim(0,130)+
  labs(x='', y= 'TD')

data2$m_VC<-(-1*var2_ALT$c)+exp(var2_ALT$MIN+var2_ALT$MAX.MIN*(var2_VC$Alpha+var2_VC$Beta*
                                                                  ((log(var2_VC$c+data2$VascC)-var2_VC$MIN)/var2_VC$MAX.MIN)))

VC_w2<-ggplot(data2, aes(x=VascC, y=data2$m_VC))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=VascC,ALT),shape=1)+theme_bw() +
  xlim(0,120)+ylim(0,130)+
  labs(x='', y= '')


data3$m_VC<-(-1*var3_ALT$c)+exp(var3_ALT$MIN+var3_ALT$MAX.MIN*(var3_VC$Alpha+var3_VC$Beta*
                                                                  ((log(var3_VC$c+data3$VascC)-var3_VC$MIN)/var3_VC$MAX.MIN)))

VC_w3<-ggplot(data3, aes(x=VascC, y=m_VC))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=VascC,ALT),shape=1)+theme_bw() +
  xlim(0,120)+ylim(0,130)+
  labs(x='', y= '')


data4$m_VC<-(-1*var4_ALT$c)+exp(var4_ALT$MIN+var4_ALT$MAX.MIN*(var4_VC$Alpha+var4_VC$Beta*
                                                                  ((log(var4_VC$c+data4$VascC)-var4_VC$MIN)/var4_VC$MAX.MIN)))

VC_w4<-ggplot(data4, aes(x=VascC, y=m_VC))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=VascC,ALT),shape=1)+theme_bw() +
  xlim(0,120)+ylim(0,130)+
  labs(x='', y= '')


data5$m_VC<-log(var5_ALT$MIN+var5_ALT$MAX.MIN*(var5_VC$Alpha+var5_VC$Beta*
                                                  ((log(var5_VC$c+data5$VascC)-var5_VC$MIN)/var5_VC$MAX.MIN)))/var5_ALT$c

VC_w5<-ggplot(data5, aes(x=VascC, y=m_VC))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=VascC,ALT),shape=1)+theme_bw() +
  xlim(0,120)+ylim(0,130)+
  labs(x='', y= '')


data6$m_VC<-log(var6_ALT$MIN+var6_ALT$MAX.MIN*(var6_VC$Alpha+var6_VC$Beta*
                                                  ((log(var6_VC$c+data6$VascC)-var6_VC$MIN)/var6_VC$MAX.MIN)))/var6_ALT$c

VC_w6<-ggplot(data6, aes(x=VascC, y=m_VC))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=VascC,ALT),shape=1)+theme_bw() +
  xlim(0,120)+ylim(0,130)+
  labs(x='', y= '')


data7$m_VC<-log(var7_ALT$MIN+var7_ALT$MAX.MIN*(var7_VC$Alpha+var7_VC$Beta*
                                                  ((log(var7_VC$c+data7$VascC)-var7_VC$MIN)/var7_VC$MAX.MIN)))/var7_ALT$c

VC_w7<-ggplot(data7, aes(x=VascC, y=m_VC))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=VascC,ALT),shape=1)+theme_bw() +
  xlim(0,120)+ylim(0,130)+
  labs(x='', y= '')


data8$m_VC<-log(var8_ALT$MIN+var8_ALT$MAX.MIN*(var8_VC$Alpha+var8_VC$Beta*
                                                  ((log(var8_VC$c+data8$VascC)-var8_VC$MIN)/var8_VC$MAX.MIN)))/var8_ALT$c

VC_w8<-ggplot(data8, aes(x=VascC, y=m_VC))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=VascC,ALT),shape=1)+theme_bw() +
  xlim(0,120)+ylim(0,130)+
  labs(x='', y= '')


ggarrange(VC_w1,VC_w2,VC_w3,VC_w4,VC_w5,VC_w6,VC_w7,VC_w8, ncol=8, nrow=1)

####Gram height####
data1$m_GH<-(-1*var1_ALT$c)+exp(var1_ALT$MIN+var1_ALT$MAX.MIN*(var1_GH$Alpha+var1_GH$Beta*
                                                                 ((log(var1_GH$c+data1$Avg_Gram_height)-var1_GH$MIN)/var1_GH$MAX.MIN)))

GH_w1<-ggplot(data1, aes(x=Avg_Gram_height, y=m_GH))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=Avg_Gram_height,ALT),shape=1)+theme_bw() +
  xlim(0,13)+ylim(0,130)+
  labs(x='', y= 'TD')


data2$m_GH<-(-1*var2_ALT$c)+exp(var2_ALT$MIN+var2_ALT$MAX.MIN*(var2_GH$Alpha+var2_GH$Beta*
            ((exp(var2_GH$c*data2$Avg_Gram_height)-var2_GH$MIN)/var2_GH$MAX.MIN)))

GH_w2<-ggplot(data2, aes(x=Avg_Gram_height, y=data2$m_GH))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=Avg_Gram_height,ALT),shape=1)+theme_bw() +
  xlim(0,13)+ylim(0,130)+
  labs(x='', y= '')


data3$m_GH<-(-1*var3_ALT$c)+exp(var3_ALT$MIN+var3_ALT$MAX.MIN*(var3_GH$Alpha+var3_GH$Beta*
                                                                 ((log(var3_GH$c+data3$Avg_Gram_height)-var3_GH$MIN)/var3_GH$MAX.MIN)))

GH_w3<-ggplot(data3, aes(x=Avg_Gram_height, y=m_GH))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=Avg_Gram_height,ALT),shape=1)+theme_bw() +
  xlim(0,13)+ylim(0,130)+
  labs(x='', y= '')


data4$m_GH<-(-1*var4_ALT$c)+exp(var4_ALT$MIN+var4_ALT$MAX.MIN*(var4_GH$Alpha+var4_GH$Beta*
                                                                 ((log(var4_GH$c+data4$Avg_Gram_height)-var4_GH$MIN)/var4_GH$MAX.MIN)))

GH_w4<-ggplot(data4, aes(x=Avg_Gram_height, y=m_GH))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=Avg_Gram_height,ALT),shape=1)+theme_bw() +
  xlim(0,13)+ylim(0,130)+
  labs(x='', y= '')

data5$m_GH<-log(var5_ALT$MIN+var5_ALT$MAX.MIN*(var5_GH$Alpha+var5_GH$Beta*
                                                 ((log(var5_GH$c+data5$Avg_Gram_height)-var5_GH$MIN)/var5_GH$MAX.MIN)))/var5_ALT$c

GH_w5<-ggplot(data5, aes(x=Avg_Gram_height, y=m_GH))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=Avg_Gram_height,ALT),shape=1)+theme_bw() +
  xlim(0,13)+ylim(0,130)+
  labs(x='', y= '')


data6$m_GH<-log(var6_ALT$MIN+var6_ALT$MAX.MIN*(var6_GH$Alpha+var6_GH$Beta*
                                                 ((log(var6_GH$c+data6$Avg_Gram_height)-var6_GH$MIN)/var6_GH$MAX.MIN)))/var6_ALT$c

GH_w6<-ggplot(data6, aes(x=Avg_Gram_height, y=m_GH))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=Avg_Gram_height,ALT),shape=1)+theme_bw() +
  xlim(0,13)+ylim(0,130)+
  labs(x='', y= '')


data7$m_GH<-log(var7_ALT$MIN+var7_ALT$MAX.MIN*(var7_GH$Alpha+var7_GH$Beta*
                                                 ((log(var7_GH$c+data7$Avg_Gram_height)-var7_GH$MIN)/var7_GH$MAX.MIN)))/var7_ALT$c

GH_w7<-ggplot(data7, aes(x=Avg_Gram_height, y=m_GH))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=Avg_Gram_height,ALT),shape=1)+theme_bw() +
  xlim(0,13)+ylim(0,130)+
  labs(x='', y= '')


data8$m_GH<-log(var8_ALT$MIN+var8_ALT$MAX.MIN*(var8_GH$Alpha+var8_GH$Beta*
                                                 ((log(var8_GH$c+data8$Avg_Gram_height)-var8_GH$MIN)/var8_GH$MAX.MIN)))/var8_ALT$c

GH_w8<-ggplot(data8, aes(x=Avg_Gram_height, y=m_GH))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=Avg_Gram_height,ALT),shape=1)+theme_bw() +
  xlim(0,13)+ylim(0,130)+
  labs(x='', y= '')

ggarrange(GH_w1,GH_w2,GH_w3,GH_w4,GH_w5,GH_w6,GH_w7,GH_w8, ncol=8, nrow=1)


####PlantC:####
data1$m_PC<-(-1*var1_ALT$c)+exp(var1_ALT$MIN+var1_ALT$MAX.MIN*(var1_PC$Alpha+var1_PC$Beta*
                                                                 ((log(var1_PC$c+data1$PlantC)-var1_PC$MIN)/var1_PC$MAX.MIN)))

PC_w1<-ggplot(data1, aes(x=PlantC, y=m_PC))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=PlantC,ALT),shape=1)+theme_bw() +
  xlim(0,175)+ylim(0,130)+
  labs(x='', y= 'TD')

data2$m_PC<-(-1*var2_ALT$c)+exp(var2_ALT$MIN+var2_ALT$MAX.MIN*(var2_PC$Alpha+var2_PC$Beta*
                                                                 ((log(var2_PC$c+data2$PlantC)-var2_PC$MIN)/var2_PC$MAX.MIN)))

PC_w2<-ggplot(data2, aes(x=PlantC, y=data2$m_PC))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=PlantC,ALT),shape=1)+theme_bw() +
  xlim(0,175)+ylim(0,130)+
  labs(x='', y= '')


data3$m_PC<-(-1*var3_ALT$c)+exp(var3_ALT$MIN+var3_ALT$MAX.MIN*(var3_PC$Alpha+var3_PC$Beta*
                                                                 ((log(var3_PC$c+data3$PlantC)-var3_PC$MIN)/var3_PC$MAX.MIN)))

PC_w3<-ggplot(data3, aes(x=PlantC, y=m_PC))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=PlantC,ALT),shape=1)+theme_bw() +
  xlim(0,175)+ylim(0,130)+
  labs(x='', y= '')


data4$m_PC<-(-1*var4_ALT$c)+exp(var4_ALT$MIN+var4_ALT$MAX.MIN*(var4_PC$Alpha+var4_PC$Beta*
                                                                 ((log(var4_PC$c+data4$PlantC)-var4_PC$MIN)/var4_PC$MAX.MIN)))

PC_w4<-ggplot(data4, aes(x=PlantC, y=m_PC))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=PlantC,ALT),shape=1)+theme_bw() +
  xlim(0,175)+ylim(0,130)+
  labs(x='', y= '')


data5$m_PC<-log(var5_ALT$MIN+var5_ALT$MAX.MIN*(var5_PC$Alpha+var5_PC$Beta*
                                                 ((log(var5_PC$c+data5$PlantC)-var5_PC$MIN)/var5_PC$MAX.MIN)))/var5_ALT$c

PC_w5<-ggplot(data5, aes(x=PlantC, y=m_PC))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=PlantC,ALT),shape=1)+theme_bw() +
  xlim(0,175)+ylim(0,130)+
  labs(x='', y= '')


data6$m_PC<-log(var6_ALT$MIN+var6_ALT$MAX.MIN*(var6_PC$Alpha+var6_PC$Beta*
                                                 ((log(var6_PC$c+data6$PlantC)-var6_PC$MIN)/var6_PC$MAX.MIN)))/var6_ALT$c

PC_w6<-ggplot(data6, aes(x=PlantC, y=m_PC))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=PlantC,ALT),shape=1)+theme_bw() +
  xlim(0,175)+ylim(0,130)+
  labs(x='', y= '')


data7$m_PC<-log(var7_ALT$MIN+var7_ALT$MAX.MIN*(var7_PC$Alpha+var7_PC$Beta*
                                                 ((log(var7_PC$c+data7$PlantC)-var7_PC$MIN)/var7_PC$MAX.MIN)))/var7_ALT$c

PC_w7<-ggplot(data7, aes(x=PlantC, y=m_PC))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=PlantC,ALT),shape=1)+theme_bw() +
  xlim(0,175)+ylim(0,130)+
  labs(x='', y= '')


data8$m_PC<-log(var8_ALT$MIN+var8_ALT$MAX.MIN*(var8_PC$Alpha+var8_PC$Beta*
                                                 ((log(var8_PC$c+data8$PlantC)-var8_PC$MIN)/var8_PC$MAX.MIN)))/var8_ALT$c

PC_w8<-ggplot(data8, aes(x=PlantC, y=m_PC))+geom_line(col='blue',linewidth=0.8)+
  geom_point(aes(x=PlantC,ALT),shape=1)+theme_bw() +
  xlim(0,175)+ylim(0,130)+
  labs(x='', y= '')


ggarrange(PC_w1,PC_w2,PC_w3,PC_w4,PC_w5,PC_w6,PC_w7,PC_w8, ncol=8, nrow=1)

####All together now:####

ggarrange(MH_w1,MH_w2,MH_w3,MH_w4,MH_w5,MH_w6,MH_w7,MH_w8,
         VH_w1,VH_w2,VH_w3,VH_w4,VH_w5,VH_w6,VH_w7,VH_w8,
         GH_w1,GH_w2,GH_w3,GH_w4,GH_w5,GH_w6,GH_w7,GH_w8,
         VC_w1,VC_w2,VC_w3,VC_w4,VC_w5,VC_w6,VC_w7,VC_w8,
         PC_w1,PC_w2,PC_w3,PC_w4,PC_w5,PC_w6,PC_w7,PC_w8,
         DoSM_w1,DoSM_w2,DoSM_w3,DoSM_w4,DoSM_w5,DoSM_w6,DoSM_w7,DoSM_w8,
         cTair_w1,cTair_w2,cTair_w3,cTair_w4,cTair_w5,cTair_w6,cTair_w7,cTair_w8,
         Max_snow_w1,Max_snow_w2,Max_snow_w3,Max_snow_w4,Max_snow_w5,Max_snow_w6,Max_snow_w7,Max_snow_w8,
         ncol=8, nrow=8)

#Subset biotic:
ggarrange(MH_w1,MH_w2,MH_w3,MH_w4,MH_w5,MH_w6,MH_w7,MH_w8,
          VH_w1,VH_w2,VH_w3,VH_w4,VH_w5,VH_w6,VH_w7,VH_w8,
          GH_w1,GH_w2,GH_w3,GH_w4,GH_w5,GH_w6,GH_w7,GH_w8,
          VC_w1,VC_w2,VC_w3,VC_w4,VC_w5,VC_w6,VC_w7,VC_w8,
          PC_w1,PC_w2,PC_w3,PC_w4,PC_w5,PC_w6,PC_w7,PC_w8,
          ncol=8, nrow=5)

#Subset abiotic:
ggarrange(DoSM_w1,DoSM_w2,DoSM_w3,DoSM_w4,DoSM_w5,DoSM_w6,DoSM_w7,DoSM_w8,
          cTair_w1,cTair_w2,cTair_w3,cTair_w4,cTair_w5,cTair_w6,cTair_w7,cTair_w8,
          Max_snow_w1,Max_snow_w2,Max_snow_w3,Max_snow_w4,Max_snow_w5,Max_snow_w6,Max_snow_w7,Max_snow_w8,
          ncol=8, nrow=3)

