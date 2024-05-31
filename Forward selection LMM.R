library(lme4)
library(nlme)
library(tidyr)
library(dplyr)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(MuMIn)

setwd('C:/Users/sils/OneDrive - Universitetssenteret på Svalbard AS/Work folder/PhD UNIS/Analysis/Finnish plots/Data/Fieldwork 2021/Using for analysis 2022')


setwd('C:/Working directory')

####WOT 1:####

#Get data
data1<-read.csv('Week 1.csv', na='-', sep=';')


#Round 1:
nlme1.1<-lme(ALT~MossC, random=~1|Grid, data=data1, na.action = na.omit)
nlme1.2<-lme(ALT~VascC, random=~1|Grid, data=data1, na.action = na.omit)
nlme1.3<-lme(ALT~LichenC, random=~1|Grid, data=data1, na.action = na.omit)
nlme1.4<-lme(ALT~RockC, random=~1|Grid, data=data1, na.action = na.omit)
nlme1.5<-lme(ALT~Avg_Gram_height, random=~1|Grid, data=data1, na.action = na.omit)
nlme1.6<-lme(ALT~Avg_Moss_height, random=~1|Grid, data=data1, na.action = na.omit)
nlme1.7<-lme(ALT~Avg_Vasc_height, random=~1|Grid, data=data1, na.action = na.omit)
nlme1.8<-lme(ALT~cMm, random=~1|Grid, data=data1, na.action = na.omit)
nlme1.9<-lme(ALT~Relative_elevation, random=~1|Grid, data=data1, na.action = na.omit)
nlme1.10<-lme(ALT~cTmeanair, random=~1|Grid, data=data1, na.action = na.omit)
nlme1.11<-lme(ALT~Org_layer, random=~1|Grid, data=data1, na.action = na.omit)
nlme1.12<-lme(ALT~SD_height, random=~1|Grid, data=data1, na.action = na.omit)
nlme1.13<-lme(ALT~DoSM, random=~1|Grid, data=data1, na.action = na.omit)
nlme1.14<-lme(ALT~Max_snow, random=~1|Grid, data=data1, na.action = na.omit)
nlme1.15<-lme(ALT~Aspect, random=~1|Grid, data=data1, na.action = na.omit)
nlme1.16<-lme(ALT~Slope, random=~1|Grid, data=data1, na.action = na.omit)

#Results:
summary(nlme1.1)#BIC=20.465
summary(nlme1.2)#BIC=20.964
summary(nlme1.3)#BIC=21.249
summary(nlme1.4)#BIC=25.474
summary(nlme1.5)#BIC=18.018
summary(nlme1.6)#BIC=16.071, lowest value
summary(nlme1.7)#BIC=18.244
summary(nlme1.8)#BIC=18.761
summary(nlme1.9)#BIC=20.223
summary(nlme1.10)#BIC=18.651
summary(nlme1.11)#BIC=16.299
summary(nlme1.12)#BIC=20.826
summary(nlme1.13)#BIC=19.593
summary(nlme1.14)#BIC=21.031
summary(nlme1.15)#BIC=21.402
summary(nlme1.16)#BIC=20.886

#Round 2:
nlme1.6.1<-lme(ALT~Avg_Moss_height+MossC, random=~1|Grid, data=data1, na.action = na.omit)
nlme1.6.2<-lme(ALT~Avg_Moss_height+VascC, random=~1|Grid, data=data1, na.action = na.omit)
nlme1.6.3<-lme(ALT~Avg_Moss_height+LichenC, random=~1|Grid, data=data1, na.action = na.omit)
nlme1.6.4<-lme(ALT~Avg_Moss_height+RockC, random=~1|Grid, data=data1, na.action = na.omit)
nlme1.6.5<-lme(ALT~Avg_Moss_height+Avg_Gram_height, random=~1|Grid, data=data1, na.action = na.omit)
nlme1.6.7<-lme(ALT~Avg_Moss_height+Avg_Vasc_height, random=~1|Grid, data=data1, na.action = na.omit)
nlme1.6.8<-lme(ALT~Avg_Moss_height+cMm, random=~1|Grid, data=data1, na.action = na.omit)
nlme1.6.9<-lme(ALT~Avg_Moss_height+Relative_elevation, random=~1|Grid, data=data1, na.action = na.omit)
nlme1.6.10<-lme(ALT~Avg_Moss_height+cTmeanair, random=~1|Grid, data=data1, na.action = na.omit)
nlme1.6.11<-lme(ALT~Avg_Moss_height+Org_layer, random=~1|Grid, data=data1, na.action = na.omit)
nlme1.6.12<-lme(ALT~Avg_Moss_height+SD_height, random=~1|Grid, data=data1, na.action = na.omit)
nlme1.6.13<-lme(ALT~Avg_Moss_height+DoSM, random=~1|Grid, data=data1, na.action = na.omit)
nlme1.6.14<-lme(ALT~Avg_Moss_height+Max_snow, random=~1|Grid, data=data1, na.action = na.omit)
nlme1.6.15<-lme(ALT~Avg_Moss_height+Aspect, random=~1|Grid, data=data1, na.action = na.omit)
nlme1.6.16<-lme(ALT~Avg_Moss_height+Slope, random=~1|Grid, data=data1, na.action = na.omit)

#Results, threshold BIC 16.071:
summary(nlme1.6.1)#BIC=22.510
summary(nlme1.6.2)#BIC=22.759
summary(nlme1.6.3)#BIC=22.633
summary(nlme1.6.4)#BIC=29.425
summary(nlme1.6.5)#BIC=21.553
summary(nlme1.6.7)#BIC=21.842
summary(nlme1.6.8)#BIC=20.271
summary(nlme1.6.9)#BIC=19.212
summary(nlme1.6.10)#BIC=19.446
summary(nlme1.6.11)#BIC=20.924
summary(nlme1.6.12)#BIC=22.470
summary(nlme1.6.13)#BIC=20.022
summary(nlme1.6.14)#BIC=21.332
summary(nlme1.6.15)#BIC=22.643
summary(nlme1.6.16)#BIC=22.328

#No improvement, no further selection
#Best model ALT~Moss thickness:
nlme1.6<-lme(ALT~Avg_Moss_height, random=~1|Grid, data=data1, na.action = na.omit)
summary(nlme1.6)
r.squaredGLMM(nlme1.14)

####WOT 2:####

#Get data
data2<-read.csv('Week 2.csv', na='-', sep=';')

#Round 1:
nlme2.1<-lme(ALT~MossC, random=~1|Grid, data=data2, na.action = na.omit)
nlme2.2<-lme(ALT~VascC, random=~1|Grid, data=data2, na.action = na.omit)
nlme2.3<-lme(ALT~LichenC, random=~1|Grid, data=data2, na.action = na.omit)
nlme2.4<-lme(ALT~RockC, random=~1|Grid, data=data2, na.action = na.omit)
nlme2.5<-lme(ALT~Avg_Gram_height, random=~1|Grid, data=data2, na.action = na.omit)
nlme2.6<-lme(ALT~Avg_Moss_height, random=~1|Grid, data=data2, na.action = na.omit)
nlme2.7<-lme(ALT~Avg_Vasc_height, random=~1|Grid, data=data2, na.action = na.omit)
nlme2.8<-lme(ALT~cMm, random=~1|Grid, data=data2, na.action = na.omit)
nlme2.9<-lme(ALT~Relative_elevation, random=~1|Grid, data=data2, na.action = na.omit)
nlme2.10<-lme(ALT~cTmeanair, random=~1|Grid, data=data2, na.action = na.omit)
nlme2.11<-lme(ALT~Org_layer, random=~1|Grid, data=data2, na.action = na.omit)
nlme2.12<-lme(ALT~SD_height, random=~1|Grid, data=data2, na.action = na.omit)
nlme2.13<-lme(ALT~DoSM, random=~1|Grid, data=data2, na.action = na.omit)
nlme2.14<-lme(ALT~Max_snow, random=~1|Grid, data=data2, na.action = na.omit)
nlme2.15<-lme(ALT~Aspect, random=~1|Grid, data=data2, na.action = na.omit)
nlme2.16<-lme(ALT~Slope, random=~1|Grid, data=data2, na.action = na.omit)

#Results:
summary(nlme2.1)#BIC=-2.344
summary(nlme2.2)#BIC=-3.991
summary(nlme2.3)#BIC=-7.582
summary(nlme2.4)#BIC=4.580
summary(nlme2.5)#BIC=-1.919
summary(nlme2.6)#BIC=-1.761
summary(nlme2.7)#BIC=-14.441, lowest value
summary(nlme2.8)#BIC=-4.319
summary(nlme2.9)#BIC=-10.460
summary(nlme2.10)#BIC=-6.106
summary(nlme2.11)#BIC=-12.617
summary(nlme2.12)#BIC=-2.953
summary(nlme2.13)#BIC=-1.855
summary(nlme2.14)#BIC=-14.160
summary(nlme2.15)#BIC=-7.254
summary(nlme2.16)#BIC=-7.211

#Round 2:
nlme2.7.1<-lme(ALT~Avg_Vasc_height+MossC, random=~1|Grid, data=data2, na.action = na.omit)
nlme2.7.2<-lme(ALT~Avg_Vasc_height+VascC, random=~1|Grid, data=data2, na.action = na.omit)
nlme2.7.3<-lme(ALT~Avg_Vasc_height+LichenC, random=~1|Grid, data=data2, na.action = na.omit)
nlme2.7.4<-lme(ALT~Avg_Vasc_height+RockC, random=~1|Grid, data=data2, na.action = na.omit)
nlme2.7.5<-lme(ALT~Avg_Vasc_height+Avg_Gram_height, random=~1|Grid, data=data2, na.action = na.omit)
nlme2.7.7<-lme(ALT~Avg_Vasc_height+Avg_Moss_height, random=~1|Grid, data=data2, na.action = na.omit)
nlme2.7.8<-lme(ALT~Avg_Vasc_height+cMm, random=~1|Grid, data=data2, na.action = na.omit)
nlme2.7.9<-lme(ALT~Avg_Vasc_height+Relative_elevation, random=~1|Grid, data=data2, na.action = na.omit)
nlme2.7.10<-lme(ALT~Avg_Vasc_height+cTmeanair, random=~1|Grid, data=data2, na.action = na.omit)
nlme2.7.11<-lme(ALT~Avg_Vasc_height+Org_layer, random=~1|Grid, data=data2, na.action = na.omit)
nlme2.7.12<-lme(ALT~Avg_Vasc_height+SD_height, random=~1|Grid, data=data2, na.action = na.omit)
nlme2.7.13<-lme(ALT~Avg_Vasc_height+DoSM, random=~1|Grid, data=data2, na.action = na.omit)
nlme2.7.14<-lme(ALT~Avg_Vasc_height+Max_snow, random=~1|Grid, data=data2, na.action = na.omit)
nlme2.7.15<-lme(ALT~Avg_Vasc_height+Aspect, random=~1|Grid, data=data2, na.action = na.omit)
nlme2.7.16<-lme(ALT~Avg_Vasc_height+Slope, random=~1|Grid, data=data2, na.action = na.omit)

#Results, threshold BIC -14.441:
summary(nlme2.7.1)#BIC=-7.877
summary(nlme2.7.2)#BIC=-8.286
summary(nlme2.7.3)#BIC=-12.577
summary(nlme2.7.4)#BIC=-2.618
summary(nlme2.7.5)#BIC=-9.013
summary(nlme2.7.7)#BIC=-8.015
summary(nlme2.7.8)#BIC=-12.347
summary(nlme2.7.9)#BIC=-20.650
summary(nlme2.7.10)#BIC=-13.987
summary(nlme2.7.11)#BIC=-12.047
summary(nlme2.7.12)#BIC=-12.974
summary(nlme2.7.13)#BIC=-8.660
summary(nlme2.7.14)#BIC=-26.982, lowest value
summary(nlme2.7.15)#BIC=-17.677
summary(nlme2.7.16)#BIC=-8.926

#Round 3:
nlme2.7.14.1<-lme(ALT~Avg_Vasc_height+Max_snow+MossC, random=~1|Grid, data=data2, na.action = na.omit)
nlme2.7.14.2<-lme(ALT~Avg_Vasc_height+Max_snow+VascC, random=~1|Grid, data=data2, na.action = na.omit)
nlme2.7.14.3<-lme(ALT~Avg_Vasc_height+Max_snow+LichenC, random=~1|Grid, data=data2, na.action = na.omit)
nlme2.7.14.4<-lme(ALT~Avg_Vasc_height+Max_snow+RockC, random=~1|Grid, data=data2, na.action = na.omit)
nlme2.7.14.5<-lme(ALT~Avg_Vasc_height+Max_snow+Avg_Gram_height, random=~1|Grid, data=data2, na.action = na.omit)
nlme2.7.14.7<-lme(ALT~Avg_Vasc_height+Max_snow+Avg_Moss_height, random=~1|Grid, data=data2, na.action = na.omit)
nlme2.7.14.8<-lme(ALT~Avg_Vasc_height+Max_snow+cMm, random=~1|Grid, data=data2, na.action = na.omit)
nlme2.7.14.9<-lme(ALT~Avg_Vasc_height+Max_snow+Relative_elevation, random=~1|Grid, data=data2, na.action = na.omit)
nlme2.7.14.10<-lme(ALT~Avg_Vasc_height+Max_snow+cTmeanair, random=~1|Grid, data=data2, na.action = na.omit)
nlme2.7.14.11<-lme(ALT~Avg_Vasc_height+Max_snow+Org_layer, random=~1|Grid, data=data2, na.action = na.omit)
nlme2.7.14.12<-lme(ALT~Avg_Vasc_height+Max_snow+SD_height, random=~1|Grid, data=data2, na.action = na.omit)
nlme2.7.14.13<-lme(ALT~Avg_Vasc_height+Max_snow+DoSM, random=~1|Grid, data=data2, na.action = na.omit)
nlme2.7.14.14<-lme(ALT~Avg_Vasc_height+Max_snow+Aspect, random=~1|Grid, data=data2, na.action = na.omit)
nlme2.7.14.15<-lme(ALT~Avg_Vasc_height+Max_snow+Slope, random=~1|Grid, data=data2, na.action = na.omit)

#Results, threshold BIC -26.982:
summary(nlme2.7.14.1)#BIC=-20.119
summary(nlme2.7.14.2)#BIC=-19.900
summary(nlme2.7.14.3)#BIC=-21.580
summary(nlme2.7.14.4)#BIC=-13.796
summary(nlme2.7.14.5)#BIC=-23.209
summary(nlme2.7.14.7)#BIC=-22.168
summary(nlme2.7.14.8)#BIC=-20.290
summary(nlme2.7.14.9)#BIC=-21.573
summary(nlme2.7.14.10)#BIC=-21.474
summary(nlme2.7.14.11)#BIC=-22.779
summary(nlme2.7.14.12)#BIC=-24.146
summary(nlme2.7.14.13)#BIC=-21.522
summary(nlme2.7.14.14)#BIC=-22.858
summary(nlme2.7.14.15)#BIC=-22.078

#No improvement, no further selection
#Best model ALT~Moss thickness:
nlme2.7.14<-lme(ALT~Avg_Vasc_height+Max_snow, random=~1|Grid, data=data2, na.action = na.omit)
summary(nlme2.7.14)
r.squaredGLMM(nlme2.7.14)

####WOT 3:####

#Get data
data3<-read.csv('Week 3.csv', na='-', sep=';')

#Round 1:
nlme3.1<-lme(ALT~MossC, random=~1|Grid, data=data3, na.action = na.omit)
nlme3.2<-lme(ALT~VascC, random=~1|Grid, data=data3, na.action = na.omit)
nlme3.3<-lme(ALT~LichenC, random=~1|Grid, data=data3, na.action = na.omit)
nlme3.4<-lme(ALT~RockC, random=~1|Grid, data=data3, na.action = na.omit)
nlme3.5<-lme(ALT~Avg_Gram_height, random=~1|Grid, data=data3, na.action = na.omit)
nlme3.6<-lme(ALT~Avg_Moss_height, random=~1|Grid, data=data3, na.action = na.omit)
nlme3.7<-lme(ALT~Avg_Vasc_height, random=~1|Grid, data=data3, na.action = na.omit)
nlme3.8<-lme(ALT~cMm, random=~1|Grid, data=data3, na.action = na.omit)
nlme3.9<-lme(ALT~Relative_elevation, random=~1|Grid, data=data3, na.action = na.omit)
nlme3.10<-lme(ALT~cTmeanair, random=~1|Grid, data=data3, na.action = na.omit)
nlme3.11<-lme(ALT~Org_layer, random=~1|Grid, data=data3, na.action = na.omit)
nlme3.12<-lme(ALT~SD_height, random=~1|Grid, data=data3, na.action = na.omit)
nlme3.13<-lme(ALT~DoSM, random=~1|Grid, data=data3, na.action = na.omit)
nlme3.14<-lme(ALT~Max_snow, random=~1|Grid, data=data3, na.action = na.omit)
nlme3.15<-lme(ALT~Aspect, random=~1|Grid, data=data3, na.action = na.omit)
nlme3.16<-lme(ALT~Slope, random=~1|Grid, data=data3, na.action = na.omit)

#Results:
summary(nlme3.1)#BIC=3.759
summary(nlme3.2)#BIC=0.804
summary(nlme3.3)#BIC=0.714
summary(nlme3.4)#BIC=10.620
summary(nlme3.5)#BIC=-0.582
summary(nlme3.6)#BIC=-8.908
summary(nlme3.7)#BIC=-4.758
summary(nlme3.8)#BIC=3.052
summary(nlme3.9)#BIC=1.440
summary(nlme3.10)#BIC=-7.894
summary(nlme3.11)#BIC=-0.759
summary(nlme3.12)#BIC=4.046
summary(nlme3.13)#BIC=-11.372, lowest value
summary(nlme3.14)#BIC=-5.190
summary(nlme3.15)#BIC=2.709
summary(nlme3.16)#BIC=1.978

#Round 2:
nlme3.13.1<-lme(ALT~DoSM+MossC, random=~1|Grid, data=data3, na.action = na.omit)
nlme3.13.2<-lme(ALT~DoSM+VascC, random=~1|Grid, data=data3, na.action = na.omit)
nlme3.13.3<-lme(ALT~DoSM+LichenC, random=~1|Grid, data=data3, na.action = na.omit)
nlme3.13.4<-lme(ALT~DoSM+RockC, random=~1|Grid, data=data3, na.action = na.omit)
nlme3.13.5<-lme(ALT~DoSM+Avg_Gram_height, random=~1|Grid, data=data3, na.action = na.omit)
nlme3.13.7<-lme(ALT~DoSM+Avg_Moss_height, random=~1|Grid, data=data3, na.action = na.omit)
nlme3.13.8<-lme(ALT~DoSM+cMm, random=~1|Grid, data=data3, na.action = na.omit)
nlme3.13.9<-lme(ALT~DoSM+Relative_elevation, random=~1|Grid, data=data3, na.action = na.omit)
nlme3.13.10<-lme(ALT~DoSM+cTmeanair, random=~1|Grid, data=data3, na.action = na.omit)
nlme3.13.11<-lme(ALT~DoSM+Org_layer, random=~1|Grid, data=data3, na.action = na.omit)
nlme3.13.12<-lme(ALT~DoSM+SD_height, random=~1|Grid, data=data3, na.action = na.omit)
nlme3.13.13<-lme(ALT~DoSM+Avg_Vasc_height, random=~1|Grid, data=data3, na.action = na.omit)
nlme3.13.14<-lme(ALT~DoSM+Max_snow, random=~1|Grid, data=data3, na.action = na.omit)
nlme3.13.15<-lme(ALT~DoSM+Aspect, random=~1|Grid, data=data3, na.action = na.omit)
nlme3.13.16<-lme(ALT~DoSM+Slope, random=~1|Grid, data=data3, na.action = na.omit)

#Results, threshold BIC -11.372:
summary(nlme3.13.1)#BIC=-17.177
summary(nlme3.13.2)#BIC=-4.530
summary(nlme3.13.3)#BIC=-4.454
summary(nlme3.13.4)#BIC=-0.643
summary(nlme3.13.5)#BIC=-9.201
summary(nlme3.13.7)#BIC=-24.161, lowest value
summary(nlme3.13.8)#BIC=-5.696
summary(nlme3.13.9)#BIC=-6.111
summary(nlme3.13.10)#BIC=-7.179
summary(nlme3.13.11)#BIC=-12.951
summary(nlme3.13.12)#BIC=-5.481
summary(nlme3.13.13)#BIC=-18.350
summary(nlme3.13.14)#BIC=-9.520
summary(nlme3.13.15)#BIC=-5.641
summary(nlme3.13.16)#BIC=-6.966

#Round 3:
nlme3.13.7.1<-lme(ALT~DoSM+Avg_Moss_height+MossC, random=~1|Grid, data=data3, na.action = na.omit)
nlme3.13.7.2<-lme(ALT~DoSM+Avg_Moss_height+VascC, random=~1|Grid, data=data3, na.action = na.omit)
nlme3.13.7.3<-lme(ALT~DoSM+Avg_Moss_height+LichenC, random=~1|Grid, data=data3, na.action = na.omit)
nlme3.13.7.4<-lme(ALT~DoSM+Avg_Moss_height+RockC, random=~1|Grid, data=data3, na.action = na.omit)
nlme3.13.7.5<-lme(ALT~DoSM+Avg_Moss_height+Avg_Gram_height, random=~1|Grid, data=data3, na.action = na.omit)
nlme3.13.7.7<-lme(ALT~DoSM+Avg_Moss_height+Avg_Vasc_height, random=~1|Grid, data=data3, na.action = na.omit)
nlme3.13.7.8<-lme(ALT~DoSM+Avg_Moss_height+cMm, random=~1|Grid, data=data3, na.action = na.omit)
nlme3.13.7.9<-lme(ALT~DoSM+Avg_Moss_height+Relative_elevation, random=~1|Grid, data=data3, na.action = na.omit)
nlme3.13.7.10<-lme(ALT~DoSM+Avg_Moss_height+cTmeanair, random=~1|Grid, data=data3, na.action = na.omit)
nlme3.13.7.11<-lme(ALT~DoSM+Avg_Moss_height+Org_layer, random=~1|Grid, data=data3, na.action = na.omit)
nlme3.13.7.12<-lme(ALT~DoSM+Avg_Moss_height+SD_height, random=~1|Grid, data=data3, na.action = na.omit)
nlme3.13.7.13<-lme(ALT~DoSM+Avg_Moss_height+Max_snow, random=~1|Grid, data=data3, na.action = na.omit)
nlme3.13.7.14<-lme(ALT~DoSM+Avg_Moss_height+Aspect, random=~1|Grid, data=data3, na.action = na.omit)
nlme3.13.7.15<-lme(ALT~DoSM+Avg_Moss_height+Slope, random=~1|Grid, data=data3, na.action = na.omit)

#Results, threshold BIC -24.161:
summary(nlme3.13.7.1)#BIC=-22.167
summary(nlme3.13.7.2)#BIC=-18.321
summary(nlme3.13.7.3)#BIC=-17.723
summary(nlme3.13.7.4)#BIC=-9.923
summary(nlme3.13.7.5)#BIC=-18.878
summary(nlme3.13.7.7)#BIC=-19.899
summary(nlme3.13.7.8)#BIC=-17.448
summary(nlme3.13.7.9)#BIC=-18.734
summary(nlme3.13.7.10)#BIC=-19.320
summary(nlme3.13.7.11)#BIC=-18.815
summary(nlme3.13.7.12)#BIC=-17.768
summary(nlme3.13.7.13)#BIC=-19.661
summary(nlme3.13.7.14)#BIC=-18.236
summary(nlme3.13.7.15)#BIC=-19.607

#No improvement, no further selection
#Best model ALT~Moss thickness:
nlme3.13.7<-lme(ALT~DoSM+Avg_Moss_height, random=~1|Grid, data=data3, na.action = na.omit)
summary(nlme3.13.7)
r.squaredGLMM(nlme3.13.7)

####WOT 4:####

#Get data
data4<-read.csv('Week 4.csv', na='-', sep=';')

#Round 1:
nlme4.1<-lme(ALT~MossC, random=~1|Grid, data=data4, na.action = na.omit)
nlme4.2<-lme(ALT~VascC, random=~1|Grid, data=data4, na.action = na.omit)
nlme4.3<-lme(ALT~LichenC, random=~1|Grid, data=data4, na.action = na.omit)
nlme4.4<-lme(ALT~RockC, random=~1|Grid, data=data4, na.action = na.omit)
nlme4.5<-lme(ALT~Avg_Gram_height, random=~1|Grid, data=data4, na.action = na.omit)
nlme4.6<-lme(ALT~Avg_Moss_height, random=~1|Grid, data=data4, na.action = na.omit)
nlme4.7<-lme(ALT~Avg_Vasc_height, random=~1|Grid, data=data4, na.action = na.omit)
nlme4.8<-lme(ALT~cMm, random=~1|Grid, data=data4, na.action = na.omit)
nlme4.9<-lme(ALT~Relative_elevation, random=~1|Grid, data=data4, na.action = na.omit)
nlme4.10<-lme(ALT~cTmeanair, random=~1|Grid, data=data4, na.action = na.omit)
nlme4.11<-lme(ALT~Org_layer, random=~1|Grid, data=data4, na.action = na.omit)
nlme4.12<-lme(ALT~SD_height, random=~1|Grid, data=data4, na.action = na.omit)
nlme4.13<-lme(ALT~DoSM, random=~1|Grid, data=data4, na.action = na.omit)
nlme4.14<-lme(ALT~Max_snow, random=~1|Grid, data=data4, na.action = na.omit)
nlme4.15<-lme(ALT~Aspect, random=~1|Grid, data=data4, na.action = na.omit)
nlme4.16<-lme(ALT~Slope, random=~1|Grid, data=data4, na.action = na.omit)

#Results:
summary(nlme4.1)#BIC=-9.000
summary(nlme4.2)#BIC=-18.254
summary(nlme4.3)#BIC=-10.886
summary(nlme4.4)#BIC=-4.106
summary(nlme4.5)#BIC=-15.486
summary(nlme4.6)#BIC=-23.012
summary(nlme4.7)#BIC=-16.468
summary(nlme4.8)#BIC=-9.411
summary(nlme4.9)#BIC=-10.856
summary(nlme4.10)#BIC=-19.195
summary(nlme4.11)#BIC=-16.500
summary(nlme4.12)#BIC=-8.595
summary(nlme4.13)#BIC=-23.067, lowest value
summary(nlme4.14)#BIC=-18.237
summary(nlme4.15)#BIC=-9.972
summary(nlme4.16)#BIC=-9.845

#Round 2:
nlme4.13.1<-lme(ALT~DoSM+MossC, random=~1|Grid, data=data4, na.action = na.omit)
nlme4.13.2<-lme(ALT~DoSM+VascC, random=~1|Grid, data=data4, na.action = na.omit)
nlme4.13.3<-lme(ALT~DoSM+LichenC, random=~1|Grid, data=data4, na.action = na.omit)
nlme4.13.4<-lme(ALT~DoSM+RockC, random=~1|Grid, data=data4, na.action = na.omit)
nlme4.13.5<-lme(ALT~DoSM+Avg_Gram_height, random=~1|Grid, data=data4, na.action = na.omit)
nlme4.13.6<-lme(ALT~DoSM+Avg_Moss_height, random=~1|Grid, data=data4, na.action = na.omit)
nlme4.13.7<-lme(ALT~DoSM+cMm, random=~1|Grid, data=data4, na.action = na.omit)
nlme4.13.8<-lme(ALT~DoSM+Relative_elevation, random=~1|Grid, data=data4, na.action = na.omit)
nlme4.13.9<-lme(ALT~DoSM+cTmeanair, random=~1|Grid, data=data4, na.action = na.omit)
nlme4.13.10<-lme(ALT~DoSM+Org_layer, random=~1|Grid, data=data4, na.action = na.omit)
nlme4.13.11<-lme(ALT~DoSM+SD_height, random=~1|Grid, data=data4, na.action = na.omit)
nlme4.13.12<-lme(ALT~DoSM+Avg_Vasc_height, random=~1|Grid, data=data4, na.action = na.omit)
nlme4.13.13<-lme(ALT~DoSM+Max_snow, random=~1|Grid, data=data4, na.action = na.omit)
nlme4.13.14<-lme(ALT~DoSM+Aspect, random=~1|Grid, data=data4, na.action = na.omit)
nlme4.13.15<-lme(ALT~DoSM+Slope, random=~1|Grid, data=data4, na.action = na.omit)

#Results, threshold BIC -11.372:
summary(nlme4.13.1)#BIC=-29.737
summary(nlme4.13.2)#BIC=-19.762
summary(nlme4.13.3)#BIC=-16.342
summary(nlme4.13.4)#BIC=-16.357
summary(nlme4.13.5)#BIC=-22.831
summary(nlme4.13.6)#BIC=-39.489, lowest value
summary(nlme4.13.7)#BIC=-17.477
summary(nlme4.13.8)#BIC=-18.012
summary(nlme4.13.9)#BIC=-17.774
summary(nlme4.13.10)#BIC=-30.477
summary(nlme4.13.11)#BIC=-16.284
summary(nlme4.13.12)#BIC=-30.631
summary(nlme4.13.13)#BIC=-19.771
summary(nlme4.13.14)#BIC=-17.257
summary(nlme4.13.15)#BIC=-17.488

#Round 3:
nlme4.13.7.1<-lme(ALT~DoSM+Avg_Moss_height+MossC, random=~1|Grid, data=data4, na.action = na.omit)
nlme4.13.7.2<-lme(ALT~DoSM+Avg_Moss_height+VascC, random=~1|Grid, data=data4, na.action = na.omit)
nlme4.13.7.3<-lme(ALT~DoSM+Avg_Moss_height+LichenC, random=~1|Grid, data=data4, na.action = na.omit)
nlme4.13.7.4<-lme(ALT~DoSM+Avg_Moss_height+RockC, random=~1|Grid, data=data4, na.action = na.omit)
nlme4.13.7.5<-lme(ALT~DoSM+Avg_Moss_height+Avg_Gram_height, random=~1|Grid, data=data4, na.action = na.omit)
nlme4.13.7.7<-lme(ALT~DoSM+Avg_Moss_height+Avg_Vasc_height, random=~1|Grid, data=data4, na.action = na.omit)
nlme4.13.7.8<-lme(ALT~DoSM+Avg_Moss_height+cMm, random=~1|Grid, data=data4, na.action = na.omit)
nlme4.13.7.9<-lme(ALT~DoSM+Avg_Moss_height+Relative_elevation, random=~1|Grid, data=data4, na.action = na.omit)
nlme4.13.7.10<-lme(ALT~DoSM+Avg_Moss_height+cTmeanair, random=~1|Grid, data=data4, na.action = na.omit)
nlme4.13.7.11<-lme(ALT~DoSM+Avg_Moss_height+Org_layer, random=~1|Grid, data=data4, na.action = na.omit)
nlme4.13.7.12<-lme(ALT~DoSM+Avg_Moss_height+SD_height, random=~1|Grid, data=data4, na.action = na.omit)
nlme4.13.7.13<-lme(ALT~DoSM+Avg_Moss_height+Max_snow, random=~1|Grid, data=data4, na.action = na.omit)
nlme4.13.7.14<-lme(ALT~DoSM+Avg_Moss_height+Aspect, random=~1|Grid, data=data4, na.action = na.omit)
nlme4.13.7.15<-lme(ALT~DoSM+Avg_Moss_height+Slope, random=~1|Grid, data=data4, na.action = na.omit)

#Results, threshold BIC -39.489:
summary(nlme4.13.7.1)#BIC=-37.143
summary(nlme4.13.7.2)#BIC=-32.467
summary(nlme4.13.7.3)#BIC=-32.447
summary(nlme4.13.7.4)#BIC=-26.594
summary(nlme4.13.7.5)#BIC=-35.445
summary(nlme4.13.7.7)#BIC=-34.382
summary(nlme4.13.7.8)#BIC=-32.506
summary(nlme4.13.7.9)#BIC=-34.170
summary(nlme4.13.7.10)#BIC=-33.870
summary(nlme4.13.7.11)#BIC=-36.671
summary(nlme4.13.7.12)#BIC=-32.301
summary(nlme4.13.7.13)#BIC=-34.163
summary(nlme4.13.7.14)#BIC=-33.156
summary(nlme4.13.7.15)#BIC=-33.402

#No improvement, no further selection
#Best model ALT~Moss thickness:
nlme4.13.7<-lme(ALT~DoSM+Avg_Moss_height, random=~1|Grid, data=data4, na.action = na.omit)
summary(nlme4.13.7)
r.squaredGLMM(nlme4.13.7)

####WOT 5:####

#Get data
data5<-read.csv('Week 5.csv', na='-', sep=';')

#Round 1:
nlme5.1<-lme(ALT~MossC, random=~1|Grid, data=data5, na.action = na.omit)
nlme5.2<-lme(ALT~VascC, random=~1|Grid, data=data5, na.action = na.omit)
nlme5.3<-lme(ALT~LichenC, random=~1|Grid, data=data5, na.action = na.omit)
nlme5.4<-lme(ALT~RockC, random=~1|Grid, data=data5, na.action = na.omit)
nlme5.5<-lme(ALT~Avg_Gram_height, random=~1|Grid, data=data5, na.action = na.omit)
nlme5.6<-lme(ALT~Avg_Moss_height, random=~1|Grid, data=data5, na.action = na.omit)
nlme5.7<-lme(ALT~Avg_Vasc_height, random=~1|Grid, data=data5, na.action = na.omit)
nlme5.8<-lme(ALT~cMm, random=~1|Grid, data=data5, na.action = na.omit)
nlme5.9<-lme(ALT~Relative_elevation, random=~1|Grid, data=data5, na.action = na.omit)
nlme5.10<-lme(ALT~cTmeanair, random=~1|Grid, data=data5, na.action = na.omit)
nlme5.11<-lme(ALT~Org_layer, random=~1|Grid, data=data5, na.action = na.omit)
nlme5.12<-lme(ALT~SD_height, random=~1|Grid, data=data5, na.action = na.omit)
nlme5.13<-lme(ALT~DoSM, random=~1|Grid, data=data5, na.action = na.omit)
nlme5.14<-lme(ALT~Max_snow, random=~1|Grid, data=data5, na.action = na.omit)
nlme5.15<-lme(ALT~Aspect, random=~1|Grid, data=data5, na.action = na.omit)
nlme5.16<-lme(ALT~Slope, random=~1|Grid, data=data5, na.action = na.omit)

#Results:
summary(nlme5.1)#BIC=-22.073
summary(nlme5.2)#BIC=-29.578
summary(nlme5.3)#BIC=-22.159
summary(nlme5.4)#BIC=-16.586
summary(nlme5.5)#BIC=-24.848
summary(nlme5.6)#BIC=-34.884, lowest value
summary(nlme5.7)#BIC=-26.700
summary(nlme5.8)#BIC=-22.181
summary(nlme5.9)#BIC=-21.181
summary(nlme5.10)#BIC=-26.567
summary(nlme5.11)#BIC=-27.695
summary(nlme5.12)#BIC=-21.445
summary(nlme5.13)#BIC=-30.332
summary(nlme5.14)#BIC=-27.898
summary(nlme5.15)#BIC=-22.452
summary(nlme5.16)#BIC=-21.804

#Round 2:
nlme5.6.1<-lme(ALT~Avg_Moss_height+MossC, random=~1|Grid, data=data5, na.action = na.omit)
nlme5.6.2<-lme(ALT~Avg_Moss_height+VascC, random=~1|Grid, data=data5, na.action = na.omit)
nlme5.6.3<-lme(ALT~Avg_Moss_height+LichenC, random=~1|Grid, data=data5, na.action = na.omit)
nlme5.6.4<-lme(ALT~Avg_Moss_height+RockC, random=~1|Grid, data=data5, na.action = na.omit)
nlme5.6.5<-lme(ALT~Avg_Moss_height+Avg_Gram_height, random=~1|Grid, data=data5, na.action = na.omit)
nlme5.6.6<-lme(ALT~Avg_Moss_height+Avg_Vasc_height, random=~1|Grid, data=data5, na.action = na.omit)
nlme5.6.7<-lme(ALT~Avg_Moss_height+cMm, random=~1|Grid, data=data5, na.action = na.omit)
nlme5.6.8<-lme(ALT~Avg_Moss_height+Relative_elevation, random=~1|Grid, data=data5, na.action = na.omit)
nlme5.6.9<-lme(ALT~Avg_Moss_height+cTmeanair, random=~1|Grid, data=data5, na.action = na.omit)
nlme5.6.10<-lme(ALT~Avg_Moss_height+Org_layer, random=~1|Grid, data=data5, na.action = na.omit)
nlme5.6.11<-lme(ALT~Avg_Moss_height+SD_height, random=~1|Grid, data=data5, na.action = na.omit)
nlme5.6.12<-lme(ALT~Avg_Moss_height+DoSM, random=~1|Grid, data=data5, na.action = na.omit)
nlme5.6.13<-lme(ALT~Avg_Moss_height+Max_snow, random=~1|Grid, data=data5, na.action = na.omit)
nlme5.6.14<-lme(ALT~Avg_Moss_height+Aspect, random=~1|Grid, data=data5, na.action = na.omit)
nlme5.6.15<-lme(ALT~Avg_Moss_height+Slope, random=~1|Grid, data=data5, na.action = na.omit)

#Results, threshold BIC -34.884:
summary(nlme5.6.1)#BIC=-28.139
summary(nlme5.6.2)#BIC=-33.114
summary(nlme5.6.3)#BIC=-33.124
summary(nlme5.6.4)#BIC=-20.601
summary(nlme5.6.5)#BIC=-29.067
summary(nlme5.6.7)#BIC=-33.012
summary(nlme5.6.8)#BIC=-28.052
summary(nlme5.6.9)#BIC=-43.296
summary(nlme5.6.10)#BIC=-28.875
summary(nlme5.6.11)#BIC=-27.840
summary(nlme5.6.12)#BIC=-45.516, lowest value
summary(nlme5.6.13)#BIC=-45.331
summary(nlme5.6.14)#BIC=-28.298
summary(nlme5.6.15)#BIC=-27.877

#Round 3:
nlme5.6.7.1<-lme(ALT~Avg_Moss_height+DoSM+MossC, random=~1|Grid, data=data5, na.action = na.omit)
nlme5.6.7.2<-lme(ALT~Avg_Moss_height+DoSM+VascC, random=~1|Grid, data=data5, na.action = na.omit)
nlme5.6.7.3<-lme(ALT~Avg_Moss_height+DoSM+LichenC, random=~1|Grid, data=data5, na.action = na.omit)
nlme5.6.7.4<-lme(ALT~Avg_Moss_height+DoSM+RockC, random=~1|Grid, data=data5, na.action = na.omit)
nlme5.6.7.5<-lme(ALT~Avg_Moss_height+DoSM+Avg_Gram_height, random=~1|Grid, data=data5, na.action = na.omit)
nlme5.6.7.6<-lme(ALT~Avg_Moss_height+DoSM+Avg_Vasc_height, random=~1|Grid, data=data5, na.action = na.omit)
nlme5.6.7.7<-lme(ALT~Avg_Moss_height+DoSM+cMm, random=~1|Grid, data=data5, na.action = na.omit)
nlme5.6.7.8<-lme(ALT~Avg_Moss_height+DoSM+Relative_elevation, random=~1|Grid, data=data5, na.action = na.omit)
nlme5.6.7.9<-lme(ALT~Avg_Moss_height+DoSM+cTmeanair, random=~1|Grid, data=data5, na.action = na.omit)
nlme5.6.7.10<-lme(ALT~Avg_Moss_height+DoSM+Org_layer, random=~1|Grid, data=data5, na.action = na.omit)
nlme5.6.7.11<-lme(ALT~Avg_Moss_height+DoSM+SD_height, random=~1|Grid, data=data5, na.action = na.omit)
nlme5.6.7.12<-lme(ALT~Avg_Moss_height+DoSM+Max_snow, random=~1|Grid, data=data5, na.action = na.omit)
nlme5.6.7.13<-lme(ALT~Avg_Moss_height+DoSM+Aspect, random=~1|Grid, data=data5, na.action = na.omit)
nlme5.6.7.14<-lme(ALT~Avg_Moss_height+DoSM+Slope, random=~1|Grid, data=data5, na.action = na.omit)

#Results, threshold BIC -45.516:
summary(nlme5.6.7.1)#BIC=-41.089
summary(nlme5.6.7.2)#BIC=-38.460
summary(nlme5.6.7.3)#BIC=-38.684
summary(nlme5.6.7.4)#BIC=-31.854
summary(nlme5.6.7.5)#BIC=-39.634
summary(nlme5.6.7.6)#BIC=-39.159
summary(nlme5.6.7.7)#BIC=-39.313
summary(nlme5.6.7.8)#BIC=-42.790
summary(nlme5.6.7.9)#BIC=-39.584
summary(nlme5.6.7.10)#BIC=-41.152
summary(nlme5.6.7.11)#BIC=-38.269
summary(nlme5.6.7.12)#BIC=-40.497
summary(nlme5.6.7.13)#BIC=-38.468
summary(nlme5.6.7.14)#BIC=-39.315

#No improvement, no further selection
#Best model ALT~Moss thickness:
nlme5.6.12<-lme(ALT~DoSM+Avg_Moss_height, random=~1|Grid, data=data5, na.action = na.omit)
summary(nlme5.6.12)
r.squaredGLMM(nlme5.6.12)

####WOT 6:####

#Get data
data6<-read.csv('Week 6.csv', na='-', sep=';')

#Round 1:
nlme6.1<-lme(ALT~MossC, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.2<-lme(ALT~VascC, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.3<-lme(ALT~LichenC, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.4<-lme(ALT~RockC, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.5<-lme(ALT~Avg_Gram_height, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.6<-lme(ALT~Avg_Moss_height, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.7<-lme(ALT~Avg_Vasc_height, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.8<-lme(ALT~cMm, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.9<-lme(ALT~Relative_elevation, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.10<-lme(ALT~cTmeanair, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.11<-lme(ALT~Org_layer, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.12<-lme(ALT~SD_height, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.13<-lme(ALT~DoSM, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.14<-lme(ALT~Max_snow, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.15<-lme(ALT~Aspect, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.16<-lme(ALT~Slope, random=~1|Grid, data=data6, na.action = na.omit)

#Results:
summary(nlme6.1)#BIC=-2.580
summary(nlme6.2)#BIC=-11.923
summary(nlme6.3)#BIC=-4.508
summary(nlme6.4)#BIC=2.305
summary(nlme6.5)#BIC=-10.100
summary(nlme6.6)#BIC=-9.770
summary(nlme6.7)#BIC=-9.042
summary(nlme6.8)#BIC=-3.637
summary(nlme6.9)#BIC=-2.575
summary(nlme6.10)#BIC=-8.638
summary(nlme6.11)#BIC=-6.674
summary(nlme6.12)#BIC=-2.949
summary(nlme6.13)#BIC=-12.305, lowest value
summary(nlme6.14)#BIC=-11.475
summary(nlme6.15)#BIC=-3.834
summary(nlme6.16)#BIC=-3.875

#Round 2:
nlme6.13.1<-lme(ALT~DoSM+MossC, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.13.2<-lme(ALT~DoSM+VascC, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.13.3<-lme(ALT~DoSM+LichenC, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.13.4<-lme(ALT~DoSM+RockC, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.13.5<-lme(ALT~DoSM+Avg_Gram_height, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.13.6<-lme(ALT~DoSM+Avg_Vasc_height, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.13.7<-lme(ALT~DoSM+Avg_Moss_height, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.13.8<-lme(ALT~DoSM+cMm, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.13.9<-lme(ALT~DoSM+Relative_elevation, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.13.10<-lme(ALT~DoSM+cTmeanair, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.13.11<-lme(ALT~DoSM+Org_layer, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.13.12<-lme(ALT~DoSM+SD_height, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.13.13<-lme(ALT~DoSM+Max_snow, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.13.14<-lme(ALT~DoSM+Aspect, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.13.15<-lme(ALT~DoSM+Slope, random=~1|Grid, data=data6, na.action = na.omit)

#Results, threshold BIC -12.305:
summary(nlme6.13.1)#BIC=-10.798
summary(nlme6.13.2)#BIC=-9.860
summary(nlme6.13.3)#BIC=-5.492
summary(nlme6.13.4)#BIC=-5.691
summary(nlme6.13.5)#BIC=-12.954
summary(nlme6.13.7)#BIC=-20.282, lowest value
summary(nlme6.13.8)#BIC=-5.874
summary(nlme6.13.9)#BIC=-15.044
summary(nlme6.13.10)#BIC=-8.283
summary(nlme6.13.11)#BIC=-15.101
summary(nlme6.13.12)#BIC=-5.557
summary(nlme6.13.13)#BIC=-7.503
summary(nlme6.13.14)#BIC=-6.471
summary(nlme6.13.15)#BIC=-7.136

#Round 3:
nlme6.13.7.1<-lme(ALT~DoSM+Avg_Moss_height+MossC, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.13.7.2<-lme(ALT~DoSM+Avg_Moss_height+VascC, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.13.7.3<-lme(ALT~DoSM+Avg_Moss_height+LichenC, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.13.7.4<-lme(ALT~DoSM+Avg_Moss_height+RockC, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.13.7.5<-lme(ALT~DoSM+Avg_Moss_height+Avg_Gram_height, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.13.7.6<-lme(ALT~DoSM+Avg_Moss_height+Avg_Vasc_height, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.13.7.7<-lme(ALT~DoSM+Avg_Moss_height+cMm, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.13.7.8<-lme(ALT~DoSM+Avg_Moss_height+Relative_elevation, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.13.7.9<-lme(ALT~DoSM+Avg_Moss_height+cTmeanair, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.13.7.10<-lme(ALT~DoSM+Avg_Moss_height+Org_layer, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.13.7.11<-lme(ALT~DoSM+Avg_Moss_height+SD_height, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.13.7.12<-lme(ALT~DoSM+Avg_Moss_height+Max_snow, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.13.7.13<-lme(ALT~DoSM+Avg_Moss_height+Aspect, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.13.7.14<-lme(ALT~DoSM+Avg_Moss_height+Slope, random=~1|Grid, data=data6, na.action = na.omit)

#Results, threshold BIC -20.282:
summary(nlme6.13.7.1)#BIC=-14.505
summary(nlme6.13.7.2)#BIC=-14.311
summary(nlme6.13.7.3)#BIC=-14.275
summary(nlme6.13.7.4)#BIC=-8.961
summary(nlme6.13.7.5)#BIC=-16.974
summary(nlme6.13.7.6)#BIC=-16.119
summary(nlme6.13.7.7)#BIC=-13.846
summary(nlme6.13.7.8)#BIC=-21.843, lowest value
summary(nlme6.13.7.9)#BIC=-14.951
summary(nlme6.13.7.10)#BIC=-16.957
summary(nlme6.13.7.11)#BIC=-13.341
summary(nlme6.13.7.12)#BIC=-16.239
summary(nlme6.13.7.13)#BIC=-14.047
summary(nlme6.13.7.14)#BIC=-14.908

#Round 4:
nlme6.13.7.8.1<-lme(ALT~DoSM+Avg_Moss_height+Relative_elevation+MossC, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.13.7.8.2<-lme(ALT~DoSM+Avg_Moss_height+Relative_elevation+VascC, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.13.7.8.3<-lme(ALT~DoSM+Avg_Moss_height+Relative_elevation+LichenC, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.13.7.8.4<-lme(ALT~DoSM+Avg_Moss_height+Relative_elevation+RockC, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.13.7.8.5<-lme(ALT~DoSM+Avg_Moss_height+Relative_elevation+Avg_Gram_height, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.13.7.8.6<-lme(ALT~DoSM+Avg_Moss_height+Relative_elevation+Avg_Vasc_height, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.13.7.8.7<-lme(ALT~DoSM+Avg_Moss_height+Relative_elevation+cMm, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.13.7.8.8<-lme(ALT~DoSM+Avg_Moss_height+Relative_elevation+cTmeanair, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.13.7.8.9<-lme(ALT~DoSM+Avg_Moss_height+Relative_elevation+Org_layer, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.13.7.8.10<-lme(ALT~DoSM+Avg_Moss_height+Relative_elevation+SD_height, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.13.7.8.11<-lme(ALT~DoSM+Avg_Moss_height+Relative_elevation+Max_snow, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.13.7.8.12<-lme(ALT~DoSM+Avg_Moss_height+Relative_elevation+Aspect, random=~1|Grid, data=data6, na.action = na.omit)
nlme6.13.7.8.13<-lme(ALT~DoSM+Avg_Moss_height+Relative_elevation+Slope, random=~1|Grid, data=data6, na.action = na.omit)

#Results, threshold BIC -21.843:
summary(nlme6.13.7.8.1)#BIC=-15.127
summary(nlme6.13.7.8.2)#BIC=-16.036
summary(nlme6.13.7.8.3)#BIC=-17.115
summary(nlme6.13.7.8.4)#BIC=-9.983
summary(nlme6.13.7.8.5)#BIC=-18.120
summary(nlme6.13.7.8.6)#BIC=-19.955
summary(nlme6.13.7.8.7)#BIC=-16.923
summary(nlme6.13.7.8.8)#BIC=-16.416
summary(nlme6.13.7.8.9)#BIC=-17.823
summary(nlme6.13.7.8.10)#BIC=-15.461
summary(nlme6.13.7.8.11)#BIC=-18.646
summary(nlme6.13.7.8.12)#BIC=-15.776
summary(nlme6.13.7.8.13)#BIC=-21.252


#No improvement, no further selection
#Best model ALT~Moss thickness:
nlme6.13.7.8<-lme(ALT~DoSM+Avg_Moss_height+Relative_elevation, random=~1|Grid, data=data6, na.action = na.omit)
summary(nlme6.13.7.8)
r.squaredGLMM(nlme6.13.7.8)

####WOT 7:####

#Get data
data7<-read.csv('Week 7.csv', na='-', sep=';')

#Round 1:
nlme7.1<-lme(ALT~MossC, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.2<-lme(ALT~VascC, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.3<-lme(ALT~LichenC, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.4<-lme(ALT~RockC, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.5<-lme(ALT~Avg_Gram_height, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.6<-lme(ALT~Avg_Moss_height, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.7<-lme(ALT~Avg_Vasc_height, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.8<-lme(ALT~cMm, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.9<-lme(ALT~Relative_elevation, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.10<-lme(ALT~cTmeanair, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.11<-lme(ALT~Org_layer, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.12<-lme(ALT~SD_height, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.13<-lme(ALT~DoSM, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.14<-lme(ALT~Max_snow, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.15<-lme(ALT~Aspect, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.16<-lme(ALT~Slope, random=~1|Grid, data=data7, na.action = na.omit)

#Results:
summary(nlme7.1)#BIC=-4.132
summary(nlme7.2)#BIC=-11.852
summary(nlme7.3)#BIC=-3.667
summary(nlme7.4)#BIC=1.814
summary(nlme7.5)#BIC=-11.961
summary(nlme7.6)#BIC=-13.649, lowest value
summary(nlme7.7)#BIC=-12.499
summary(nlme7.8)#BIC=-4.198
summary(nlme7.9)#BIC=-4.254
summary(nlme7.10)#BIC=-5.574
summary(nlme7.11)#BIC=-7.908
summary(nlme7.12)#BIC=-4.158
summary(nlme7.13)#BIC=-7.003
summary(nlme7.14)#BIC=-7.337
summary(nlme7.15)#BIC=-4.569
summary(nlme7.16)#BIC=-3.854

#Round 2:
nlme7.6.1<-lme(ALT~Avg_Moss_height+MossC, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.6.2<-lme(ALT~Avg_Moss_height+VascC, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.6.3<-lme(ALT~Avg_Moss_height+LichenC, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.6.4<-lme(ALT~Avg_Moss_height+RockC, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.6.5<-lme(ALT~Avg_Moss_height+Avg_Gram_height, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.6.6<-lme(ALT~Avg_Moss_height+Avg_Vasc_height, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.6.7<-lme(ALT~Avg_Moss_height+cMm, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.6.8<-lme(ALT~Avg_Moss_height+Relative_elevation, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.6.9<-lme(ALT~Avg_Moss_height+cTmeanair, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.6.10<-lme(ALT~Avg_Moss_height+Org_layer, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.6.11<-lme(ALT~Avg_Moss_height+SD_height, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.6.12<-lme(ALT~Avg_Moss_height+DoSM, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.6.13<-lme(ALT~Avg_Moss_height+Max_snow, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.6.14<-lme(ALT~Avg_Moss_height+Aspect, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.6.15<-lme(ALT~Avg_Moss_height+Slope, random=~1|Grid, data=data7, na.action = na.omit)

#Results, threshold BIC -13.649:
summary(nlme7.6.1)#BIC=-7.335
summary(nlme7.6.2)#BIC=-12.493
summary(nlme7.6.3)#BIC=-9.690
summary(nlme7.6.4)#BIC=0.420
summary(nlme7.6.5)#BIC=-12.417
summary(nlme7.6.7)#BIC=-12.924
summary(nlme7.6.8)#BIC=-6.632
summary(nlme7.6.9)#BIC=-15.599
summary(nlme7.6.10)#BIC=-7.640
summary(nlme7.6.11)#BIC=-8.123
summary(nlme7.6.12)#BIC=-15.963
summary(nlme7.6.13)#BIC=-18.920, lowest value
summary(nlme7.6.14)#BIC=-7.918
summary(nlme7.6.15)#BIC=-7.004

#Round 3:
nlme7.6.7.1<-lme(ALT~Avg_Moss_height+Max_snow+MossC, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.6.7.2<-lme(ALT~Avg_Moss_height+Max_snow+VascC, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.6.7.3<-lme(ALT~Avg_Moss_height+Max_snow+LichenC, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.6.7.4<-lme(ALT~Avg_Moss_height+Max_snow+RockC, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.6.7.5<-lme(ALT~Avg_Moss_height+Max_snow+Avg_Gram_height, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.6.7.6<-lme(ALT~Avg_Moss_height+Max_snow+Avg_Vasc_height, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.6.7.7<-lme(ALT~Avg_Moss_height+Max_snow+cMm, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.6.7.8<-lme(ALT~Avg_Moss_height+Max_snow+Relative_elevation, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.6.7.9<-lme(ALT~Avg_Moss_height+Max_snow+cTmeanair, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.6.7.10<-lme(ALT~Avg_Moss_height+Max_snow+Org_layer, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.6.7.11<-lme(ALT~Avg_Moss_height+Max_snow+SD_height, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.6.7.12<-lme(ALT~Avg_Moss_height+Max_snow+DoSM, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.6.7.13<-lme(ALT~Avg_Moss_height+Max_snow+Aspect, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.6.7.14<-lme(ALT~Avg_Moss_height+Max_snow+Slope, random=~1|Grid, data=data7, na.action = na.omit)

#Results, threshold BIC -18.920:
summary(nlme7.6.7.1)#BIC=-14.111
summary(nlme7.6.7.2)#BIC=-13.013
summary(nlme7.6.7.3)#BIC=-12.151
summary(nlme7.6.7.4)#BIC=-5.731
summary(nlme7.6.7.5)#BIC=-15.520
summary(nlme7.6.7.6)#BIC=-14.195
summary(nlme7.6.7.7)#BIC=-13.685
summary(nlme7.6.7.8)#BIC=-20.900, lowest value
summary(nlme7.6.7.9)#BIC=-13.069
summary(nlme7.6.7.10)#BIC=-14.892
summary(nlme7.6.7.11)#BIC=-12.176
summary(nlme7.6.7.12)#BIC=-13.361
summary(nlme7.6.7.13)#BIC=-12.649
summary(nlme7.6.7.14)#BIC=-12.318

#Round 4:
nlme7.6.7.8.1<-lme(ALT~Avg_Moss_height+Max_snow+Relative_elevation+MossC, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.6.7.8.2<-lme(ALT~Avg_Moss_height+Max_snow+Relative_elevation+VascC, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.6.7.8.3<-lme(ALT~Avg_Moss_height+Max_snow+Relative_elevation+LichenC, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.6.7.8.4<-lme(ALT~Avg_Moss_height+Max_snow+Relative_elevation+RockC, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.6.7.8.5<-lme(ALT~Avg_Moss_height+Max_snow+Relative_elevation+Avg_Gram_height, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.6.7.8.6<-lme(ALT~Avg_Moss_height+Max_snow+Relative_elevation+Avg_Vasc_height, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.6.7.8.7<-lme(ALT~Avg_Moss_height+Max_snow+Relative_elevation+cMm, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.6.7.8.8<-lme(ALT~Avg_Moss_height+Max_snow+Relative_elevation+cTmeanair, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.6.7.8.9<-lme(ALT~Avg_Moss_height+Max_snow+Relative_elevation+Org_layer, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.6.7.8.10<-lme(ALT~Avg_Moss_height+Max_snow+Relative_elevation+SD_height, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.6.7.8.11<-lme(ALT~Avg_Moss_height+Max_snow+Relative_elevation+DoSM, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.6.7.8.12<-lme(ALT~Avg_Moss_height+Max_snow+Relative_elevation+Aspect, random=~1|Grid, data=data7, na.action = na.omit)
nlme7.6.7.8.13<-lme(ALT~Avg_Moss_height+Max_snow+Relative_elevation+Slope, random=~1|Grid, data=data7, na.action = na.omit)

#Results, threshold BIC -20.900:
summary(nlme7.6.7.8.1)#BIC=-14.334
summary(nlme7.6.7.8.2)#BIC=-15.267
summary(nlme7.6.7.8.3)#BIC=-14.893
summary(nlme7.6.7.8.4)#BIC=-7.460
summary(nlme7.6.7.8.5)#BIC=-17.467
summary(nlme7.6.7.8.6)#BIC=-17.996
summary(nlme7.6.7.8.7)#BIC=-19.786
summary(nlme7.6.7.8.8)#BIC=-14.966
summary(nlme7.6.7.8.9)#BIC=-16.132
summary(nlme7.6.7.8.10)#BIC=-14.020
summary(nlme7.6.7.8.11)#BIC=-15.189
summary(nlme7.6.7.8.12)#BIC=-14.393
summary(nlme7.6.7.8.13)#BIC=-19.377

#No improvement, no further selection
#Best model ALT~Moss thickness:
nlme7.6.7.8<-lme(ALT~Avg_Moss_height+Max_snow+Relative_elevation, random=~1|Grid, data=data7, na.action = na.omit)
summary(nlme7.6.7.8)
r.squaredGLMM(nlme7.6.7.8)

####WOT 8:####

#Get data
data8<-read.csv('Week 8.csv', na='-', sep=';')

#Round 1:
nlme8.1<-lme(ALT~MossC, random=~1|Grid, data=data8, na.action = na.omit)
nlme8.2<-lme(ALT~VascC, random=~1|Grid, data=data8, na.action = na.omit)
nlme8.3<-lme(ALT~LichenC, random=~1|Grid, data=data8, na.action = na.omit)
nlme8.4<-lme(ALT~RockC, random=~1|Grid, data=data8, na.action = na.omit)
nlme8.5<-lme(ALT~Avg_Gram_height, random=~1|Grid, data=data8, na.action = na.omit)
nlme8.6<-lme(ALT~Avg_Moss_height, random=~1|Grid, data=data8, na.action = na.omit)
nlme8.7<-lme(ALT~Avg_Vasc_height, random=~1|Grid, data=data8, na.action = na.omit)
nlme8.8<-lme(ALT~cMm, random=~1|Grid, data=data8, na.action = na.omit)
nlme8.9<-lme(ALT~Relative_elevation, random=~1|Grid, data=data8, na.action = na.omit)
nlme8.10<-lme(ALT~cTmeanair, random=~1|Grid, data=data8, na.action = na.omit)
nlme8.11<-lme(ALT~Org_layer, random=~1|Grid, data=data8, na.action = na.omit)
nlme8.12<-lme(ALT~SD_height, random=~1|Grid, data=data8, na.action = na.omit)
nlme8.13<-lme(ALT~DoSM, random=~1|Grid, data=data8, na.action = na.omit)
nlme8.14<-lme(ALT~Max_snow, random=~1|Grid, data=data8, na.action = na.omit)
nlme8.15<-lme(ALT~Aspect, random=~1|Grid, data=data8, na.action = na.omit)
nlme8.16<-lme(ALT~Slope, random=~1|Grid, data=data8, na.action = na.omit)

#Results:
summary(nlme8.1)#BIC=-2.813
summary(nlme8.2)#BIC=-5.516
summary(nlme8.3)#BIC=-3.939
summary(nlme8.4)#BIC=4.744
summary(nlme8.5)#BIC=-5.704, lowest value
summary(nlme8.6)#BIC=-3.854
summary(nlme8.7)#BIC=-4.007
summary(nlme8.8)#BIC=-4.275
summary(nlme8.9)#BIC=-4.057
summary(nlme8.10)#BIC=-2.869
summary(nlme8.11)#BIC=-2.490
summary(nlme8.12)#BIC=-3.251
summary(nlme8.13)#BIC=-3.697
summary(nlme8.14)#BIC=-3.807
summary(nlme8.15)#BIC=-1.966
summary(nlme8.16)#BIC=-2.560

#Round 2:
nlme8.5.1<-lme(ALT~Avg_Gram_height+MossC, random=~1|Grid, data=data8, na.action = na.omit)
nlme8.5.2<-lme(ALT~Avg_Gram_height+VascC, random=~1|Grid, data=data8, na.action = na.omit)
nlme8.5.3<-lme(ALT~Avg_Gram_height+LichenC, random=~1|Grid, data=data8, na.action = na.omit)
nlme8.5.4<-lme(ALT~Avg_Gram_height+RockC, random=~1|Grid, data=data8, na.action = na.omit)
nlme8.5.5<-lme(ALT~Avg_Gram_height+Avg_Moss_height, random=~1|Grid, data=data8, na.action = na.omit)
nlme8.5.6<-lme(ALT~Avg_Gram_height+Avg_Vasc_height, random=~1|Grid, data=data8, na.action = na.omit)
nlme8.5.7<-lme(ALT~Avg_Gram_height+cMm, random=~1|Grid, data=data8, na.action = na.omit)
nlme8.5.8<-lme(ALT~Avg_Gram_height+Relative_elevation, random=~1|Grid, data=data8, na.action = na.omit)
nlme8.5.9<-lme(ALT~Avg_Gram_height+cTmeanair, random=~1|Grid, data=data8, na.action = na.omit)
nlme8.5.10<-lme(ALT~Avg_Gram_height+Org_layer, random=~1|Grid, data=data8, na.action = na.omit)
nlme8.5.11<-lme(ALT~Avg_Gram_height+SD_height, random=~1|Grid, data=data8, na.action = na.omit)
nlme8.5.12<-lme(ALT~Avg_Gram_height+DoSM, random=~1|Grid, data=data8, na.action = na.omit)
nlme8.5.13<-lme(ALT~Avg_Gram_height+Max_snow, random=~1|Grid, data=data8, na.action = na.omit)
nlme8.5.14<-lme(ALT~Avg_Gram_height+Aspect, random=~1|Grid, data=data8, na.action = na.omit)
nlme8.5.15<-lme(ALT~Avg_Gram_height+Slope, random=~1|Grid, data=data8, na.action = na.omit)

#Results, threshold BIC -5.704:
summary(nlme8.5.1)#BIC=0.794
summary(nlme8.5.2)#BIC=-1.398
summary(nlme8.5.3)#BIC=-1.552
summary(nlme8.5.4)#BIC=7.946
summary(nlme8.5.5)#BIC=-0.283
summary(nlme8.5.7)#BIC=-1.362
summary(nlme8.5.8)#BIC=-0.422
summary(nlme8.5.9)#BIC=0.132
summary(nlme8.5.10)#BIC=0.546
summary(nlme8.5.11)#BIC=-0.097
summary(nlme8.5.12)#BIC=-0.654
summary(nlme8.5.13)#BIC=-1.125
summary(nlme8.5.14)#BIC=-0.294
summary(nlme8.5.15)#BIC=0.356

#No improvement, no further selection
#Best model ALT~Moss thickness:
nlme8.5<-lme(ALT~Avg_Gram_height, random=~1|Grid, data=data8, na.action = na.omit)
summary(nlme8.5)
r.squaredGLMM(nlme8.5)


####Summary of best models per week:####
#WOT1
nlme1.6<-lme(ALT~Avg_Moss_height, random=~1|Grid, data=data1, na.action = na.omit)
summary(nlme1.6)
r.squaredGLMM(nlme1.6)
#WOT2
nlme2.7.14<-lme(ALT~Avg_Vasc_height+Max_snow, random=~1|Grid, data=data2, na.action = na.omit)
summary(nlme2.7.14)
r.squaredGLMM(nlme2.7.14)
#WOT3
nlme3.13.7<-lme(ALT~DoSM+Avg_Moss_height, random=~1|Grid, data=data3, na.action = na.omit)
summary(nlme3.13.7)
r.squaredGLMM(nlme3.13.7)
#WOT4
nlme4.13.7<-lme(ALT~DoSM+Avg_Moss_height, random=~1|Grid, data=data4, na.action = na.omit)
summary(nlme4.13.7)
r.squaredGLMM(nlme4.13.7)
#WOT5
nlme5.6.12<-lme(ALT~DoSM+Avg_Moss_height, random=~1|Grid, data=data5, na.action = na.omit)
summary(nlme5.6.12)
r.squaredGLMM(nlme5.6.12)
#WOT6
nlme6.13.7.8<-lme(ALT~DoSM+Avg_Moss_height+Relative_elevation, random=~1|Grid, data=data6, na.action = na.omit)
summary(nlme6.13.7.8)
r.squaredGLMM(nlme6.13.7.8)
#WOT7
nlme7.6.7.8<-lme(ALT~Avg_Moss_height+Max_snow+Relative_elevation, random=~1|Grid, data=data7, na.action = na.omit)
summary(nlme7.6.7.8)
r.squaredGLMM(nlme7.6.7.8)
#WOT8
nlme8.5<-lme(ALT~Avg_Gram_height, random=~1|Grid, data=data8, na.action = na.omit)
summary(nlme8.5)
r.squaredGLMM(nlme8.5)