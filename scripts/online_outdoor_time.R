library(dplyr)
library(ggthemes)
library(cowplot)

### set colors
bar_color = c('#DBD3C3','#86AAB8')
errorbar_color = c('#898071','#2D545F')




## d1: SE(Dec_2019 - Jun_2019)
## d2: SE(Jun_2020 - Dec_2019)

#------------------------------------------------------------------------#

#### filtering data
questionnaire = questionnaire[which(questionnaire$d1>-5),]
questionnaire = questionnaire[which(questionnaire$d1<0.5),]
questionnaire = questionnaire[which(questionnaire$d2>-5),]
questionnaire = questionnaire[which(questionnaire$d2<0.5),]


###########################################################################
############################# outdoor time more than  one hour
c1 = c()
c1 = c(c1, length(which(questionnaire_low$outdoorTime_norm >1))/nrow(questionnaire_low))
c1 = c(c1, length(which(questionnaire_low$outdoorTime_covid >1))/nrow(questionnaire_low))
c1 = c(c1, length(which(questionnaire_high$outdoorTime_norm >1))/nrow(questionnaire_high))
c1 = c(c1, length(which(questionnaire_high$outdoorTime_covid >1))/nrow(questionnaire_high))
d1 = data.frame(percent = c1, 
                grade = factor(c('low','low','high','high'),levels = c('low','high')), 
                n = c(nrow(questionnaire_low),nrow(questionnaire_low),
                      nrow(questionnaire_high),nrow(questionnaire_high)),
                period = factor(c('Normal','COVID-19','Normal','COVID-19'),levels = c('Normal','COVID-19')))
d1$se = CI(d1$percent,d1$n)
p1 = ggplot(d1)+
  geom_bar(aes(x=grade,y=percent,fill = period),
           position = position_dodge(width=0.7),width = 0.58,stat="identity",alpha=1)+
  geom_errorbar(mapping = aes(x=grade,y=percent,color = period,
                              ymin=percent-se,ymax=percent+se),
                position = position_dodge(width=0.7),
                width = 0.2,size=1,
                stat="identity" )+
  #geom_text(aes(x=grade,y=percent,fill = period,
  #              label = round(d1$percent*100, 1)),position=position_dodge(width = 0.7),size = 10,vjust = -1,hjust = 0.5)+
  scale_fill_manual(values = bar_color)+
  scale_discrete_manual(values = errorbar_color,aesthetics = c("colour"))+
  scale_x_discrete(breaks=c('low','high'),labels=c('Grade Stage I','Grade Stage II'))+
  theme_linedraw(base_size=10)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(colour = 'gray'),
        panel.border = element_blank(),
        axis.line.x = element_line(),
        axis.line.y = element_line(),
        legend.position = 'right',
        legend.title = element_blank(),
        legend.key.size = unit(10,'pt'),
        legend.margin = margin(0,5,5,5),
        legend.text = element_text(size=10),
        axis.text = element_text(size=10),
        legend.background = element_rect(color = 'gray'))+
  scale_y_continuous(breaks=seq(0,0.5,0.1),labels=seq(0,0.5,0.1)*100)+
  ylab('Percent (%)')

######################################################################
### online time more than two hours
c2=c()
c2=c(c2, length(which(questionnaire_low$onlineTime_norm_value > 2))/nrow(questionnaire_low))
c2=c(c2, length(which(questionnaire_low$onlineTime_covid_add > 2))/nrow(questionnaire_low))
c2=c(c2, length(which(questionnaire_high$onlineTime_norm_value > 2))/nrow(questionnaire_high))
c2=c(c2, length(which(questionnaire_high$onlineTime_covid_add > 2))/nrow(questionnaire_high))
d2=data.frame(percent = c2, 
              grade = factor(c('low','low','high','high'),levels = c('low','high')), 
              n = c(nrow(questionnaire_low),nrow(questionnaire_low),
                    nrow(questionnaire_high),nrow(questionnaire_high)),
              period = factor(c('Normal','COVID-19','Normal','COVID-19'),levels = c('Normal','COVID-19')))
d2$se = CI(d2$percent,d2$n)
p2 = ggplot(d2)+
  geom_bar(mapping =  aes(x=grade,y=percent,fill = period),
           position = position_dodge(width=0.7),width = 0.58,stat="identity",alpha=1)+
  geom_errorbar(mapping = aes(x=grade,y=percent,color = period,
                              ymin=percent-se,ymax=percent+se),
                position = position_dodge(width=0.7),
                width = 0.2,size=1,
                stat="identity" )+
  #geom_text(aes(x=grade,y=percent,fill = period,
  #              label = round(percent*100, 1)),position=position_dodge(width = 0.7),size = 5,vjust = -1,hjust = 0.5)+
  scale_fill_manual(values = bar_color)+
  scale_discrete_manual(values = errorbar_color,aesthetics = c("colour"))+
  scale_x_discrete(breaks=c('low','high'),labels=c('Grade Stage I','Grade Stage II'))+
  theme_linedraw(base_size=10)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(colour = 'gray'),
        panel.border = element_blank(),
        axis.line.x = element_line(),
        axis.line.y = element_line(),
        legend.position = 'right',
        legend.title = element_blank(),
        legend.key.size = unit(10,'pt'),
        legend.margin = margin(0,5,5,5),
        legend.text = element_text(size=10),
        axis.text = element_text(size=10),
        legend.background = element_rect(color = 'gray'))+
  scale_y_continuous(breaks=seq(0,1,0.2),labels=seq(0,1,0.2)*100)+
  ylab('Percent (%)')
