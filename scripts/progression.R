



#################################################################
######################## progression  ###########################
#################################################################

### Birth Day:  |___________September_________February
###             |               |               |
### Pre-COVID:  O---------------I---------------O
###             |               |               |
### Post-COVID: |               O---------------I---------------O                     



## d1: SE(Dec_2019 - Jun_2019)
## d2: SE(Jun_2020 - Dec_2019)

#------------------------------------------------------------------------#

###### Pre-COVID progression
normalProgression_new = lapply(1:11, function(x){
  grade_data = comb[which(comb$grade == x),]
  mean(grade_data[which(grade_data$month %in% c(2,1,12,11,10,9)),]$d1)
})

###### CI width of Pre-COVID progression
normalProgression_sd_new = lapply(1:11, function(x){
  grade_data = comb[which(comb$grade == x),]
  grade_data = grade_data[which(grade_data$month %in% c(2,1,12,11,10,9)),]
  numberCI(sd(grade_data$d1), nrow(grade_data))
})



###### Post-COVID progression
covidProgression_new = lapply(1:11, function(x){
  grade_data = comb[which(comb$grade == x),]
  mean(grade_data[which(grade_data$month %in% c(8,7,6,5,4,3)),]$d2)
})

###### CI width of Post-COVID progression
covidProgression_sd_new = lapply(1:11, function(x){
  grade_data = comb[which(comb$grade == x),]
  grade_data = grade_data[which(grade_data$month %in% c(8,7,6,5,4,3)),]
  numberCI(sd(grade_data$d2), nrow(grade_data))
})

##### prepare dataset for plot
data_p3_new = data.frame(progression = c(as.numeric(normalProgression_new),
                                         as.numeric(covidProgression_new)),
                         grade = c(1:11,1:11),
                         ciWidth = c(as.numeric(normalProgression_sd_new)),
                         as.numeric(covidProgression_sd_new),
                         period = c(rep('Normal',11),c(rep('COVID-19',11))))

##### progression plot with error bar
p3_new = ggplot(data_p3_new, aes(x=grade,y=progression,color=period))+
  geom_errorbar(aes(ymin=progression-ciWidth,ymax=progression+ciWidth) ,size=0.6,width=0.2)+
  geom_point(size=1.3)+
  geom_line()+
  scale_discrete_manual(values = c(errorbar_color[2],errorbar_color[1]),
                        aesthetics = c("colour"))+
  theme_linedraw(base_size=10)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(colour = 'gray'),
        panel.border = element_blank(),
        axis.line  = element_line(),
        legend.position = 'right',
        legend.key.size = unit(10,'pt'),
        legend.title = element_blank(),
        legend.margin = margin(0,5,5,5),
        legend.text = element_text(size=10),
        axis.text = element_text(size=10),
        legend.background = element_rect(color = 'gray'))+
  scale_x_continuous(breaks=1:11,labels=1:11)+
  scale_y_continuous(limits = c(-0.55,0),
                     breaks=rev(seq(-0.5,0,0.1)),
                     labels=rev(seq(-0.5,0,0.1)))+
  ylab('Change in spherical equivalent (D)')+
  xlab('Grade')



### Overall progression
covid_progression = mean(data_p3_new[which(data_p3_new$period == 'COVID-19'),'progression'])
normal_progression = mean(data_p3_new[which(data_p3_new$period == 'Normal'),'progression'])
