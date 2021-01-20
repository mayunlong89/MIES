
library(ggplot)
source('./utils.R')


getIncidence = function(data, period='normal', myopia=T){
  if(period == 'normal'){
    if(myopia){
      data = data[which(data$myopia_1 == 0),]
      return(sum(data$myopia_2 == 1)/nrow(data))
    }else{
      data = data[which(data$high_1 == 0),]
      return(sum(data$high_2 == 1)/nrow(data))
    }
  }else{
    if(myopia){
      data = data[which(data$myopia_2 == 0),]
      return(sum(data$myopia_3 == 1)/nrow(data))
    }else{
      data = data[which(data$high_2 == 0),]
      return(sum(data$high_3 == 1)/nrow(data))
    }
  }
}




#################################################################
###################### incidence rate  ##########################
#################################################################

### Birth Day:  |___________September_________February
###             |               |               |
### Pre-COVID:  O---------------I---------------O
###             |               |               |
### Post-COVID: |               O---------------I---------------O                     



## d1: SE(Dec_2019 - Jun_2019)
## d2: SE(Jun_2020 - Dec_2019)

#------------------------------------------------------------------------#

######## population separation
comb_normal = comb[which(comb$birth_month %in% c(2,1,12,11,10,9)),]
comb_covid = comb[which(comb$birth_month %in% c(8,7,6,5,4,3)),]


##################################################################################
########################### Pre-COVID ############################################
##################################################################################
comb_test = comb_normal

##### Gender
male_normal = comb_test[which(comb_test$sex == 1 & comb_test$myopia_1 == 0),]
mal_normal_incidence = sum(male_normal$myopia_2)/nrow(male_normal)
female_normal = comb_test[which(comb_test$sex == 0 & comb_test$myopia_1 == 0),]
femal_normal_incidence = sum(female_normal$myopia_2)/nrow(female_normal)

##### Educational Level
group1_normal = comb_test[which(comb_test$grade %in% 1:6 & comb_test$myopia_1 == 0),]
group1_normal_incidence = sum(group1_normal$myopia_2)/nrow(group1_normal)
group2_normal = comb_test[which(comb_test$grade %in% 7:11 & comb_test$myopia_1 == 0),]
group2_normal_incidence = sum(group2_normal$myopia_2)/nrow(group2_normal)

##### Educational System
nokey_normal = comb_test[which(comb_test$school_key == 0 & comb_test$myopia_1 == 0),]
nokey_normal_incidence = sum(nokey_normal$myopia_2)/nrow(nokey_normal)
key_normal = comb_test[which(comb_test$school_key == 1 & comb_test$myopia_1 == 0),]
key_normal_incidence = sum(key_normal$myopia_2)/nrow(key_normal)

##### Region of habitation
rural_normal = comb_test[which(comb_test$school_town == 0 & comb_test$myopia_1 == 0),]
rural_normal_incidence = sum(rural_normal$myopia_2)/nrow(rural_normal)
urban_normal = comb_test[which(comb_test$school_town == 1 & comb_test$myopia_1 == 0),]
urban_normal_incidence = sum(urban_normal$myopia_2)/nrow(urban_normal)

##### Total
total_normal = comb_test[which(comb_test$myopia_1 == 0),]
total_normal_incidence = sum(total_normal$myopia_2)/nrow(total_normal)


##################################################################################
########################### Post-COVID ###########################################
##################################################################################

comb_test = comb

##### Gender
male_covid = comb_test[which(comb_test$sex == 1 & comb_test$myopia_2 == 0),]
mal_covid_incidence = sum(male_covid$myopia_3)/nrow(male_covid)
female_covid = comb_test[which(comb_test$sex == 0 & comb_test$myopia_2 == 0),]
femal_covid_incidence = sum(female_covid$myopia_3)/nrow(female_covid)

##### Educational Level
group1_covid = comb_test[which(comb_test$grade %in% 1:6 & comb_test$myopia_2 == 0),]
group1_covid_incidence = sum(group1_covid$myopia_3)/nrow(group1_covid)
group2_covid = comb_test[which(comb_test$grade %in% 7:11 & comb_test$myopia_2 == 0),]
group2_covid_incidence = sum(group2_covid$myopia_3)/nrow(group2_covid)

##### Educational System
nokey_covid = comb_test[which(comb_test$school_key == 0 & comb_test$myopia_2 == 0),]
nokey_covid_incidence = sum(nokey_covid$myopia_3)/nrow(nokey_covid)
key_covid = comb_test[which(comb_test$school_key == 1 & comb_test$myopia_2 == 0),]
key_covid_incidence = sum(key_covid$myopia_3)/nrow(key_covid)

##### Region of habitation
rural_covid = comb_test[which(comb_test$school_town == 0 & comb_test$myopia_2 == 0),]
rural_covid_incidence = sum(rural_covid$myopia_3)/nrow(rural_covid)
urban_covid = comb_test[which(comb_test$school_town == 1 & comb_test$myopia_2 == 0),]
urban_covid_incidence = sum(urban_covid$myopia_3)/nrow(urban_covid)

##### Total
total_covid = comb_test[which(comb_test$myopia_2 == 0),]
total_covid_incidence = sum(total_covid$myopia_3)/nrow(total_covid)


####--------------------------- OR & CI ----------------------------------------####
#######  Fisher Test

##### Male
male_or = fisher.test(matrix(c(sum(male_covid$myopia_3 == 1),
                               sum(male_covid$myopia_3 == 0),
                               sum(male_normal$myopia_2 == 1),
                               sum(male_normal$myopia_2 == 0)),nrow=2,ncol=2))
male_or = paste0(round(male_or$estimate,2),
                 ' (',
                 paste(round(male_or$conf.int,2),collapse = '-'),
                 ')')

##### Female
female_or = fisher.test(matrix(c(sum(female_covid$myopia_3 == 1),
                                 sum(female_covid$myopia_3 == 0),
                                 sum(female_normal$myopia_2 == 1),
                                 sum(female_normal$myopia_2 == 0)),nrow=2,ncol=2))
female_or = paste0(round(female_or$estimate,2),
                   ' (',
                   paste(round(female_or$conf.int,2),collapse = '-'),
                   ')')

##### Grade Stage I
group1_or = fisher.test(matrix(c(sum(group1_covid$myopia_3 == 1),
                                 sum(group1_covid$myopia_3 == 0),
                                 sum(group1_normal$myopia_2 == 1),
                                 sum(group1_normal$myopia_2 == 0)),nrow=2,ncol=2))
group1_or = paste0(round(group1_or$estimate,2),
                   ' (',
                   paste(round(group1_or$conf.int,2),collapse = '-'),
                   ')')

##### Grade Stage II
group2_or = fisher.test(matrix(c(sum(group2_covid$myopia_3 == 1),
                                 sum(group2_covid$myopia_3 == 0),
                                 sum(group2_normal$myopia_2 == 1),
                                 sum(group2_normal$myopia_2 == 0)),nrow=2,ncol=2))
group2_or = paste0(round(group2_or$estimate,2),
                   ' (',
                   paste(round(group2_or$conf.int,2),collapse = '-'),
                   ')')

##### Non-Key School
noKey_or = fisher.test(matrix(c(sum(nokey_covid$myopia_3 == 1),
                                sum(nokey_covid$myopia_3 == 0),
                                sum(nokey_normal$myopia_2 == 1),
                                sum(nokey_normal$myopia_2 == 0)),nrow=2,ncol=2))
noKey_or = paste0(round(noKey_or$estimate,2),
                  ' (',
                  paste(round(noKey_or$conf.int,2),collapse = '-'),
                  ')')

##### Key School
key_or = fisher.test(matrix(c(sum(key_covid$myopia_3 == 1),
                              sum(key_covid$myopia_3 == 0),
                              sum(key_normal$myopia_2 == 1),
                              sum(key_normal$myopia_2 == 0)),nrow=2,ncol=2))
key_or = paste0(round(key_or$estimate,2),
                ' (',
                paste(round(key_or$conf.int,2),collapse = '-'),
                ')')

##### Rural
rural_or = fisher.test(matrix(c(sum(rural_covid$myopia_3 == 1),
                                sum(rural_covid$myopia_3 == 0),
                                sum(rural_normal$myopia_2 == 1),
                                sum(rural_normal$myopia_2 == 0)),nrow=2,ncol=2))
rural_or = paste0(round(rural_or$estimate,2),
                  ' (',
                  paste(round(rural_or$conf.int,2),collapse = '-'),
                  ')')

##### Urban
urban_or = fisher.test(matrix(c(sum(urban_covid$myopia_3 == 1),
                                sum(urban_covid$myopia_3 == 0),
                                sum(urban_normal$myopia_2 == 1),
                                sum(urban_normal$myopia_2 == 0)),nrow=2,ncol=2))
urban_or = paste0(round(urban_or$estimate,2),
                  ' (',
                  paste(round(urban_or$conf.int,2),collapse = '-'),
                  ')')

##### Total
total_or = fisher.test(matrix(c(sum(total_covid$myopia_3 == 1),
                                sum(total_covid$myopia_3 == 0),
                                sum(total_normal$myopia_2 == 1),
                                sum(total_normal$myopia_2 == 0)),nrow=2,ncol=2))
total_or = paste0(round(total_or$estimate,2),
                  ' (',
                  paste(round(total_or$conf.int,2),collapse = '-'),
                  ')')

incidences = data.frame(normal_rate = c(mal_normal_incidence,
                                        femal_normal_incidence,
                                        group1_normal_incidence,
                                        group2_normal_incidence,
                                        nokey_normal_incidence,
                                        key_normal_incidence,
                                        rural_normal_incidence,
                                        urban_normal_incidence,
                                        total_normal_incidence),
                        normal_number = c(nrow(male_normal),
                                          nrow(female_normal),
                                          nrow(group1_normal),
                                          nrow(group2_normal),
                                          nrow(nokey_normal),
                                          nrow(key_normal),
                                          nrow(rural_normal),
                                          nrow(urban_normal),
                                          nrow(total_normal)),
                        covid_rate = c(mal_covid_incidence,
                                       femal_covid_incidence,
                                       group1_covid_incidence,
                                       group2_covid_incidence,
                                       nokey_covid_incidence,
                                       key_covid_incidence,
                                       rural_covid_incidence,
                                       urban_covid_incidence,
                                       total_covid_incidence),
                        covid_number = c(nrow(male_covid),
                                         nrow(female_covid),
                                         nrow(group1_covid),
                                         nrow(group2_covid),
                                         nrow(nokey_covid),
                                         nrow(key_covid),
                                         nrow(rural_covid),
                                         nrow(urban_covid),
                                         nrow(total_covid)),
                        OR = c(male_or,
                               female_or,
                               group1_or,
                               group2_or,
                               noKey_or,
                               key_or,
                               rural_or,
                               urban_or,
                               total_or))
incidences$normal_ci = CI(incidences$normal_rate,incidences$normal_number)
incidences$covid_ci = CI(incidences$covid_rate,incidences$covid_number)

incidences$Normal = paste0(round(incidences$normal_rate*100,2),
                           '% (',
                           round(incidences$normal_rate*100 - incidences$normal_ci*100, 2),
                           '%-',
                           round(incidences$normal_rate*100 + incidences$normal_ci*100, 2),
                           '%)')
incidences$COVID = paste0(round(incidences$covid_rate*100,2),
                          '% (',
                          round(incidences$covid_rate*100 - incidences$covid_ci*100, 2),
                          '%-',
                          round(incidences$covid_rate*100 + incidences$covid_ci*100, 2),
                          '%)')
write.csv(incidences,'./results/incidences_or.csv',row.names = F)


####----------------------- plot table -------------------------------------####
###
library(forestplot)
forest_data = read.csv('./results/myopia_forest.txt',sep = '\t')
colnames(forest_data) = c('name','Normal (95% CI)', 'COVID-19 (95% CI)','OR (95% CI)')
forest_data$OR = sapply(forest_data$`OR (95% CI)`, function(x) as.numeric(strsplit(x,split = ' ')[[1]][1]))
forest_data$lower = sapply(forest_data$`OR (95% CI)`, function(x){
  a = strsplit(x,split = '\\(')[[1]][2]
  as.numeric(strsplit(a,split = '-')[[1]][1])
})
forest_data$upper = sapply(forest_data$`OR (95% CI)`, function(x){
  a = strsplit(x,split = '\\)')[[1]][1]
  as.numeric(strsplit(a,split = '-')[[1]][2])
})
forest_data$summary = is.na(forest_data$OR)
forest_data$summary[nrow(forest_data)] = T
pdf("./results/myopia_forest.pdf",width = 16)
forestplot(labeltext = as.matrix(forest_data[,c('name','Normal (95% CI)','COVID-19 (95% CI)','OR (95% CI)')]), 
           mean = forest_data$OR, 
           lower = forest_data$lower, 
           upper = forest_data$upper,
           is.summary = forest_data$summary,
           graph.pos = 5,
           zero = 1,
           new_page = FALSE,
           boxsize = c(rep(0.3,nrow(forest_data)-1),1),
           #grid = T,
           #txt_gp = fpTxtGp(ticks = gpar(cex=0.9), cex=1),
           #hrzl_lines =T,
           hrzl_lines = list("1" = gpar(lty=1, lwd= 2, columns=c(1:4),col='black'),
                             "2" = gpar(lty=1, columns=c(1:4),col='gray'),
                             "3" = gpar(lty=1, columns=c(1:4),col='gray'),
                             "4" = gpar(lty=1, columns=c(1:4),col='gray'),
                             "5" = gpar(lty=1, columns=c(1:4),col='gray'),
                             "6" = gpar(lty=1, columns=c(1:4),col='gray'),
                             "7" = gpar(lty=1, columns=c(1:4),col='gray'),
                             "8" = gpar(lty=1, columns=c(1:4),col='gray'),
                             "9" = gpar(lty=1, columns=c(1:4),col='gray'),
                             "10" = gpar(lty=1, columns=c(1:4),col='gray'),
                             "11" = gpar(lty=1, columns=c(1:4),col='gray'),
                             "12" = gpar(lty=1, columns=c(1:4),col='gray'),
                             "13" = gpar(lty=1, columns=c(1:4),col='gray')),
           #graphwidth=unit(50,"mm"),
           #clip=c(0.1,2.5), 
           colgap=unit(3,"mm"),
           lineheight = unit(9,"mm"),
           graphwidth = unit(c(60,10,10,10,10,10),"mm"),
           txt_gp = fpTxtGp(summary = gpar(cex=1,fontface=1),
                            ticks = gpar(cex=0.9), cex=1),
           #xticks.digits=10,
           #xticks = c(1,1.2),
           #xticks = c(0.9,1,1.4),
           #fn.ci_norm = matrix(c(rep("fpDrawNormalCI",12),"fpDrawDiamondCI"), 
           #                   nrow = 13, ncol=1),
           col = fpColors(box = '#2E5662',
                          zero='black',
                          lines = 'black'),
           shapes_gp = fpShapesGp(summary = list( # as many parameters as band per label
             gpar(fill = "#83A5B2", col = "black")
           ),
           zero=gpar(lty = 3)),
           grid = structure(c(1.18), 
                            gp = gpar(lty = 2, col = "black"))
)
dev.off()


