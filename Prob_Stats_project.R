library(e1071)
library(tidyverse)
install.packages('reshape')
library(reshape)


setwd('C:/Users/User/Desktop/sem1/eps/group4/projects/cosmeticsproject')
getwd()

adversefood <- read.csv('Cosmetic.csv')
target<-c("ALOPECIA","HAIR COLOUR CHANGES","HAIR DISORDER","HAIR GROWTH ABNORMAL","HAIR TEXTURE ABNORMAL","HYPOTRICHOSIS","TRICHORRHEXIS")

cosmetics<-dplyr::filter(adversefood,Symptoms%in% target)

cosmetics_serious<-dplyr::filter(cosmetics,cosmetics$Outcomes=="SERIOUS ")
cosmetics_non_serious<-dplyr::filter(cosmetics,cosmetics$Outcomes=="NON-SERIOUS")



mean_age<-mean(cosmetics$Age)
mean_age





median_age<-median(cosmetics$Age)
median_age



range_age<-range(cosmetics$Age)
range_age


IQR_age<-IQR(cosmetics$Age)
IQR_age


var_age<-var(cosmetics$Age)
var_age


sd_age<-sd(cosmetics$Age)
sd_age


cv_age<- ((sd_age/mean_age)*100)
cv_age


skewness_age<-skewness(cosmetics$Age)
skewness_age


kurtosis_age<-kurtosis(cosmetics$Age)
kurtosis_age

#Histogram
ggplot(data=cosmetics) +
  geom_histogram(mapping = aes(x=Age), binwidth=10,color= 'white',fill='#424242')+
  ggtitle("Plot of Age v/s count") +
  theme(
    plot.title = element_text(color="red"
                              , size=14, face="bold.italic",hjust = 0.5)
  )
    

#Scatterplot
ggplot(data=cosmetics) +
  geom_point(mapping = aes(x = Age, y = Gender, color = Gender)) +
  geom_vline(xintercept = mean_age,color="#424242") +
  ggtitle("Plot of Gender by Age") +
  theme(
    plot.title = element_text(color="red", size=14, face="bold.italic",hjust = 0.5,vjust = 0)
  )

  

#Boxplot
boxplot <- ggplot(data =adversefood) +
  geom_boxplot(mapping = aes(x=Symptoms%in%target  , y= Age, color=Symptoms%in%target))+
  ggtitle("Plot of Cosmetics Related Symptoms \n by Age") +
  xlab("Cosmetics Related Symptoms")+
  labs(color = "Hair related issues")+
theme(
  plot.title = element_text(color="red", size=14, face="bold.italic",hjust = 0.5)
)

boxplot


 





wendata <- cosmetics[grep("WEN", cosmetics$Reported.Brand.Product.Name), ]
  head(wendata)
  
wendata_serious<-dplyr::filter(wendata,wendata$Outcomes=="SERIOUS ")  
wendata_non_serious<-dplyr::filter(wendata,wendata$Outcomes=="NON-SERIOUS")



reqd_cols<-dplyr::select(wendata,Reported.Brand.Product.Name,Age,Gender,Outcomes,Symptoms)

wen_diffproducts_report<-dplyr::group_by(reqd_cols,reqd_cols$Reported.Brand.Product.Name,reqd_cols$Symptoms)%>%
  summarise(brand_reported_count=n())


view(wen_diffproducts_report)

wen_report_count<-group_by(wen_diffproducts_report,brand_reported_count)%>%
  summarise(total_reported_count_ntimes=n())

view(wen_report_count)

count_pmf<-round(wen_report_count$total_reported_count_ntimes/sum(wen_report_count$total_reported_count_ntimes),4)
count_pmf

count_cdf<-round(cumsum(count_pmf),4)
count_cdf


combined_table<-cbind(wen_report_count,count_pmf,count_cdf)
view(combined_table)


ggplot(data=combined_table) +
  geom_bar(mapping = aes(brand_reported_count,count_pmf),stat = "identity",color= 'white',fill='#424242')+
  ggtitle("Brand_reported_count v/s count_pmf") +
  xlim(0, 40)+
  theme(
    plot.title = element_text(color="red", size=14, face="bold.italic",hjust = 0.5)
  )

  
  
  #expected value
  
weighted.mean(combined_table$brand_reported_count, combined_table$count_pmf)




view(wendata)


wendatafreq<- wendata %>%
  select(Reported.Brand.Product.Name,Age,Gender,Outcomes,Symptoms) %>%
  group_by(Reported.Brand.Product.Name) %>%
  summarise(brand_reported_count=n()) %>%
  group_by(brand_reported_count) %>%
  summarise(total_reported_count_ntimes=n()) %>%
  mutate(wen_data_pmf = round(total_reported_count_ntimes/sum(total_reported_count_ntimes),3)) %>%
  mutate(wen_data_cdf=round(cumsum(wen_data_pmf),3))


view(wendatafreq)





wenconditionerdata <- wendata[grep("WEN CONDITIONER", wendata$Reported.Brand.Product.Name), ]


view(wenconditionerdata)

wenconditionerfreq<- wenconditionerdata %>%
  select(Reported.Brand.Product.Name,Age,Gender,Outcomes,Symptoms) %>%
  group_by(Reported.Brand.Product.Name) %>%
  summarise(brand_reported_count=n()) %>%
  
  mutate(wen_conditionerdata_pmf = round(brand_reported_count/sum(brand_reported_count),3)) %>%
  mutate(wen_conditionerdata_cdf=round(cumsum(wen_conditionerdata_pmf),3))


view(wenconditionerfreq)

wenconditionerfreq$Reported.Brand.Product.Name
wendatafreq$Reported.Brand.Product.Name

joint_freq <- outer(wendatafreq$brand_reported_count,wenconditionerfreq$brand_reported_count, FUN = "+")
rownames(joint_freq) <- wendatafreq$brand_reported_count
colnames(joint_freq) <- (wenconditionerfreq$Reported.Brand.Product.Name)

joint_freq

joint_prob <- round(joint_freq/sum(joint_freq),3)
joint_prob





combined_table_serious<- wendata_serious %>%
  select(Reported.Brand.Product.Name,Age,Gender,Outcomes,Symptoms) %>%
  group_by(Reported.Brand.Product.Name,Symptoms) %>%
  summarise(brand_reported_count=n()) %>%
  group_by(brand_reported_count) %>%
  summarise(total_reported_count_ntimes=n()) %>%
  mutate(wen_serious_pmf = round(total_reported_count_ntimes/sum(total_reported_count_ntimes),3)) %>%
  mutate(wen_serious_cdf=round(cumsum(wen_serious_pmf),3))



view(combined_table_serious)
#cosmetics_serious_freq
cosmetics_serious_freq<- cosmetics_serious %>%
  select(Reported.Brand.Product.Name,Age,Gender,Outcomes,Symptoms) %>%
 group_by(Reported.Brand.Product.Name,Symptoms) %>%
  summarise(brand_reported_count=n()) %>%
  group_by(brand_reported_count) %>%
  summarise(total_reported_count_ntimes=n()) %>%
  mutate(cosmetic_serious_pmf = round(total_reported_count_ntimes/sum(total_reported_count_ntimes),3)) %>%
 mutate(cosmetic_serious_cdf=round(cumsum(cosmetic_serious_pmf),3))


view(cosmetics_serious_freq)
joint_freq <- outer(cosmetics_serious_freq$total_reported_count_ntimes,combined_table_serious$total_reported_count_ntimes, FUN = "+")
rownames(joint_freq) <- cosmetics_serious_freq$brand_reported_count
colnames(joint_freq) <- combined_table_serious$brand_reported_count

joint_freq

joint_prob <- round(joint_freq/sum(joint_freq),3)
joint_prob


#cosmetic_non_serious

cosmetics_nonserious_freq<-cosmetics_non_serious %>%
  select(Reported.Brand.Product.Name,Age,Gender,Outcomes,Symptoms) %>%
  group_by(Reported.Brand.Product.Name,Symptoms) %>%
  summarise(brand_reported_count=n()) %>%
  group_by(brand_reported_count) %>%
  summarise(total_reported_count_ntimes=n()) %>%
  mutate(cosmetic_nonserious_pmf = round(total_reported_count_ntimes/sum(total_reported_count_ntimes),3)) %>%
  mutate(cosmetic_nonserious_cdf=round(cumsum(cosmetic_nonserious_pmf),3))

view(cosmetics_nonserious_freq)

wen_nonserious_freq<- wendata_non_serious %>%
  select(Reported.Brand.Product.Name,Age,Gender,Outcomes,Symptoms) %>%
  group_by(Reported.Brand.Product.Name,Symptoms) %>%
  summarise(brand_reported_count=n()) %>%
  group_by(brand_reported_count) %>%
  summarise(total_reported_count_ntimes=n()) %>%
  mutate(wen_nonserious_pmf = round(total_reported_count_ntimes/sum(total_reported_count_ntimes),3)) %>%
  mutate(wen_nonserious_cdf=round(cumsum(wen_nonserious_pmf),3))


view(wen_nonserious_freq)

joint_freq <- outer(cosmetics_nonserious_freq$total_reported_count_ntimes,wen_nonserious_freq$total_reported_count_ntimes, FUN = "+")
rownames(joint_freq) <- cosmetics_nonserious_freq$brand_reported_count
colnames(joint_freq) <- wen_nonserious_freq$brand_reported_count


joint_freq

joint_prob <- round(joint_freq/sum(joint_freq),3)
joint_prob







#which wen product is worst  where in the reported incident is serious
worst_brand_serious<-wendata_serious%>%
                      select(Reported.Brand.Product.Name,Age,Gender,Outcomes,Symptoms) %>%
                      group_by(Reported.Brand.Product.Name) %>%
                      summarise(count=n())%>%
  arrange(desc(count))
view(worst_brand_serious)
print(paste0("the worst product under serious incidents is ",  worst_brand_serious$Reported.Brand.Product.Name[1]," with count ",worst_brand_serious$count[1]))

view(wendata_non_serious)

#which wen product is worst where in the reported incident is non serious
worst_brand_nonserious<-wendata_non_serious%>%
  select(Reported.Brand.Product.Name,Age,Gender,Outcomes,Symptoms) %>%
  group_by(Reported.Brand.Product.Name) %>%
  summarise(count=n())%>%
  arrange(desc(count))
view(worst_brand_nonserious)
print(paste0("the worst product under non serious incidents is ",  worst_brand_nonserious$Reported.Brand.Product.Name[1]," with count ",worst_brand_nonserious$count[1]))




# probability that there were more than 700 serious events
#X -RV of Serious events 

#calculating n 

summary(cosmetics)

#serious=730, nonserious=517, n=1247, p=x/n =730/1247=0.585, q=0.415


#P(x>=800)=1-F(1)

1-pbinom(q=699,size=1247,prob = 0.585)



# probability that there were less than 500 non serious events
#X -RV of Non Serious events 

#calculating n 

summary(cosmetics)

#serious=730, nonserious=517, n=1247, p=x/n =517/1247=0.415, q=0.585

pbinom(q=500,size=1247,prob=0.415)









#sampling
Sampledata1 <- sample_n(cosmetics, 700)
# one sample z test 
# X-RV of age of a person
# Check if Mean sample age is different from population age

# defining Hypotheis
# H0: ?? = ??0
# H1: ?? != ??0
# we take the confidence interval to be 95%
# alpha will be 0.05
# #  rejection region -z0.025=-1.96 and z0.025=1.96
z.test <- function(sample, pop){
  sample_mean = mean(sample) 
  pop_mean = mean(pop)
  n = length(sample) 
  var = var(pop)
  z = (sample_mean - pop_mean) / (sqrt(var/(n))) 
  return(z)
}



# performing the z test
z.test(Sampledata1$Age, cosmetics$Age)

#since the z value is 0.196 it does not fall in the rejection region 
# Hence we fail to reject the NULL Hyothesis  and can say that there is no significant between the sample mean age and population mean age
t.test(cosmetics_serious$Age, mu = mean(cosmetics$Age))

#Since Pvalue=0.8064>0.05 we  reject Null Hypothesis and can say that there is  difference between the sample mean age of people with serious outcomes and population mean age

cen_1 <- cosmetics[1:400,] #select all columns and rows from 1 to 16000  
cen_2 <- cosmetics[401:801,] #select all columns and rows from 16001 to 32561
cen_1_sample <- sample_n(cen_1, 300) #sample 1000 rows from the first population
cen_2_sample <- sample_n(cen_2, 300) #sample 1000 rows from the second population

# two sample z test
z_test2 = function(a, b, var_a, var_b){
  n.a = length(a)
  n.b = length(b)
  z = (mean(a) - mean(b)) / (sqrt((var_a)/n.a + (var_b)/n.b))
  return(z)
}

# performing two sample z test
z_test2(cen_1_sample$Age, cen_2_sample$Age, var(cen_1_sample$Age), var(cen_2_sample$Age))


# #  rejection region -z0.025=-1.96 and z0.025=1.96
#since the z value is 1.92 it falls in the rejection region 
# Hence we  reject the NULL Hyothesis  and can say that the true difference in means is not equal to 0

# performing two sample t test
t.test(cen_1_sample$Age, cen_2_sample$Age)
# since p-value=0.05462>0.050 we reject the NULL Hypothesis and accept the alternate hypothesis and can conclude that the true difference in means is not equal to 0

