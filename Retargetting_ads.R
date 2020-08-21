#Main purpose of the model will be focusing on how we can optimize our ad campaigns for better reach and so it could impact the business of a company in a good way.
#Lets load the data, I would suggest you to go through the dataset to understand the variables better.
Ad_campaign <- read.csv("C:/Users/Asish nayak/Desktop/Strirring minds/datasets_2678_4448_KAG_conversion_data.csv")
View(Ad_campaign)
#In my opinion we don't need the variables 'ad_id','fb_campaign_id','Total_conversion' as the dataset can get us the required insights even without these variables. 
library("dplyr")
Ad_campaign <- select(Ad_campaign, -ad_id, -fb_campaign_id, -Total_Conversion)
#Cleaning the dataset and understanding it for better analysis.
colSums(is.na(Ad_campaign))
str(Ad_campaign)
summary(Ad_campaign)

#Let us now compare the independent variables with the dependent one, which is 'Approved_Conversion' varibale which denotes the successful coversions of ads to application towards the content of the ad.
#Relation between a particular company's ad and the Approved status!
library("ggplot2")
xtabs(Approved_Conversion ~ xyz_campaign_id, data = Ad_campaign)
Ad_campaign$xyz_campaign_id <- as.factor(Ad_campaign$xyz_campaign_id)
Ad_campaign$Approved_Conversion <- as.factor(Ad_campaign$Approved_Conversion)
ggplot(Ad_campaign, aes(x = Approved_Conversion, fill = xyz_campaign_id)) +
  geom_bar(position = "stack")
#We see that the company having the id of 1178 is having a better marketting campaign which leads to successful coversion.

#Relation between age and the Approved_Conversions.
Ad_campaign$Approved_Conversion <- as.integer(Ad_campaign$Approved_Conversion)
xtabs(Approved_Conversion ~ age, data = Ad_campaign)
ggplot(Ad_campaign, aes(x = Approved_Conversion, fill = age)) +
  geom_bar(position = "stack")
#We see that the age group of 30-34 are following up the ads more than any other age group.

#Relation between gender and the Approved_Conversions.
xtabs(Approved_Conversion ~ gender, data = Ad_campaign)
ggplot(Ad_campaign, aes(x = Approved_Conversion)) + geom_histogram() + facet_grid(gender ~.)      
#Males are following up the ads slightly more than the female ones.

#Relation between impressions and the Approved_Conversions.
ggplot(Ad_campaign, aes(x = Approved_Conversion, y = Impressions)) +
  geom_point()
#This relation is quite complex to optimize.

#Relation between number of clicks on the ad and its successful conversion.
xtabs(Clicks ~ Approved_Conversion+xyz_campaign_id, data = Ad_campaign)
ggplot(Ad_campaign, aes(x = Approved_Conversion, y = Clicks)) +
  geom_point()
ggplot(Ad_campaign, aes(x = Approved_Conversion, y = Clicks)) + geom_boxplot()
ggplot(Ad_campaign, aes(x = Approved_Conversion, fill = Clicks)) +
  geom_bar(position = "stack")
#Generally we would expect that more the number of clicks, more should be the approved status but it's not the case in here as the data is quite random(Though most of the clicks have produced either 0 or 1 conversions and highest has been around 21 approved conversions).
#And we see that the ad of the company having Id '1178' is getting more number of clicks and accordingly more coversions of approvals.

#Relation between the price of producing the ads on facebook and the approved status.
xtabs(Spent ~ Approved_Conversion+xyz_campaign_id, data = Ad_campaign)
ggplot(Ad_campaign, aes(x = Approved_Conversion, y = Spent)) +
  geom_point()
ggplot(Ad_campaign, aes(x = Approved_Conversion, fill = Spent)) +
  geom_bar(position = "stack")
#The company having the Id '1178' is spending more amount of money for producing the ads.



#For descriptive definition of the variables, simply use summary function.
summary(Ad_campaign)
#Or we can use it in more depth using the dplyr package!
x <- group_by(Ad_campaign, Approved_Conversion)

summarise(x, mean_value = mean(Impressions))
#As expected, more the number of ad, more will be the chance of approvals.
summarise(x, mean_value = mean(Spent))
#On an average the amount spent for producing ads is high for more no. of approvals.
summarise(x, mean_value = mean(Clicks))
#More the number of clicks, more will be the successful approvals.


###IMPORTANT INSIGHTS FROM THE ANALYSIS!
# A.Company having the ID '1178' is having better marketing grip as it has most number of approval_conversions. (So we should focus more on the other companies for their growth.)
# B.The Age group of 30-34 is the most vulnerable one for the approval conversions following the ads, so we should target that age group!
# C.Males are following the ads slightly more compared to the females, so we should look to produce ads that could infulence the females as well for improving the stats.
# D.More the number of apprearance of ads, more will be the count of successful approvals.
# E.The ad of the company having ID '1178' is influencing the users more and also it's found on an average that more the clicks on the add, more will be the coversions. (So we should look to improve the marketing trategy regarding the ads for the companies having ID '916'&'936'.)
# F.The company having ID '1178' is spending more amount of money for producing the ads on facebook and so the count of approval_conversions is also high for the same reason as it's influencing more users.
