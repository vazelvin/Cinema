library("ggplot2")
library("dplyr")
library("tidyverse") 
library("ggpubr")
library("gtools")
library("ClusterR")
library("cluster")

##################################Part A.Data Preparation#########################################
# Load the data
customer_pre_data = read.csv("PrimeAnalytics_CustomerDetail_cleaned.csv")
points_pre_data = read.csv("PrimeAnalytics_Points_cleaned.csv")

# Remove unwanted variables
drop = c("pointid","pointtypeid","ex_sourceid")
points_data = points_pre_data[,!(names(points_pre_data) %in% drop)]
drop2 = c("City","FSA")
customer_data = customer_pre_data[,!(names(customer_pre_data) %in% drop2)]

## Change the date format
points_data1 = points_data %>% mutate(date = as.Date(Date, format = "%y/%m/%d"))
points_data1 = points_data1[,(names(points_data1) != "Date")]
points_data1$Year <- as.numeric(format(points_data1$date, "%Y"))

# Remove the "2023","2024","2025","2026","2027","2028","2029","2030","2031" 
points_data1 = points_data1[!(points_data1$Year %in% c("2023","2024","2025","2026",
                                                       "2027","2028","2029","2030","2031")),]
## Join two data
Total_data = merge(x=customer_data,y=points_data1,by="Unique_member_identifier",all=TRUE)
## Remove unused data and list
rm("customer_data","points_pre_data","points_data","points_data1","drop","drop2","customer_pre_data")

##################################Part B.Data Exploration#########################################
#################Part i.Total spend amount and Total earned points in each Year##################
# Total spend amount in each Year
Tspend_amount = Total_data %>% select(TransAmount,Year,points)%>%
  filter(Year %in% c(2000:2029),TransAmount>0,points>0)%>%group_by(Year)%>%
  summarise(TotalAmount = sum(TransAmount))

# Total earned points in each Year
Tearn_points = Total_data %>% select(points,Year,TransAmount)%>%
  filter(Year %in% c(2000:2029),points>0,TransAmount>0)%>%group_by(Year)%>%
  summarise(TotalPoints = sum(points))

## Make the two data frames into a list
amount_points_list = list(Tspend_amount,Tearn_points)

## merge all data frames together
amount_points_df = amount_points_list %>% reduce(full_join, by='Year')

## Aggregate the points and amount
amount_points_df1 = amount_points_df %>% 
  pivot_longer(cols = c('TotalPoints',
                        'TotalAmount'),
               names_to = "Type",values_to = "Total")

## Show the line chart
amount_points_line = ggplot(amount_points_df1,aes(x=Year,y = Total,color = Type))+
  geom_line(size = 1)+geom_point()+theme_classic()+ 
  geom_text(aes(label = Total),vjust=-0.5)+theme(legend.position = c(0.2, 0.8))+
  ylab("Total spend amount and Total earned points")+
  ggtitle("Total spend amount and Total earned points for each Year")
amount_points_line
######################Part ii.Proportion of days used by members ############################

## Calculate the days that customers used with the N/A
cust_days_wna = Total_data %>% select(Unique_member_identifier,date)%>% 
  group_by(Unique_member_identifier)%>%
  summarise(lastdate = max(date),firstdate = min(date))%>%
  mutate(days_diff = as.integer(difftime(lastdate, firstdate, units = "days")))

cust_level_wna = cust_days_wna %>% 
  mutate(Days_level =case_when(days_diff >= 7300 ~'More than 20 years',
                               days_diff >= 3650 ~ 'Between 10 to 20 years',
                               days_diff >= 1825 ~ 'Between 5 to 10 years',
                               days_diff >= 730 ~ 'Between 2 to 5 years',
                               days_diff >= 365 ~ 'Between 1 to 2 years',
                               days_diff >= 0 ~ 'Less than 1 year',
                               TRUE ~ 'N/A'))


cust_level_n_wna = cust_level_wna %>% group_by(Days_level)%>% summarise(Number = n())


customer_pct_wna = round(100*cust_level_n_wna$Number/sum(cust_level_n_wna$Number),digits=2)

## Show pie chart
pie(cust_level_n_wna$Number,
    labels = paste(cust_level_n_wna$Days_level, sep = " ", customer_pct_wna, "%"), 
    col = rainbow(length(cust_level_n_wna$Number)), 
    main = "Proportion of the days used by members with N/A")

## Calculate the days that customers used without the N/A
cust_days = Total_data %>% select(Unique_member_identifier,date)%>% 
  group_by(Unique_member_identifier)%>% filter(!is.na(date))%>%
  summarise(lastdate = max(date),firstdate = min(date))%>%
  mutate(days_diff = as.integer(difftime(lastdate, firstdate, units = "days")))


cust_level = cust_days %>% 
  mutate(Days_level =case_when(days_diff >= 7300 ~'More than 20 years',
                               days_diff >= 3650 ~ 'Between 10 to 20 years',
                               days_diff >= 1825 ~ 'Between 5 to 10 years',
                               days_diff >= 730 ~ 'Between 2 to 5 years',
                               days_diff >= 365 ~ 'Between 1 to 2 years',
                               days_diff >= 0 ~ 'Less than 1 year'))


cust_level_n = cust_level %>% group_by(Days_level)%>% summarise(Number = n())


customer_pct = round(100*cust_level_n$Number/sum(cust_level_n$Number),digits=2)

## Show pie chart
pie(cust_level_n$Number,
    labels = paste(cust_level_n$Days_level, sep = " ", customer_pct, "%"), 
    col = rainbow(length(cust_level_n$Number)), 
    main = "Proportion of days used by members without N/A")


##########################Part iii.Distribution of earned points and spend amount######################
## Boxplot of the earned points 
freq_Pt = Total_data %>% select(points)%>%filter(points>0)%>%drop_na()
# Show the boxplot
freq_Pt_box = ggplot(freq_Pt, aes(y=points)) + 
  geom_boxplot(outlier.shape = NA,fill = "blue")+coord_cartesian(
    ylim = quantile(freq_Pt$points,c(0.1,0.9)))+
  theme(legend.position = "none")+
  ggtitle("Distribution of the earned points")

## Boxplot of the spend amount 
freq_TA = Total_data %>% select(TransAmount)%>%filter(TransAmount>0)%>%drop_na()
# Show the boxplot
freq_TA_box = ggplot(freq_TA, aes(y=TransAmount)) + 
  geom_boxplot(outlier.shape = NA,fill = "green")+coord_cartesian(
    ylim = quantile(freq_TA$TransAmount,c(0.1,0.9)))+
  theme(legend.position = "none")+
  ggtitle("Distribution of the spend amount")

## Combine three plots into one plot
ggarrange(freq_Pt_box,                                               
          freq_TA_box,
          ncol = 2) 

##################################Part C.Data Analysis###########################################
###############################Part i.K-Mean Cluster##############################################

clus = Total_data %>% select(ex_transactiondescription,points,TransAmount)%>%
  filter(points >0,TransAmount>0,points<40,TransAmount<80)%>%drop_na()

set.seed(1)
## K-Mean Cluster
cluster_prime = kmeans(clus[, 2:3], 3, nstart = 25)

cluster_prime$cluster = as.factor(cluster_prime$cluster)
## 3-Mean cluster plot
cluster_plot = ggplot(clus, aes(points, TransAmount, color=cluster_prime$cluster)) + 
  geom_point()+ggtitle("3-Mean Cluster Plot")
cluster_plot

## Calculate the size for each cluster
cluster_size = c(cluster_prime$size)
cluster_1 = round(cluster_size / sum(cluster_size) *100,2)

###############################Part ii.Based on Gender##############################################
## Top 5 transaction descriptions in each gender by transactions
n_trans_gender = Total_data %>%
  select(gender,points,TransAmount,ex_transactiondescription)%>%
  filter(points >0,TransAmount>0,points<35,TransAmount<19)%>%
  group_by(gender,ex_transactiondescription)%>%summarise(Numbers = n())%>%
  arrange(desc(Numbers))%>%top_n(5)
n_trans_gender_bar = ggplot(data = n_trans_gender,aes(x=gender,
                                                      y=Numbers,
                                                      fill=ex_transactiondescription))+
  geom_bar(colour="black",stat = "identity",position = "dodge")+
  geom_text(aes(label = Numbers),position = position_dodge(width = .9),vjust=-0.5)+
  theme(legend.position = c(0.8, 0.8))+ylab("Number of transactions")+
  ggtitle("Top 5 transaction descriptions in each gender by number of transactions")
n_trans_gender_bar


###############################Part iii.Based on Age##############################################
## Top 5 transaction descriptions in each age by number of transactions
n_trans_age = Total_data %>%
  select(age_class,points,TransAmount,ex_transactiondescription)%>%
  filter(points >0,TransAmount>0,points<35,TransAmount<19)%>%
  group_by(age_class,ex_transactiondescription)%>%summarise(Numbers = n())%>%
  arrange(desc(Numbers))%>%top_n(5)
n_trans_age_bar = ggplot(data = n_trans_age,aes(x=age_class,y=Numbers,fill=ex_transactiondescription))+
  geom_bar(colour="black",stat = "identity",position = "dodge")+
  geom_text(aes(label = Numbers),position = position_dodge(width = .9),vjust=-0.5)+
  theme(legend.position = c(0.8, 0.8))+ylab("Number of transactions")+
  ggtitle("Top 5 transaction descriptions in each age_class by number of transactions")
n_trans_age_bar

###############################Part iv.Based on Province############################################
## Top 5 transaction descriptions in each province by number of transactions
n_trans_prov = Total_data %>%
  select(StateProv,points,TransAmount,ex_transactiondescription)%>%
  filter(points >0,TransAmount>0,points<35,TransAmount<19)%>%
  group_by(StateProv,ex_transactiondescription)%>%summarise(Numbers = n())%>%
  arrange(desc(Numbers))%>%top_n(5)
n_trans_prov_bar = ggplot(data = n_trans_prov,aes(x=StateProv,y=Numbers,fill=ex_transactiondescription))+
  geom_bar(colour="black",stat = "identity",position = "dodge")+
  geom_text(aes(label = Numbers),position = position_dodge(width = .9),vjust=-0.5)+
  theme(legend.position = c(0.8, 0.8))+ylab("Number of transactions")+
  ggtitle("Top 5 transaction descriptions in each province by number of transactions")
n_trans_prov_bar


