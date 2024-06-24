#Import data
train <- read_csv("~/Desktop/Coding Temple/R/train.csv")
View(train)

#Understand the data set
diff_planets <- unique(train$HomePlanet)
print(diff_planets)
summary(train)

#1) Data Processing
## (a) remove unnecessary data 
passengers_spending <- select(train,
                       # HomePlanet,
                       VIP,
                       RoomService:VRDeck,
                       Name,
                       Transported
)

## (b) handle missing values:
### replace NA with 0 in all luxury amenities columns 
### drop rows where the VIP is NA
passengers_spending <- passengers_spending  %>% replace_na(list(
  RoomService = 0,
  FoodCourt = 0,
  ShoppingMall = 0,
  Spa = 0,
  VRDeck = 0
)) %>% 
  drop_na(VIP) 
  

#2) Visualization
#(a) calculate the total spending +
#    filter out the passengers who didnt spend any money on the amenities
##(i) passengers
passengers_spending <- passengers_spending %>%
 mutate(TotalSpending = rowSums(select(., RoomService:VRDeck), na.rm = TRUE)) %>%
 filter(TotalSpending > 0)

View(passengers_spending)

### see which amenity is the most popular 
TotalSpendingRoomService <-sum(passengers_spending$RoomService) / 100000 
TotalSpendingFoodCourt <-sum(passengers_spending$FoodCourt) / 100000 
TotalSpendingShoppingMall <-sum(passengers_spending$ShoppingMall) / 100000 
TotalSpendingSpa <-sum(passengers_spending$Spa) / 100000 
TotalSpendingVRDeck <-sum(passengers_spending$VRDeck) / 100000

amenities_data <- data.frame(Amenity = c("RoomService",
                                             "FoodCourt",
                                              "ShoppingMall",
                                              "Spa",
                                              "VRDeck"),
                                  Spending = c(TotalSpendingRoomService,
                                               TotalSpendingFoodCourt,
                                               TotalSpendingShoppingMall,
                                               TotalSpendingSpa,
                                               TotalSpendingVRDeck)
)

ggplot(amenities_data, aes(x = Amenity, y = Spending, fill = Amenity)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Spending on Each Amenity(include VIP and non-VIP)",
       x = "Amenities",
       y = "Spending(In Hundred Thousand)")

###find the proportion of VIP in the ship
VIP_proportion <- passengers_spending %>%
  group_by(VIP) %>%
  summarize(
   count = n()
  )

print(VIP_proportion)

# ggplot(passengers_spending) +
#   geom_bar(mapping = aes(x = VIP, fill = VIP)) +
#   labs(title = "Distribution of VIP Passengers")


##(ii) non-VIP
nonVIP_spending <- passengers_spending %>%
  mutate(TotalSpending = rowSums(select(., RoomService:VRDeck), na.rm = TRUE)) %>%
  filter(VIP == FALSE)
View(nonVIP_spending)

nonVIP_total <- sum(nonVIP_spending$TotalSpending)

TotalSpendingRoomService_nonVIP <- sum(nonVIP_spending$RoomService) / 100000
TotalSpendingFoodCourt_nonVIP <- sum(nonVIP_spending$FoodCourt) / 100000
TotalSpendingShoppingMall_nonVIP <- sum(nonVIP_spending$ShoppingMall) / 100000
TotalSpendingSpa_nonVIP <- sum(nonVIP_spending$Spa) / 100000
TotalSpendingVRDeck_nonVIP <- sum(nonVIP_spending$VRDeck) / 100000

amenities_data_nonVIP <- data.frame(Amenity =  c("RoomService",
                                                   "FoodCourt",
                                                  "ShoppingMall",
                                                   "Spa",
                                                   "VRDeck"),
                                     Spending = c(TotalSpendingRoomService_nonVIP,
                                                  TotalSpendingFoodCourt_nonVIP,
                                                  TotalSpendingShoppingMall_nonVIP,
                                                  TotalSpendingSpa_nonVIP,
                                                  TotalSpendingVRDeck_nonVIP),
                                    VIP_Status =c("FALSE",
                                                  "FALSE",
                                                  "FALSE",
                                                  "FALSE",
                                                  "FALSE")
                                       )

# ggplot(amenities_data_nonVIP, aes(x = Amenity, y = Spending, fill = Amenity)) +
#   geom_bar(stat = "identity") +
#   labs(title = "Total Spending on Each Amenity(non-VIP only)",
#        x = "Amenity",
#       y = "Spending(in Hundred Thousands)")


#(iii) VIP
 VIP_spending <- passengers_spending %>%
   mutate(TotalSpending = rowSums(select(., RoomService:VRDeck), na.rm = TRUE)) %>%
   filter(VIP == TRUE)
 View(VIP_spending)

 VIP_total <- sum(VIP_spending$TotalSpending)

 TotalSpendingRoomService_VIP <- sum(VIP_spending$RoomService) / 100000
 TotalSpendingFoodCourt_VIP <- sum(VIP_spending$FoodCourt) / 100000
 TotalSpendingShoppingMall_VIP <- sum(VIP_spending$ShoppingMall) / 100000
 TotalSpendingSpa_VIP <- sum(VIP_spending$Spa) / 100000 
 TotalSpendingVRDeck_VIP <- sum(VIP_spending$VRDeck) / 100000

 amenities_data_VIP <- data.frame(Amenity = c("RoomService",
                                          "FoodCourt",
                                          "ShoppingMall",
                                          "Spa",
                                         "VRDeck"),
                              Spending = c(TotalSpendingRoomService_VIP,
                                           TotalSpendingFoodCourt_VIP,
                                           TotalSpendingShoppingMall_VIP,
                                           TotalSpendingSpa_VIP,
                                           TotalSpendingVRDeck_VIP),
                              VIP_Status = c("TRUE",
                                             "TRUE",
                                             "TRUE",
                                             "TRUE",
                                             "TRUE")
 )
View(amenities_data_VIP)
ggplot(amenities_data_VIP, aes(x = Amenity, y = Spending, fill = Amenity)) +
   geom_bar(stat = "identity") +
   labs(title = "Total Spending on Each Amenity(VIP only)",
        x = "Amenity",
        y = "Spending(in Hundred Thousands)")



### create a double bar chart
merge_VIP_and_nonVIP <- union(amenities_data_VIP, amenities_data_nonVIP)
View(merge_VIP_and_nonVIP)


ggplot(merge_VIP_and_nonVIP, aes(x = Amenity, y = Spending, fill = VIP_Status)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparision of Spending Preference: VIP vs. non-VIP",
       x = "Amentities",
       y = "Spending (In Hundred Thoudsand")

###############################################################################
##(b) t-test:determine if there is a significant difference between the means of 
##           VIP and non-VIP and how they are related
VIP_TotalSpending <- summarize(VIP_spending,
                               VIP_mean = mean(TotalSpending), na.rm = TRUE
                               )

print(VIP_TotalSpending)

nonVIP_TotalSpending <- summarize(nonVIP_spending,
                               nonVIP_mean = mean(TotalSpending), na.rm = TRUE
)
print(nonVIP_TotalSpending)

t.test(VIP_spending$TotalSpending, nonVIP_spending$TotalSpending)

###############################################################################
#) find out who spend the most in amenities 
##passenger spend the most
data_order_by_totalspending <- filter(passengers_spending) %>% 
  arrange(desc(TotalSpending))
View(data_order_by_totalspending)
passenger_spend_the_most <- print(data_order_by_totalspending$Name)


#) VIP passengers spend the most
passengers_spending <- VIP_spending[order(VIP_spending$TotalSpending, decreasing = TRUE),] 
View(passengers_spending)

passengers_spending %>% select(passengers_spending, TotalSpending > 0)
##how much did he spend
spend_the_most <- summarize(VIP_spending, max(TotalSpending, na.rm = TRUE))
print(spend_the_most)

###############################################################################


