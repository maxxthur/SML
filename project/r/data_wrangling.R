pacman::p_load("tidyverse", "janitor", "naniar", "ggthemes", "xtable", "patchwork")


# Load data ---------------------------------------------------------------
raw_data <- read_csv("project/data/spaceship-titanic/train.csv")

# Clean data --------------------------------------------------------------

# inspect data
glimpse(raw_data)

# Check for NA

# Feature Engineering -----------------------------------------------------

# age groups
# total expenditure
# any expenditure
# (log transformations)
# family
# family size
# travel group
# travel group size

data_prel <- raw_data %>% 
  separate_wider_delim(cols = "Cabin",
                       delim = "/",
                       names = c("Deck", "Cabin_Number", "Side")) %>% # make three features instead of one
  # create age groups
  mutate(Age_Group = case_when(
    between(Age, 0, 12) ~ "0-12", 
    between(Age, 13, 17) ~ "13-17", 
    between(Age, 18, 25) ~ "18-25", 
    between(Age, 26, 30) ~ "26-30", 
    between(Age, 31, 50) ~ "31-50", 
    is.na(Age) ~ NA_character_, 
    T ~ "51+"
  )) %>% 
  # create expenditure related features
  mutate(total_expenditure = rowSums(.[, c("RoomService", "FoodCourt", "ShoppingMall", "Spa", "VRDeck")], na.rm = T), 
         any_expenditure = total_expenditure > 0) %>%
  # get family and family size
  mutate(family = word(Name, -1)) %>% 
  group_by(family) %>% 
  mutate(family_size = n()) %>% 
  ungroup() %>% 
  # get travel groups and their size
  mutate(travel_group = substr(PassengerId, 1, 4)) %>% 
  group_by(travel_group) %>% 
  mutate(travel_group_size = n()) %>% 
  ungroup() %>% 
  mutate(Cabin_Number = as.integer(Cabin_Number))


# Exploratory Data Analysis -----------------------------------------------

# plot target distribution
data_prel %>% 
  ggplot(aes(x = Transported, fill = Transported)) +
  scale_fill_manual(values = c("steelblue", "orange")) +
  geom_bar(stat = "count") +
  theme_classic() +
  theme(legend.position = "none")

ggsave("project/plots/target.jpg", width = 96, height = 72, units = "mm")

# plot of age
data_prel %>% 
ggplot(aes(x = Age)) +
  scale_color_manual(values = c("steelblue", "orange")) +
  scale_fill_manual(values = c("steelblue", "orange")) +
  geom_histogram(aes(fill = Transported, color = Transported), 
                 alpha=0.6, position = "identity", binwidth = 1) +
  theme_classic() +
  theme(legend.position = "bottom", 
        legend.title = element_blank())

ggsave("project/plots/age.jpg", width = 96, height = 72, units = "mm")

# plots for features related to booking details
data_prel %>% 
  select(HomePlanet, CryoSleep, VIP, Destination, Transported) %>% 
  mutate(Destination = case_when(
    str_detect(Destination, "55") ~ "C",
    str_detect(Destination, "22") ~ "PSO",
    str_detect(Destination, "1e") ~ "T"
  )) %>%
  mutate(across(.cols = everything(), ~ as.character(.x))) %>% 
  pivot_longer(cols = - Transported) %>% 
  filter(value != "NA") %>% 
  ggplot(aes(x = value, fill = as.logical(Transported))) +
  scale_fill_manual(values = c("steelblue", "orange")) +
  geom_bar(position = "dodge") +
  facet_wrap(~name, scales = "free") +
  theme_classic() +
  theme(legend.position = "bottom", 
        legend.title = element_blank()) +
  labs(x = "", y = "")

ggsave("project/plots/booking.jpg", width = 96, height = 72, units = "mm")


# location on spaceship
data_prel %>% 
  ggplot(aes(x = Cabin_Number, fill = Transported)) +
  scale_color_manual(values = c("steelblue", "orange")) +
  scale_fill_manual(values = c("steelblue", "orange")) +
  geom_histogram(position = "identity", alpha = 0.6, binwidth = 10) +
  theme_classic() +
  labs(x = "Cabin Number") + 
  theme(legend.position = "none")

ggsave("project/plots/cabin.jpg", width = 96, height = 72, units = "mm")

plot_deck <- data_prel %>% 
  filter(Deck != "NA") %>% 
  ggplot(aes(x = Deck, fill = Transported)) +
  scale_fill_manual(values = c("steelblue", "orange")) +
  geom_bar(position = "dodge") +
  theme_classic() +
  theme(legend.position = "none")

ggsave("project/plots/deck.jpg", width = 96, height = 72, units = "mm")

plot_side <- data_prel %>% 
  filter(Deck != "NA") %>% 
  ggplot(aes(x = Side, fill = Transported)) +
  scale_fill_manual(values = c("steelblue", "orange")) +
  geom_bar(position = "dodge") +
  theme_classic()

ggsave("project/plots/side.jpg", width = 96, height = 72, units = "mm")

  
  
# plot for extremely skewed  expenditure features - use log values
data_prel %>% 
  select(Transported, RoomService, FoodCourt, ShoppingMall, Spa, VRDeck, TotalExpenditure= total_expenditure) %>% 
  pivot_longer(cols = -Transported) %>% 
  ggplot(aes(x = log(value+1))) +
  scale_color_manual(values = c("steelblue", "orange")) +
  scale_fill_manual(values = c("steelblue", "orange")) +
  geom_histogram(aes(fill = Transported, color = Transported), 
                 alpha=0.6, position = "identity") +
  facet_wrap(~name) +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = "none")

ggsave("project/plots/expenditure.jpg", width = 96, height = 72, units = "mm")

# plot for engineered features
data_prel %>% 
  filter(!is.na(family)) %>% 
  select(family_size, travel_group_size, Transported) %>% 
  pivot_longer(cols = -Transported) %>% 
  ggplot(aes(x = value, fill = Transported)) +
  geom_bar(position = "dodge") +
  facet_wrap(~name, nrow = 2)

# heatmap of missing values
data_prel %>% 
  select(-Age_Group, -total_expenditure, -any_expenditure, -family, -family_size, 
         -travel_group, -travel_group_size) %>% 
  mutate(across(.cols = everything(), ~ is.na(.x))) %>% 
  mutate(row = row_number()) %>% 
  pivot_longer(cols = -row) %>% 
  ggplot(aes(x = name, y = row, fill = value)) +
    geom_tile() +
  labs(fill = "Missing", x = "") +
  scale_fill_manual(values = c("darkgreen", "orange")) +
  coord_flip() +
  theme(panel.border = element_blank(), 
        panel.background = element_blank(),
        legend.position = "bottom")
  
  ggsave("project/plots/missings.png", width = 96, height = 72, units = "mm")
  
data_prel %>% 
  select(-Age_Group, -total_expenditure, -any_expenditure, -family, -family_size, 
         -travel_group, -travel_group_size) %>% 
  mutate(across(.cols = everything(), ~ is.na(.x))) %>% 
  summarise(across(.cols = everything(), 
                   ~ mean(.x))) %>% 
  pivot_longer(cols = everything(), 
               names_to = "Variable", 
               values_to = "Share Missing") %>% 
  xtable()
  
  
  
  
    
  


# Impute NA ---------------------------------------------------------------

# 

# keep it easy and impute with median for numerical features and the mode
# for categorical ones

mode_impute <- function(x, logical = F) {
  out <- names(table(x)[which.max(table(x))])
  if(logical == T) return(as.logical(out))
  out
}

data_imp <- raw_data%>% 
  mutate(across(.cols = where(is.character), ~ replace_na(.x, mode_impute(.x)))) %>% 
  mutate(across(.cols = where(is.logical), ~ replace_na(.x, mode_impute(.x, logical = T)))) %>% 
  mutate(across(.cols = where(is.numeric), ~ replace_na(.x, median(.x, na.rm = T)))) %>% 
  separate_wider_delim(cols = "Cabin",
                       delim = "/",
                       names = c("Deck", "Cabin_Number", "Side")) %>% # make three features instead of one
  # create age groups
  mutate(Age_Group = case_when(
    between(Age, 0, 12) ~ "0-12", 
    between(Age, 13, 17) ~ "13-17", 
    between(Age, 18, 25) ~ "18-25", 
    between(Age, 26, 30) ~ "26-30", 
    between(Age, 31, 50) ~ "31-50", 
    is.na(Age) ~ NA_character_, 
    T ~ "51+"
  )) %>% 
  # create expenditure related features
  mutate(total_expenditure = rowSums(.[, c("RoomService", "FoodCourt", "ShoppingMall", "Spa", "VRDeck")], na.rm = T), 
         any_expenditure = total_expenditure > 0) %>%
  # get family and family size
  mutate(family = word(Name, -1)) %>% 
  group_by(family) %>% 
  mutate(family_size = n()) %>% 
  ungroup() %>% 
  # get travel groups and their size
  mutate(travel_group = substr(PassengerId, 1, 4)) %>% 
  group_by(travel_group) %>% 
  mutate(travel_group_size = n()) %>% 
  ungroup()

test <- sample(1:nrow(data_imp), 0.25 * nrow(data_imp))

set.seed(123)
dat_test <- data_imp[test, ]
dat_train <- data_imp[-test, ]

# save 
write_csv(dat_test, "project/r/modeling/final data/test.csv")
write_csv(dat_train, "project/r/modeling/final data/train.csv")

# Modeling ----------------------------------------------------------------
library(randomForest)
library(caret)

control <- trainControl(method = 'repeatedcv',
                        number = 10,
                        repeats = 3,
                        search = 'grid')
#create tunegrid
tunegrid <- expand.grid(.mtry = c(sqrt(ncol(dataset))))
modellist <- list()

#train with different ntree parameters

train <- dat_train %>% 
  select(Transported, total_expenditure) %>% 

  fit <- train(Transported~.,
               data = train,
               method = 'rf',
               metric = 'Accuracy',
               tuneGrid = tunegrid,
               trControl = control)

#Compare results
results <- resamples(modellist)
summary(results)

  