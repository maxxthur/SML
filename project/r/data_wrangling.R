pacman::p_load("tidyverse", "janitor")


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
  ungroup()


# Exploratory Data Analysis -----------------------------------------------

# plot target distribution





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


data_imp %>% 
  write_csv(file = "project/r/modeling/final data/data.csv")



  