################################################################################
# GOAL                                                                         #
################################################################################

# This script will perform one hot encoding for the `Mechanics` and `Domains`
# fields included in the `boardgames` data frame. The output object will be
# called `boardgames_clean`.

################################################################################

# Create a lookup table of top mechanics
mechanic_counts <-
  
  boardgames %>% 
  
  mutate(Mechanics = str_extract_all(Mechanics, pattern = "[A-Za-z-\\/ ]+")) %>% 

  # Expand the genres column so that there is one record per id per genre
  unnest(Mechanics) %>% 
  
  # Clean things up
  mutate(Mechanics = str_trim(Mechanics),
         occurs = 1L) %>% 
  
  count(Mechanics, occurs, name = "count_mechanics", sort = T) %>% 
  
  # Only select the top 20 mechanics - all others consolidated
  mutate(rank = row_number(desc(count_mechanics)),
         new_mechanic = case_when(
           is.na(Mechanics) ~ "No Mechanic",
           rank <= 20 ~ Mechanics,
           TRUE ~ "Other"
         )) %>% 
  
  select(Mechanics, occurs, new_mechanic)


# One hot encoding of the mechanics column
boardgames_mechanics <- 
  
  boardgames %>% 
  
  mutate(Mechanics = str_extract_all(Mechanics, pattern = "[A-Za-z-\\/ ]+")) %>% 
  
  # Expand the genres column so that there is one record per id per genre
  unnest(Mechanics) %>% 
  
  # Clean things up
  mutate(Mechanics = str_trim(Mechanics)) %>% 
  
  left_join(mechanic_counts, by = "Mechanics") %>% 
  
  select(-Mechanics) %>% 
  
  distinct() %>% 
  
  # Make the dataset wide again
  pivot_wider(names_from = new_mechanic,
              names_prefix = "mechanic_",
              values_from = occurs)


# One hot encoding the domains column
boardgames_clean <- 
  boardgames_mechanics %>% 
  
  mutate(Domains = str_extract_all(Domains, pattern = "[A-Za-z-\\/' ]+")) %>% 
  
  # Expand the genres column so that there is one record per id per genre
  unnest(Domains) %>% 
  
  mutate(Domains = if_else(is.na(Domains), "No Domain", Domains),
         Domains = str_trim(Domains),
         occurs = "x") %>% 
  
  # Make the dataset wide again
  pivot_wider(names_from = Domains,
              names_prefix = "domain_",
              values_from = occurs) %>% 
  
  mutate(across(c(`Owned Users`, `Play Time`), ~ if_else(. == 0, NA_real_, .)))
