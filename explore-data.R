# https://www.kaggle.com/ai-first/cocktail-ingredients

library(tidyverse)
library(here)

col_spec <-
  cols(
    .default = col_character(),
    X1 = col_skip(),
    dateModified = col_skip(),
    idDrink = col_skip(),
    strIngredient13 = col_skip(),
    strIngredient14 = col_skip(),
    strIngredient15 = col_skip(),
    strMeasure13 = col_skip(),
    strMeasure14 = col_skip(),
    strMeasure15 = col_skip(),
    strVideo = col_skip()
  )

cocktails <- 
  read_csv(here("raw_data", "all_drinks.csv"), col_types = col_spec) %>% 
  select(strDrink:strIngredient1, strIngredient2:strIngredient9, strIngredient10:strIngredient12,
         strMeasure1:strMeasure9, strMeasure10:strMeasure12, strInstructions) %>% 
  rename_all(~ str_to_lower(str_remove(., "str"))) %>% 
  mutate_at(vars(starts_with("ingredient")), ~ str_to_lower(chartr("ñä", "na", .)))

ingredient_conversion <- 
  read_csv(here("ingredient-conversion.csv")) %>% 
  mutate(new_ingredient = if_else(is.na(new_ingredient), original_ingredient, new_ingredient))

ingredients <-
  ingredient_conversion %>% 
  pull(new_ingredient) %>% 
  setNames(ingredient_conversion$original_ingredient)


cocktails <- 
  cocktails %>% 
  mutate_at(vars(starts_with("ingredient")), ~ recode(., !!!ingredients)) %>% 
  mutate(ingredient1 = if_else(drink == "Ziemes Martini Apfelsaft", "vermouth - dry", ingredient1),
         ingredient2 = if_else(drink == "Addison", "vermouth - sweet", ingredient2),
         ingredient2 = if_else(drink == "Archbishop", "wine - green ginger", ingredient2),
         ingredient2 = if_else(drink == "Berry Deadly", "wine - boones strawberry", ingredient2),
         ingredient2 = if_else(drink == "Thriller", "wine - green ginger", ingredient2),
         ingredient2 = if_else(drink == "Whisky Mac", "wine - green ginger", ingredient2),
         ingredient3 = if_else(drink == "Clove Cocktail", "wine - muscat", ingredient3))

ingredients <-
  cocktails %>% 
  select(starts_with("ingredient")) %>% 
  unlist(use.names = FALSE) %>% 
  na.omit() %>% 
  unique() %>% 
  sort()

ingredients %>% 
  enframe(name = NULL, value = "ingredient") %>%
  write_csv(here("ingredients.csv"))

cocktails <-
  cocktails %>% 
  filter(alcoholic == "Alcoholic")

# ggplot(cocktails, aes(x = fct_infreq(alcoholic))) + geom_bar()
ggplot(cocktails, aes(x = fct_infreq(category))) + geom_bar()
ggplot(cocktails, aes(x = fct_infreq(glass))) + geom_bar()
ggplot(cocktails, aes(x = fct_infreq(iba))) + geom_bar()
ggplot(cocktails, aes(x = fct_infreq(ingredient1))) + geom_bar()
ggplot(cocktails, aes(x = fct_infreq(ingredient2))) + geom_bar()
ggplot(cocktails, aes(x = fct_infreq(ingredient3))) + geom_bar()
ggplot(cocktails, aes(x = fct_infreq(ingredient4))) + geom_bar()
ggplot(cocktails, aes(x = fct_infreq(ingredient5))) + geom_bar()
ggplot(cocktails, aes(x = fct_infreq(ingredient6))) + geom_bar()


cocktails %>% 
  count(ingredient1, sort = TRUE)

cocktails %>% 
  count(ingredient2, sort = TRUE)

cocktails %>% 
  count(ingredient3, sort = TRUE)

cocktails %>% 
  select(ingredient1, ingredient2, ingredient3) %>% 
  unlist(use.names = FALSE) %>% 
  na.omit() %>%
  enframe(name = NULL, value = "ingredient") %>% 
  count(ingredient, sort = TRUE)

cocktails %>% 
  select(starts_with("ingredient")) %>% 
  unlist(use.names = FALSE) %>% 
  na.omit() %>%
  enframe(name = NULL, value = "ingredient") %>% 
  count(ingredient, sort = TRUE) %>% 
  select(ingredient, n)

  pull(ingredient) %>% 
  .[1:50]

cocktails %>% 
  filter(ingredient1 == "lemon juice") %>% 
  pull(instructions)


  
