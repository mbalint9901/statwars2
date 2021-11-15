library(tidyverse)

teacher_df <- readxl::read_excel("3. forduló STAT WARS UNI.xlsx", sheet = 2)

teacher_df %>% 
  mutate(
    nem = case_when(
      nem == 1 ~ "Férfi",
      nem == 2 ~ "Nő"
    ),
    eletkor = as.integer(eletkor),
    iskvegz = case_when(
      iskvegz == 1 ~ "Legfeljebb érettségi",
      iskvegz == 2 ~ "Főiskola/Bsc",
      iskvegz == 3 ~ "Egyetem/Msc"
    )
  )
