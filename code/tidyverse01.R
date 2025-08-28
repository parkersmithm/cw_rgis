if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse)


set.seed(123)

iris_sub <- as_tibble(iris) %>% 
  group_by(Species) %>% 
  sample_n(3) %>% 
  ungroup()

print(iris_sub)


#8/28/25 - swirl-tidy
library(swirl)
install_course_github("sysilviakim", "swirl-tidy")
swirl()


# whoa- section label (ctl+shift+r) ---------------------------------------

#exercise 1: filter "iris_sub" to only contain virginica

filter(iris_sub, Species == "virginica")

#exercise 2: select "Sepal.Width" in 'iris_sub

select(iris_sub, Sepal.Width)