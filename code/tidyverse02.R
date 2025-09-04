

# undergrad ---------------------------------------------------------------

pacman::p_load(tidyverse)

#point

g_point <- ggplot(data=iris, 
       mapping = aes(x=Sepal.Length,
                     y=Sepal.Width)) +
  geom_point()

#hot key for pipe == cmd+shift+m

#with pipe and color

g_point_color <- iris %>% 
  ggplot(aes(x = Sepal.Length,
                    y = Sepal.Width, 
                    color = Species)) +
  geom_point()
  

# ggplot exercise ---------------------------------------------------------

#draw a scatter plot of Petal.Width(x) and Petal.Length (y)
#assign to g_petal


#draw a boxplot between Species (x) and Petal.Length(y)
#fill the box with Species
#assign to 'g_petal_box'

#add a new layer of point, x=Species, y =Petal.Length



# other exercise ----------------------------------------------------------

df_mtcars <- as.tibble(mtcars)

#filter rows where cyl is 4

#select columns mpg, cyl, disp, wt, vs, carb

#select rows with 'cyl' greater than 4
#then select columns of mpg, cyl, disp, wt, vs, carb
#assign the output to df_sub
df_sub <- df_mtcars %>% 
  filter(cyl > 4) %>%
  select(mpg, cyl, disp, wt, vs, carb)


#type the following code and run (ROWNAMES ARE SUPPOSED TO BE NAMES OF CARS, BUT THEY AREN'T)
v_car <- rownames(df_mtcars)


#add a new column called "car" to 'df_mtcars'
#then reassign to df_mtcars
df_mtcars <- mutate(df_mtcars,
                    car = v_car)


#identify the lightest car ('wt) with cyl = 8
df_mtcars %>% 
  filter(cyl == 8) %>% 
  arrange(wt)


#calculate the average weight of cars within each group of gear numbers 
#consider using group_by and summarize
#assign to "df_mean"

df_mean <- df_mtcars %>% group_by(gear) %>% summarize(mean_wt = mean(wt))


#draw a figure between wt and qsec, but only those with cyl=6
df_mtcars %>% filter(cyl == 6) %>%
  ggplot(aes(x=wt, y=qsec)) +
  geom_point()

#draw a figure between mean and mean qsec for each group of gear
df_mtcars %>% group_by(gear) %>% summarize(mean_wt = mean(wt),
                                          mean_qsec = mean(qsec)) %>%
  ggplot(aes(x=mean_wt, y=mean_qsec, color=gear)) + geom_point()

