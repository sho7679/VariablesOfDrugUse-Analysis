---
  title: "Variables of Cocaine Usage"
output: html_document
---
  
  # importing data and libraries 
  ```{r}
library(Hmisc) ##may need to install these packages first
library("SASxport")
library(tidyverse)
library(plyr)
library(plotly) 
library(reshape2)
drugs <- read.xport("DUQ_I.XPT")
demographics <- read.xport("DEMO_I.XPT")
demographics
```

# Labelling column variables 

```{r}
drugs2<- drugs %>% rename(
  "last time you used cocaine/unit" = DUQ270U,
  "number of times you used cocaine" = DUQ272,
  "ID" = SEQN
) %>% select("last time you used cocaine/unit","number of times you used cocaine", "ID")

demographics2 <- demographics %>% rename(
  "Annual Household Income" = INDHHIN2,
  "Citizenship Status" = DMDCITZN,
  "Education level - Children/Youth 6-19" = DMDEDUC3,
  "Education level - Adults 20+" = DMDEDUC2,
  "Race/Hispanic origin with NH Asian" = RIDRETH3,
  "ID" = SEQN, 
  "Age" = RIDAGEYR, 
  "Gender" = RIAGENDR
) %>% select("Annual Household Income", "Age", "Citizenship Status", "Education level - Children/Youth 6-19", "Education level - Adults 20+", "Race/Hispanic origin with NH Asian", "ID", "Gender" )

```


# Labelling the data factor levels 
```{r}
# merge dataset for respondents of Drug use 
df<-inner_join(drugs2, demographics2, by = "ID")

# make new dataset for factor levels 
df1 <-df

# change to factor levels 
df1$`Citizenship Status`<- as.factor(df$`Citizenship Status`)
df1$`Education level - Children/Youth 6-19` <- as.factor(df$`Education level - Children/Youth 6-19`)
df1$`Education level - Adults 20+` <- as.factor(df$`Education level - Adults 20+`)
df1$`Race/Hispanic origin with NH Asian` <- as.factor(df$`Race/Hispanic origin with NH Asian`)
df1$`Annual Household Income` <-as.factor(df$`Race/Hispanic origin with NH Asian`)
df1$`last time you used cocaine/unit` <- as.factor(df1$`last time you used cocaine/unit`)
df1$`number of times you used cocaine` <- as.factor(df1$`number of times you used cocaine`) 
df1$`Annual Household Income`<- as.factor(df1$`Annual Household Income`)
df1$Gender <-as.factor(df1$Gender)

levels(df1$`Annual Household Income`)

# rename factor levels 

df1$`Citizenship Status` <- mapvalues(df$`Citizenship Status`, from = c("1", "2", "7", "9"), to = c("citizen", "not citizen", "other", "other"))

df1$`Education level - Children/Youth 6-19` <- mapvalues(df$`Education level - Children/Youth 6-19`, from = c("9",  "10", "11", "12", "13", "14", "15", "66"), to = c("9", "10", "11", "12", "high school", "GED", "more than high school", "<9"))

df1$`Education level - Adults 20+` <- mapvalues(df$`Education level - Adults 20+`, from = c("1", "2", "3", "4", "5"), to = c("<9", "9-11", "high school", "some college", "college graduate"))

df1$`last time you used cocaine/unit` <- mapvalues(df1$`last time you used cocaine/unit`, from = c("1", "2", "3", "4", "7", "9"), to = c("Days", "Weeks", "Months", "Years", "other", "other"))

df1$`number of times you used cocaine`<- mapvalues(df1$`number of times you used cocaine`, from = c("1", "2", "3", "4", "5", "6"), to = c("Once", "2-5 times", "6-19 times", "20-49 items", "50-99 times", "100 times or more"))

df1$`Race/Hispanic origin with NH Asian` <- mapvalues(df1$`Race/Hispanic origin with NH Asian`, from = c("1", "2", "3", "4", "6", "7"), to = c("Mexian American", "Other Hispanic", "Non-Hispanic White", "Non-Hispanic Black", "Non-Hispanic Asian", "Other Race/MultiRacial"))

df1$`Annual Household Income` <- mapvalues(df1$`Annual Household Income`, from = c("1", "2", "3", "4", "6", "7"), to = c("0-4999", "5000-9999", "10,000-14,999", "15,000-19,999", "25,000-34,999", "35,000-44,999"))

df1$Gender <- mapvalues(df1$Gender, from = c("1", "2"), to = c("Male", "Female"))

df1

```

```{r}
# remove the NAs
df2 <-df1


df2$'Race' <- as.character(df2$`Race/Hispanic origin with NH Asian`)
df2$'Citizen Status' <- as.character(df2$`Citizenship Status`)
df2$'Use' <- as.character(df2$`number of times you used cocaine`)
df2$'lastuse' <- as.character(df2$`last time you used cocaine/unit`)



df2 <- df2[!is.na(df2$`Use`) & !is.na(df2$`Citizen Status`), ]
df2
```
# age vs. last time you used cocaine
```{r}
#p1<-plot_ly(df2, x = ~lastuse, y=~Age) %>% add_boxplot()

plot_ly(df2, x = ~lastuse, y = ~Age, type = "box") %>%
  layout(title = "Last time used cocaine by age", x_title = "f")

```


# Question 2 
How does cocaine use vary with citizenship status and race (sample population of those that used it within the past year)? What about those who refused to answer questions about cocaine? 
  
  
  ```{r}
p <- ggplot(df2, aes(`Race`, fill = `Use`)) +
  geom_bar(position = "fill") 

p %>%
  ggplotly(layerData = 1, originalData = FALSE) %>%
  plotly_data()


ggplotly(p, originalData = FALSE) %>%
  mutate(ydiff = ymax - ymin) %>% 
  add_text(
    x = ~x, y = ~(ymin + ymax) / 2,
    text =  ~ifelse(ydiff > 0.02, round(ydiff, 2), ""),
    showlegend = FALSE, hoverinfo = "none",
    color = I("black"), size = I(9)
  )
```

```{r}
p <- ggplot(df2, aes(`Citizen Status`, fill = `Use`)) +
  geom_bar(position = "fill") +
  ylab("Proportion")

p %>%
  ggplotly(layerData = 1, originalData = FALSE) %>%
  plotly_data()


ggplotly(p, originalData = FALSE) %>%
  mutate(ydiff = ymax - ymin) %>% 
  add_text(
    x = ~x, y = ~(ymin + ymax) / 2,
    text = ~ifelse(ydiff > 0.02, round(ydiff, 2), ""),
    showlegend = FALSE, hoverinfo = "none",
    color = I("black"), size = I(9)
  )
```