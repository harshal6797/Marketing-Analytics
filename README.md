# The Dataset

The data set “hiphop” contains results from a study conducted by a linguist at the University of Minnesota. The researcher was interested in predicting musical taste based on familiarity with African American English (AAE). 168 subjects participated in the study, and each was asked to define 64 different AAE terms. The definitions given were used to create a “familiarity” score for each subject for each term. This score quantifies how well the subject knew the term on a scale of 1-5 (1 = not at all, 5 = very well). Before tackling the problems, study the information on the following website, which includes a description of each variable:

http://conservancy.umn.edu/bitstream/handle/11299/116327/5/explanationAAEHiphopChesley.txt

# Libraries
```
library(tidyverse)
library(here)
library(ggplot2)
```

# Reading Data
- Create a "Data" Variable in your project directory and save your dataset there before running this line
```
hiphop_data <- read_csv(here("Data", "hiphop.csv"))
```

# Data Wrangling and Exploration

- Factoring categorical variables
```
hiphop_data <- hiphop_data %>%
  mutate(
    sex = as.factor(sex),
    ethnic = as.factor(ethnic))
```

- Unique AAVE words studied in this dataset
```
length(unique(hiphop_data[["word"]]))
```

- Recategoriging ethnic into only two groups, “white” and “non-white”, to simplify data
```
hiphop_data <- hiphop_data %>%
  mutate(
   ethnicity = ifelse(ethnic == "white", "white", "non-white")
  )
 ````
 
 - Studying Demographics and summarizing our findings
 ```
 hiphop_demo <- hiphop_data %>%
  select(sex, subj, age, ethnic)

hiphop_demo1 <- unique(hiphop_demo)
summary(hiphop_demo1)
```

- Plots to display the demographic information of the subjects in this study
```
hiphop_data %>%
  ggplot(aes(x = age, fill = sex)) + geom_histogram(position = "dodge", alpha=0.8) + labs(x = "Age", y = "Count of males and females") + ggtitle("Count of  Males and Females with respect to their age")
```
```
hiphop_data %>%
  ggplot(aes(x= sex, y = age, fill = ethnicity)) + geom_boxplot() + labs(x = "Sex", y = "Age") + ggtitle("Distribution of males and females accross their ethnicity with respect to their age")
```

# Finding Familiar Words
- Determining which word(s) in this study was the most and least familiar on average for each demographic group
```
FamiliarWords <- hiphop_data %>%
  select(word, age, familiarity) %>%
  filter(age < 20) %>%
  group_by(word) %>%
  summarize(mean_familiarity = mean(familiarity))
```
```
FamiliarWords %>% top_n(1)
```
```
FamiliarWords %>% top_n(-1)
```
```
FamiliarWords <- hiphop_data %>%
  select(word, sex, ethnicity, familiarity) %>%
  filter(sex == "Female", ethnicity == "non-white") %>%
  group_by(word) %>%
  summarize(mean_familiarity = mean(familiarity))
  
FamiliarWords %>% top_n(1) # Most popular word for non-white women
FamiliarWords %>% top_n(-1) # Least popular word for non-white women
```
```
FamiliarWords <- hiphop_data %>%
  select(word, sex, age, ethnicity, familiarity) %>%
  filter(sex == "Male", ethnicity == "white", age > 30) %>%
  group_by(word) %>%
  summarize(mean_familiarity = mean(familiarity))
  FamiliarWords
  
FamiliarWords %>% top_n(1)
FamiliarWords %>% top_n(-1)
```

- Determining which music genre most differentiates the groups for each demographic. That is, which genre had much higher average (mean or median) score in one group than the other
```
hiphop_data1 <- hiphop_data %>%
  select(intl:unclassifiable, sex) %>%
  group_by(sex) %>%
  summarise(across(intl:unclassifiable, list(mean)))

hiphop_data1 <- setNames(data.frame(t(hiphop_data1[ , - 1])), hiphop_data1[ , 1])
colnames(hiphop_data1) = c("Female", "Male")

hiphop_data1 %>%
  mutate(Difference = Female - Male) %>%
  arrange(hiphop_data1)
```
 
```
hiphop_data1 <- hiphop_data %>%
  select(intl:unclassifiable, ethnicity) %>%
  group_by(ethnicity) %>%
  summarise(across(intl:unclassifiable, list(mean)))

hiphop_data1 <- setNames(data.frame(t(hiphop_data1[ , - 1])), hiphop_data1[ , 1])
colnames(hiphop_data1) = c("non_white", "white")

hiphop_data1 %>%
  mutate(Difference = non_white - white) %>%
  arrange(hiphop_data1)
```

```
hiphop_data1 <- hiphop_data %>%
  select(intl:unclassifiable, age) %>%
  mutate(age_group = case_when(age < 21 ~ "below 21", age >= 21 ~ "above 21")) %>%
  group_by(age_group) %>%
  summarise(across(intl:unclassifiable, mean))

hiphop_data1 <- setNames(data.frame(t(hiphop_data1[ , - 1])), hiphop_data1[ , 1])
colnames(hiphop_data1) = c("above 21", "below 21")

hiphop_data1 %>%
  mutate(Difference =  `above 21` - `below 21`) %>%
  arrange(hiphop_data1)
```

- Finding words which could be used and avoided in a song which will be percieved as authentically hiphop. Lyrics will be recognizeable to those who describe themselves as hiphop fans, but less recognizeable to those who do not consider themselves fans

```
A_data <- hiphop_data %>%
  select(word, hiphop, familiarity)

summary(A_data)
```
Defining fans and non-fans
```
A_data <- A_data %>%
  mutate(hiphop = ifelse(hiphop > 3, "fans", "non-fans")) %>%
  group_by(hiphop, word) %>%
  summarize(mean_familiarity= mean(familiarity))

fans<- A_data %>%
  filter(hiphop == "fans", mean_familiarity >= 3)
fans

non_fans <- A_data %>%
  filter(hiphop == "non-fans", mean_familiarity >= 3)
non_fans

anti_join(fans, non_fans, by = "word")
```

- Finding which title among “Hotline Boo” or “Hella Bling” for an album would appeal more to larger chunk of population
```
summary(hiphop_data$county)
```
```
Album_Title <- hiphop_data %>%
  mutate(county = cut(county, breaks = c(0,99905,331582,3053793), labels=c('small', 'medium', 'large'))) %>%
  select(fam1, word, subj, county) %>%
  filter(word == c('boo','hella')) %>%
  group_by(word, county) %>%
  summarize(population_appeal = sum(fam1)) %>%
  drop_na()

Album_Title %>%
  ggplot(aes(x=word, y=population_appeal)) + geom_col(aes(fill=county), position = 'dodge') + labs(x = "Word", y = "Appeal") + ggtitle('Word appeal with respect to population size')
```
 
