# for R and GNU/Linux and UNIX course
options(stringsAsFactors = FALSE,
        scipen = 9999)

# set libraries
if(!require("pacman"))
  install.packages("pacman", dependencies = TRUE)
library(pacman)

libraries <- c("dplyr",
               "ggplot2",
               "stringr")

pacman::p_load(libraries, character.only = TRUE)

rm(libraries)

# import
# which_data <- base::readline(prompt = "Which file to import?\n")

data <- dplyr::as_tibble(utils::read.csv("never_use_4R.csv")) %>% 
  dplyr::select(-Date) %>% 
  dplyr::select_if(sapply(., function(x) !all(is.na(x)))) %>% 
  dplyr::mutate_at(vars(contains("Age")), ~ str_replace(., ",", ".")) %>% 
  utils::type.convert(as.is = TRUE) %>% 
  dplyr::mutate(age_new = ifelse(Age..or.some.other.number. > 100, 
                                 Age..or.some.other.number. / 10, 
                                 Age..or.some.other.number.),
         Outcome_new = ifelse(Outcome == "aliv", "alive", Outcome))

saveRDS(data,
         "cleaned_data.rds")

gg1 <- data %>% 
  ggplot(aes(x = Outcome_new,
             y = age_new)) +
  stat_summary(geom = "bar",
              fun = mean) +
  stat_summary(fun.min = function(x) mean(x, na.rm = T) - sd(x, na.rm = T),
               fun.max = function(x) mean(x, na.rm = T) + sd(x, na.rm = T),
               geom = "errorbar",
               width = 0.2) +
  stat_summary(geom = "text",
               fun.data = function(x) c(y = max(x) + 5, label = length(x))) +
  geom_point() +
  theme_minimal()

ggsave("barplot.jpg")
  
