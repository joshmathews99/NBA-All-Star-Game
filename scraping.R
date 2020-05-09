
library(tidyverse)
library(rvest)
library(purrr)

html <- read_html('https://www.espn.com/nba/game?gameId=401098037')
html %>% html_nodes('#shot127')
n <- html_nodes(html, c('#shot1', '#shot2', '#shot3', '#shot4', '#shot5', '#shot6', 
                        '#shot7', '#shot8', '#shot9', '#shot10', '#shot11', '#shot12', 
                        '#shot13', '#shot14', '#shot15', '#shot16', '#shot17', '#shot18', 
                        '#shot19', '#shot20', '#shot21', '#shot22', '#shot23', '#shot24', 
                        '#shot25', '#shot26', '#shot27', '#shot28', '#shot29', '#shot30', 
                        '#shot31', '#shot32', '#shot33', '#shot34', '#shot35', '#shot36', 
                        '#shot37', '#shot38', '#shot39', '#shot40', '#shot41', '#shot42', 
                        '#shot43', '#shot44', '#shot45', '#shot46', '#shot47', '#shot48', 
                        '#shot49', '#shot50', '#shot51', '#shot52', '#shot53', '#shot54', 
                        '#shot55', '#shot56', '#shot57', '#shot58', '#shot59', '#shot60', 
                        '#shot61', '#shot62', '#shot63', '#shot64', '#shot65', '#shot66', 
                        '#shot67', '#shot68', '#shot69', '#shot70', '#shot71', '#shot72', 
                        '#shot73', '#shot74', '#shot75', '#shot76', '#shot77', '#shot78', 
                        '#shot79', '#shot80', '#shot81', '#shot82', '#shot83', '#shot84', 
                        '#shot85', '#shot86', '#shot87', '#shot88', '#shot89', '#shot90', 
                        '#shot91', '#shot92', '#shot93', '#shot94', '#shot95', '#shot96', 
                        '#shot97', '#shot98', '#shot99', '#shot100', '#shot101', '#shot102')) %>% 
  map(xml_attrs) %>% 
  map_df(~as.list(.))
n
n$style

data %>% 
  filter(!is.na(locationX)) %>% 
  select(yearSeason, namePlayer, idEvent, locationX, locationY, isShotAttempted, isShotMade, numberPeriod, typeAction, typeShot)
data %>% 
  min(data$locationX)
ggplot(data %>% filter(yearSeason == 1998 & namePlayer == 'Karl Malone'), aes(locationX, locationY)) + geom_point()
data %>% filtermax(data$locationX)

data %>% 
  select(locationX) %>% 
  arrange(desc(locationX)) # range is -250 to 250
data %>% 
  select(locationY) %>% 
  arrange((locationY)) # range is -40 to 728 (normal range for halfcourt is -52 to 418)
data %>% 
  select(typeAction) %>% 
  distinct() # many different shot types, might be cool for comparisons
ggplot(data %>% filter(namePlayer == 'LeBron James') %>% filter(yearSeason == 2020) %>% filter, 
       aes(locationX, locationY)) + geom_point()
ggplot(data %>% filter(namePlayer == 'Giannis Antetokounmpo') %>% filter(yearSeason == 2020) %>% filter, 
       aes(locationX*(-1), locationY)) + geom_point()
# it appears as if the data from nbastatR has x axis shot locations that are
# inverted. correct orientation will require multiplying x axis values by -1.

# If converting 2019 scraped values to coordinates, then must first compare 2020
# values to align such that conversion factors become known.

html2020 <- read_html('https://www.espn.com/nba/game?gameId=401197813')
n <- html_nodes(html2020, '#shot37') %>% 
  map(xml_attrs) %>% 
  map_df(~as.list(.))
n
n$style

data %>% filter(yearSeason == 2020 & namePlayer == 'Jayson Tatum') %>% select(namePlayer, locationX, locationY, isShotMade) %>% 
  ggplot(aes(locationX*(-1), locationY)) + geom_point()
data %>% filter(yearSeason == 2020 & namePlayer == 'Jayson Tatum') %>% select(namePlayer, locationX, locationY, isShotMade) %>% slice(6)
# Need conversion to put out x = -174, y = 89.
n$style # Jayson: left: 14.444%, top: 16.0%. Top is x, left is y.
# -255.15 + 0.16(2*255.15) = -174
# -25 + 789*.14444 = 89

data %>% filter(yearSeason == 2020 & namePlayer == 'Giannis Antetokounmpo') %>% select(namePlayer, locationX, locationY, isShotMade) %>% 
  ggplot(aes(locationX*(-1), locationY)) + geom_point()
data %>% filter(yearSeason == 2020 & namePlayer == 'Giannis Antetokounmpo') %>% select(namePlayer, locationX, locationY, isShotMade) %>% 
  filter(locationY > 250)
n$style # Giannis: left: 63.555%, top: 46.0%. Must match x = 19, y = 263.
# 255.15 - 0.46*2*255.15 = 20
# -25 + 789*(1 - .63555) = 263

# Conversion Equations: 
# -255.15 + top/100 * 2 * 255.15 = x (for lhs or away team)
# -25 + 789 * left/100 = y (for lhs or away team)
# 255.15 - top/100 * 2 * 255.15 = x (for rhs or home team)
# -25 + 789 * (1 - left/100) = y (for rhs or home team)

html2019 <- read_html('https://www.espn.com/nba/game?gameId=401098037')
n <- html_nodes(html2019, '#shot1') %>% 
  map(xml_attrs) %>% 
  map_df(~as.list(.))
n
n$style

numbers <- tibble(number = c(1:150))
shots_2019 <- numbers %>% 
  mutate(shot_id = paste('#shot', number, sep = '')) %>% 
  mutate(scrape = map(shot_id, ~ html_nodes(html2019, .) %>% map(xml_attrs) %>% map_df(~as.list(.)))) %>% 
  unnest(cols = c(scrape))

# Columns in data dataset needed for shots: typeAction, typeShot, idEvent,
# numberPeriod, locationX, locationY, isShotMade.
# Full join by namePlayer. Can omit isShotAttempted variable. Have 273 entries.
shots_2019 <- shots_2019 %>% 
  select(id, `data-text`, class, `data-period`, style, `data-homeaway`) %>% 
  rename(idEvent = id)
shots_2019$idEvent <- gsub("[A-z]", "", shots_2019$idEvent) %>% as.numeric()
shots_2019
# idEvent set!
shots_2019 <- shots_2019 %>% mutate(class = ifelse(class == 'made', TRUE, FALSE)) %>% rename(isShotMade = class)
# isShotMade set!
shots_2019 <- shots_2019 %>% mutate(`data-period` = as.numeric(`data-period`)) %>% rename(numberPeriod = `data-period`)
# numberPeriod set!
shots_2019 <- shots_2019 %>% mutate(namePlayer = sub('* m.*', '', `data-text`)) %>% 
  mutate(namePlayer = replace(namePlayer, namePlayer == "Nikola Vucevic", "Nikola Vučević"),
         namePlayer = replace(namePlayer, namePlayer == "Nikola Jokic", "Nikola Jokić"),
         namePlayer = replace(namePlayer, namePlayer == "Joel Embiid blocks Kyrie Irving 's 2-foot two point shot", 'Kyrie Irving'),
         namePlayer = replace(namePlayer, namePlayer == "Kevin Durant blocks Kemba Walker 's 4-foot layup", 'Kemba Walker'),
         namePlayer = replace(namePlayer, namePlayer == "Kawhi Leonard blocks Blake Griffin 's 4-foot layup", 'Blake Griffin'),
         namePlayer = replace(namePlayer, namePlayer == "Kevin Durant blocks Joel Embiid 's 3-foot layup", 'Joel Embiid'),
         namePlayer = replace(namePlayer, namePlayer == "LeBron James blocks Joel Embiid's layup", 'Joel Embiid'),
         namePlayer = replace(namePlayer, namePlayer == "Kyrie Irving blocks Stephen Curry 's 4-foot driving layup", 'Stephen Curry'),
         namePlayer = replace(namePlayer, namePlayer == "LeBron James blocks Giannis Antetokounmpo 's 2-foot layup", 'Giannis Antetokounmpo'))
# namePlayer set!
data %>% select(typeShot) %>% distinct() # typeShot consists of 2PT Field Goal and 3PT Field Goal values
shots_2019$typeShot <- ifelse(grepl("three point|26-foot|25-foot", shots_2019$`data-text`),'3PT Field Goal','2PT Field Goal')
# typeShot set!
shots_2019$style
string <- "border-color:#000000;background-color:#000000;left:91.33333333333333%;top:50.0%;"
sub("[A-z]","", string)
sub('.*left:', '', string) %>% sub('%;top.*', '', .) %>% as.numeric() # gives left value
sub('.*top:', '', string) %>% sub('%;', '', .) %>% as.numeric() # gives top value
shots_2019 <- shots_2019 %>% mutate(top = sub('.*top:', '', shots_2019$style) %>% sub('%;', '', .) %>% as.numeric())
shots_2019 <- shots_2019 %>% mutate(left = sub('.*left:', '', shots_2019$style) %>% sub('%;top.*', '', .) %>% as.numeric())
# Conversion Equations: 
# -255.15 + top/100 * 2 * 255.15 = x (for lhs or away team)
# -25 + 789 * left/100 = y (for lhs or away team)
# 255.15 - top/100 * 2 * 255.15 = x (for rhs or home team)
# -25 + 789 * (1 - left/100) = y (for rhs or home team)
shots_2019 <- shots_2019 %>% 
  mutate(locationX = ifelse(`data-homeaway` == 'away', round(-255.15 + top * 2 * 2.5515, digits = 0), 
                            round(255.15 - top * 2 * 2.5515, digits = 0))) %>% 
  mutate(locationY = ifelse(`data-homeaway` == 'away', round(-25 + 7.89 * left, digits = 0), 
                            round(-25 + 789 * (1 - left/100), digits = 0)))
actions <- c('Jump Shot', 'Step Back Jump Shot', 'Pullup Jump Shot', 'Jump Shot', 'Driving Layup', 'Alley Oop Dunk Shot', 'Jump Shot', '2 Foot Shot', 'Alley Oop Dunk Shot', 'Jump Shot', '2 Foot Shot', 'Dunk', 'Jump Shot', '2 Point Shot', 'Driving Dunk', 'Alley Oop Dunk Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Step Back Jump Shot', 'Dunk', '10 Foot Shot', 'Jump Shot', 'Layup', 'Pullup Jump Shot', 'Dunk', '13 Foot Shot', 'Jump Shot', 'Layup', 'Layup', 'Driving Layup', 'Jump Shot', 'Dunk', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Alley Oop Dunk Shot', 'Jump Shot', 'Layup', 'Tip Shot', 'Jump Shot', '2 Foot Shot', 'Jump Shot', 'Jump Shot', 'Jump Bank Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Driving Dunk', 'Jump Shot', 'Alley Oop Dunk Shot', 'Jump Shot', 'Dunk', '2 Foot Shot', 'Tip Shot', 'Jump Shot', 'Tip Shot', 'Jump Shot', '9 Foot Shot', 'Jump Shot', 'Alley Oop Dunk Shot', 'Jump Shot', 'Jump Shot', 'Driving Layup', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Dunk', 'Jump Shot', 'Jump Shot', 'Step Back Jump Shot', 'Tip Shot', 'Driving Layup', '2 Point Shot', 'Jump Shot', 'Jump Shot', '6 Foot Shot', 'Alley Oop Dunk Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Layup', 'Jump Shot', 'Alley Oop Dunk Shot', 'Driving Layup', 'Jump Shot', 'Dunk', 'Hook Shot', 'Jump Shot', 'Dunk', 'Alley Oop Dunk Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', '2 Foot Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Dunk', 'Layup', 'Alley Oop Layup', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Driving Layup', 'Jump Shot', 'Layup', 'Dunk', 'Jump Shot', 'Jump Shot', 'Jump Shot','‘Dunk', 'Driving Layup', 'Alley Oop Dunk Shot', 'Dunk', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Driving Layup', 'Jump Shot', 'Jump Shot', 'Alley Oop Dunk Shot', 'Layup', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Alley Oop Dunk Shot', 'Jump Shot', 'Layup', 'Driving Layup', 'Jump Shot', 'Driving Dunk', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', '2 Point Shot', 'Jump Shot', 'Layup', 'Jump Shot', 'Alley Oop Dunk Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Dunk', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Layup', 'Tip Shot', 'Jump Shot', '8 Foot Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Driving Layup', 'Jump Shot', 'Layup', 'Jump Shot', 'Driving Layup', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Driving Layup', 'Jump Shot', 'Driving Layup', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', '2 Point Shot', '10 Foot Shot', 'Tip Shot', 'Jump Shot', 'Pullup Jump Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Dunk', 'Jump Shot', 'Jump Shot', 'Dunk', 'Jump Shot', 'Jump Shot', 'Layup', 'Jump Shot', '14 Foot Shot', 'Layup', 'Tip Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', '1 Foot Shot', 'Pullup Jump Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Tip Shot', '2 Point Shot', 'Dunk', 'Driving Layup', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Driving Dunk', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Driving Layup', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Alley Oop Dunk Shot', 'Layup', 'Jump Shot', 'Layup', 'Jump Shot', 'Jump Shot', 'Driving Layup', '2 Point Shot', '17 Foot Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', 'Jump Shot', '2 Point Shot', 'Jump Shot', 'Jump Shot', 'Dunk')
shots_2019 <- shots_2019 %>% mutate(typeAction = actions) %>% mutate(isShotAttempted = TRUE) %>% mutate(yearSeason = 2019)
add_2019 <- shots_2019 %>% select(namePlayer, yearSeason, typeAction, typeShot, idEvent, numberPeriod, locationX, locationY, isShotAttempted, isShotMade)
# Finish by adding this data to the existing data1951 dataset subsetted to
# include only games 1998 onwards with shot logs. Switch x coordinates by
# multiplying by -1.
