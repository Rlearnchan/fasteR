# melon

# 1. setting

library(tidyverse)
library(rstudioapi)
library(RSelenium)
library(rvest)
library(openxlsx)

# 2. selenium

TERM_COMMAND <- 'java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4445'
terminalExecute(command = TERM_COMMAND) # 개인 컴퓨터에서 잘 안 되는 경우 java 설치, 방화벽 개인/공용 설정 권장

REMDR = remoteDriver(port = 4445, browserName = 'chrome')
REMDR$open() # 크롬 오픈

# 3. crawling

PERIOD <- 2000:2021

MASTERPIECE <- tibble()

for (i in seq_along(PERIOD)) {
  
  URL <- paste0('https://www.melon.com/chart/age/index.htm',
                '?chartType=YE',
                '&chartGenre=KPOP',
                '&chartDate=', PERIOD[i])
  
  REMDR$navigate(URL)
  
  temp1 <- REMDR$getPageSource()
  
  TITLE <- read_html(temp1[[1]]) %>%
    html_elements('form#frm') %>%
    html_elements('div.wrap_song_info') %>%
    html_elements('div.ellipsis.rank01') %>%
    html_text() %>%
    str_remove_all('\n') %>%
    str_remove_all('\t') %>%
    str_sub(end = -2)
  
  SINGER <- read_html(temp1[[1]]) %>%
    html_elements('form#frm') %>%
    html_elements('div.wrap_song_info') %>%
    html_elements('div.ellipsis.rank02') %>%
    html_text() %>%
    str_sub(end = nchar(.)/2)
  
  ALBUM <- read_html(temp1[[1]]) %>%
    html_elements('form#frm') %>%
    html_elements('div.wrap_song_info') %>%
    html_elements('div.ellipsis.rank03') %>%
    html_text() %>%
    str_remove_all('\n') %>%
    str_remove_all('\t')
  
  LIKE <- read_html(temp1[[1]]) %>%
    html_elements('button.btn_icon.like') %>%
    html_text() %>%
    str_remove_all('좋아요\n\n총건수\n') %>%
    str_remove_all(',') %>%
    as.integer()
  
  temp2 <- tibble(YEAR = PERIOD[i], RANK = 1:100, 
                  TITLE, SINGER, ALBUM, LIKE)
  
  MASTERPIECE <- rbind(MASTERPIECE, temp2)
  
  Sys.sleep(5 + rnorm(1))
  
}

# 4. export(save)

write.xlsx(MASTERPIECE, '멜론차트.xlsx')

# 5. import

MELON <- read.xlsx('멜론차트.xlsx') %>% tibble()

# 6. analysis

# 6-1. TOP100 수록곡이 가장 많은 가수는?

SUPERSTAR <- MELON %>%
  distinct(TITLE, .keep_all = TRUE) %>%
  group_by(SINGER) %>%
  summarise(NUMBER = n()) %>%
  arrange(desc(NUMBER))

SUPERSTAR

# 6-2. 아이유의 TOP100 수록곡은?

DLWLRMA <- MELON %>%
  filter(SINGER %in% c('아이유', 'IU')) %>%
  arrange(desc(LIKE), RANK) %>%
  distinct(TITLE, .keep_all = TRUE)

DLWLRMA

# 6-3. 연도별 아이유 LIKE 총량은?

UAENA <- MELON %>%
  filter(SINGER %in% c('아이유', 'IU')) %>%
  group_by(YEAR) %>%
  summarise(HEART = sum(LIKE))

UAENA

# 6-4. 아이유의 성장은 계속될까?

UAENA_GRAPH <- UAENA %>%
  ggplot(aes(x = YEAR, y = log(HEART))) +
  geom_point() +
  geom_smooth(formula = y ~ poly(x, 3), method = 'lm')

UAENA_GRAPH

