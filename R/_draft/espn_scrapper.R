library(tidyverse)
library(httr)
library(jsonlite)
library(rvest)
library(janitor)

resp <- GET(
    url = "https://fantasy.espn.com/apis/v3/games/ffl/seasons/2020/segments/0/leaguedefaults/3?view=kona_player_info",
    add_headers(
      "User-Agent"='Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:84.0) Gecko/20100101 Firefox/84.0',
      "Accept"='application/json',
      "Accept-Language"='pt-BR,pt;q=0.8,en-US;q=0.5,en;q=0.3',
      "X-Fantasy-Source"='kona',
      "X-Fantasy-Filter"='{"players":{"filterStatsForSplitTypeIds":{"value":[1]},"filterSlotIds":{"value":[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,23,24]},"filterStatsForSourceIds":{"value":[1]},"sortAppliedStatTotal":{"sortAsc":false,"sortPriority":2,"value":"11202016"},"sortDraftRanks":{"sortPriority":3,"sortAsc":true,"value":"PPR"},"sortPercOwned":{"sortPriority":4,"sortAsc":false},"limit":1000,"offset":0,"filterRanksForScoringPeriodIds":{"value":[16]},"filterRanksForRankTypes":{"value":["PPR"]},"filterRanksForSlotIds":{"value":[0,2,4,6,17,16]},"filterStatsForTopScoringPeriodIds":{"value":2,"additionalValue":["002020","102020","002019","11202016","022020"]}}}',
      "X-Fantasy-Platform"='kona-PROD-a5abcf16cafe5c335041277beeafcabc2a236402',
      "Connection"='keep-alive',
      "Referer"='https://fantasy.espn.com/football/players/projections',
      "Cookie"='SWID=37e37eae-2d24-4fed-bacd-c7ad659047d6; AMCV_EE0201AC512D2BE80A490D4C%40AdobeOrg=-330454231%7CMCIDTS%7C18623%7CMCMID%7C56700191024045287203545623779762289455%7CMCAID%7CNONE%7CvVersion%7C3.1.2%7CMCAAMLH-1609614042%7C4%7CMCAAMB-1609614042%7Cj8Odv6LonN4r3an7LhD3WZrU1bUpAkFkkiY1ncBR96t2PTI%7CMCOPTOUT-1609016442s%7CNONE; s_ecid=MCMID%7C56151651865291846460420886922936625506; _cb_ls=1; _cb=B0S2h-BZd_53Clatu3; _chartbeat2=.1600170473205.1609009242379.0000000000000001.DMXM9cBbSlgwc7GE1DQ00UZdDuTt.1; trc_cookie_storage=taboola%2520global%253Auser-id%3De1837a82-9969-4f34-92c6-c71129d35b08-tuct65a2f70; _v__chartbeat3=DdmsiXBHozojCVTlwd; s_pers=%20s_c24%3D1600343007505%7C1694951007505%3B%20s_c24_s%3DFirst%2520Visit%7C1600344807505%3B%20s_gpv_pn%3Dwww.espn.com%253Ahttp%253A%252F%252Fwww.espn.com%252Fapis%252Fdevcenter%252Fblog%252Fread%252Fpublicretirement.html%7C1600344807511%3B; s_vi=[CS]v1|2FB1A5610515D1F0-6000066F67E70F84[CE]; s_c24=1609009257102; mbox=session#206a65bf0d394d90a7537ed437707307#1607025769|PC#3f8467d6cf34497a8a9bd2081d342031.34_0#1670268709; _fbp=fb.1.1607023911596.1618534971; __gads=ID=257eb8513971b68c-2233b8a574b80065:T=1607023936:S=ALNI_MZDCDaTUvB3QOzNEunFW1rnZ0de3A; region=unknown; _dcf=1; IR_gbd=espn.com; IR_9070=1609009241895%7C0%7C1609009241895%7C%7C; _uetsid=a68f04b047ac11ebb57d0d3dea5006c8; _uetvid=a68f42e047ac11eb8b31994d8b02b94d; _cb_svref=null; AMCVS_EE0201AC512D2BE80A490D4C%40AdobeOrg=1; s_c24_s=More%20than%207%20days; s_gpv_pn=fantasy%3Afootball%3Aleague%3Atoolsprojections; s_c6=1609009243039-New; s_cc=true; country=br; country=br; cto_bundle=DJRCDV9ZTlpFd0RqUjclMkZJcFkzRHZZUW9lOVJFdVhwRXBSM0RnRlhPbkgweSUyQjNsM0xmQm9GRXpYanl1RFVTQ0ZuQmF1dzFwZmRHTE02ZTJ4WndXM1F3cHZFa2Rpb1RjRThXdGpudjV1RmxkczNMcjlpY3I1TSUyRmZFM1JSR1BHcUpzY3ZndVlmbTNLeXZCZGhKNXgxUkNRSkVtUGclM0QlM0Q; espnCreative138271411242=true; s_sq=wdgespcom%252Cwdgespge%3D%2526pid%253Dfantasy%25253Afootball%25253Aleague%25253Atoolsprojections%2526pidt%253D1%2526oid%253Dfunctionyr%252528%252529%25257B%25257D%2526oidt%253D2%2526ot%253DSUBMIT',
      "If-None-Match"='W/"086e5139f98124f365f03fc9ca34ad448"',
      "TE"='Trailers'
    )
  )
    
json <- content(resp,as="text") %>% 
  fromJSON()

x <- json$players$player %>%  
  as_tibble() %>% 
  select(active, fullName, injured, rankings, stats) 

y <- x[c(2,11,15),] %>% 
  unnest(stats) %>% 
  as_tibble() 

y$stats %>% 
  glimpse()

PASSING
0 - ATTEMPS
1 - COMPLETED
3 - YARDS
4 - tdS
? - INT

Rushing
23 - CAR
24 - yards
25 - TD



