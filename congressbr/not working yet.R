
# https://danilofreire.github.io/pols1600/

# https://github.com/RobertMyles/Various_R-Scripts
# https://www.robertmylesmcdonnell.com/content/posts/tips-and-tricks-for-r-markdown-html/
# https://www.robertmylesmcdonnell.com/content/posts/Bayesian-IRT-in-R-and-Stan/


readline()
parse()


# devtools::install_github("RobertMyles/flag_fillr")


library(flagfillr)
library(rnaturalearth)
library(dplyr)
library(stringi)

br <- ne_states(country = "Brazil", returnclass = "sf")
trade <- tibble(
  country = "Brazil",
  state = br$name,
  partner = NA_character_) %>% 
  mutate(state = stri_trans_general(state, "Latin-ASCII"),
         partner = case_when(
           state == "Acre" ~ "Peru",
           state == "Alagoas" ~ "China",
           state == "Amapa" ~ "United States of America",
           state == "Amazonas" ~ "Argentina",
           state == "Bahia" ~ "China",
           state == "Ceara" ~ "United States of America",
           state == "Distrito Federal" ~ "China",
           state == "Espirito Santo" ~ "United States of America",
           state == "Goias" ~ "China",
           state == "Maranhao" ~ "Canada",
           state == "Mato Grosso" ~ "China",
           state == "Mato Grosso do Sul" ~ "China",
           state == "Minas Gerais" ~ "China",
           state == "Para" ~ "China",
           state == "Paraiba" ~ "United States of America",
           state == "Parana" ~ "China",
           state == "Pernambuco" ~ "Argentina",
           state == "Piaui" ~ "China",
           state == "Rio de Janeiro" ~ "China",
           state == "Rio Grande do Norte" ~ "United States of America",
           state == "Rio Grande do Sul" ~ "China",
           state == "Rondonia" ~ "Hong Kong",
           state == "Roraima" ~ "Venezuela",
           state == "Santa Catarina" ~ "United States of America",
           state == "Sao Paulo" ~ "United States of America",
           state == "Sergipe" ~ "Netherlands",
           TRUE ~ "China"
         ))

flag_fillr_data(trade, country = "Brazil", partner_col = trade$partner, 
                state_col = trade$state, type = "state", 
                resolution = "large", size = "250")