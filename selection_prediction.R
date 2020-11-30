# library(pdftools)
# 
# Sep_07 <- pdf_text("raw_data/B02Sep07.pdf") %>% 
#   readr::read_lines()
# Sep_07
# 
# goods <- Sep_07[c(6:31, 37:62, 68:83)] %>% 
#   str_squish() %>% 
#   strsplit(split = " ")
# 
# goods_2 <- goods[c(1, 27, 53)] %>% 
#   unlist()
# 
# goods_2[c(1:2, 14:15, 27:28)] <- "Circuit and Nature"
# goods_2[c(3:6, 16:19, 29:32)] <- "October 1, 2006"
# goods_2[c(11:13, 24:26, 37:39)]<- "Sepember 30, 2007"
# 
# str(goods_2)
# goods_2
# 
# goods_df <- plyr::ldply(goods[-c(1, 27, 53)])

roberts_circuit

roberts_model <- stan_glm(as.numeric(granted) ~ nature_of_proceeding + district + year,
         data = roberts_circuit,
         refresh = 0)

take_2 <- stan_glm(as.numeric(granted) ~ circuit_and_nature_of_proceeding + year - 1,
         data = roberts_circuit,
         refresh = 0)

print(roberts_model, digits = 3)
print(take_2, digits = 3)