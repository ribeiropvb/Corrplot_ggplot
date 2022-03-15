
library(tidyverse)
library(corrplot)
library(faraway)

data(seatpos)

# Function to get correletions and p.values in a "long" data frame
corr.data = function(data) {
  
  # Get correlations
  cor.vals = cor(data)
  
  # Get p-values
  cor.p = cor.mtest(data, conf.level = 0.95)$p
  rownames(cor.p) = rownames(cor.vals)
  colnames(cor.p) = colnames(cor.vals)
  
  cbind(rowvars=rownames(cor.vals), data.frame(cor.vals)) %>% 
    gather(colvars, corr, -rowvars) %>% 
    left_join(cbind(rowvars=rownames(cor.p), data.frame(cor.p)) %>% 
                gather(colvars, p.value, -rowvars))
}
# basic correlation plot
corr.data(seatpos) %>% 
  ggplot(aes(colvars, fct_rev(rowvars))) +
  geom_tile(colour="grey70", fill=NA) +
  geom_text(aes(label=sprintf("%1.2f", corr)), position=position_nudge(y=0.2), 
            size=3, colour="grey20") +
  geom_text(aes(label=paste0("(",sprintf("%1.2f", p.value),")")), position=position_nudge(y=-0.2), 
            colour="grey20", size=2.5) +
  labs(x="",y="") +
  theme_classic() +
  coord_fixed()

# Inputing color range for corralation value

corr.data(seatpos) %>% 
  ggplot(aes(colvars, fct_rev(rowvars))) +
  geom_tile(colour="grey20", aes(fill=corr), size=0.5) +
  geom_text(aes(label=sprintf("%1.2f", corr)), position=position_nudge(y=0.2), 
            size=3, colour='black') +
  geom_text(aes(label=paste0("(",sprintf("%1.4f", p.value),")")), position=position_nudge(y=-0.2), 
            size=2.5, colour='black') +
  scale_fill_gradient2(low="red", mid="white", high="blue", midpoint=0, limits=c(-1,1)) +
  scale_size_continuous(range=c(8,12)) +
  labs(x="",y="") +
  theme_classic() +
  coord_fixed()

# Changing the color of the text
# depending on the correlation value

color <- ifelse(corr.data(seatpos)$corr>0.7,'white','black')

corr.data(seatpos) %>% 
  ggplot(aes(colvars, fct_rev(rowvars))) +
  geom_tile(colour="grey20", aes(fill=corr), size=0.5) +
  geom_text(aes(label=sprintf("%1.2f", corr)), position=position_nudge(y=0.2), 
            size=3, colour=color) +
  geom_text(aes(label=paste0("(",sprintf("%1.4f", p.value),")")), position=position_nudge(y=-0.2), 
            size=2.5, colour=color) +
  scale_fill_gradient2(low="red", mid="white", high="blue", midpoint=0, limits=c(-1,1)) +
  scale_size_continuous(range=c(8,12)) +
  labs(x="",y="") +
  theme_classic() +
  coord_fixed()

# Selecting a specific row

color <- ifelse(
  (corr.data(seatpos) %>% 
     filter(rowvars == 'Weight') %>% 
     .$corr)>0.5
  ,'white','black'
)

corr.data(seatpos) %>% 
  filter(rowvars == 'Weight') %>% 
  ggplot(aes(colvars, fct_rev(rowvars))) +
  geom_tile(colour="grey20", aes(fill=corr), size=0.5) +
  geom_text(aes(label=sprintf("%1.2f", corr)), position=position_nudge(y=0.2), 
            size=3, colour=color) +
  geom_text(aes(label=paste0("(",sprintf("%1.4f", p.value),")")), position=position_nudge(y=-0.2), 
            size=2.5, colour=color) +
  scale_fill_gradient2(low="red", mid="white", high="blue", midpoint=0, limits=c(-1,1)) +
  scale_size_continuous(range=c(8,12)) +
  labs(x="",y="") +
  theme_classic() +
  coord_fixed()

# Selecting more than one row

color <- ifelse(
  (corr.data(seatpos) %>% 
     filter(rowvars %in% c('Weight','Leg','Ht')) %>% 
     .$corr)>0.5
  ,'white','black'
)

corr.data(seatpos) %>% 
  filter(rowvars %in% c('Weight','Leg','Ht')) %>% 
  ggplot(aes(colvars, fct_rev(rowvars))) +
  geom_tile(colour="grey20", aes(fill=corr), size=0.5) +
  geom_text(aes(label=sprintf("%1.2f", corr)), position=position_nudge(y=0.2), 
            size=3, colour=color) +
  geom_text(aes(label=paste0("(",sprintf("%1.4f", p.value),")")), position=position_nudge(y=-0.2), 
            size=2.5, colour=color) +
  scale_fill_gradient2(low="red", mid="white", high="blue", midpoint=0, limits=c(-1,1)) +
  scale_size_continuous(range=c(8,12)) +
  labs(x="",y="") +
  theme_classic() +
  coord_fixed()

# Removing repeated values

cor_tibble <- corr.data(seatpos) %>%
  pivot_wider(
    id_cols = c(rowvars)
    , names_from = colvars
    , values_from = corr
  ) %>% as.data.frame() %>% 
  column_to_rownames(var="rowvars") %>% 
  as.matrix() %>% Matrix::tril() %>% 
  as.matrix() %>% 
  reshape2::melt() %>% 
  magrittr::set_colnames(c('rowvars','colvars','corr')) %>% 
  as_tibble()

pvalue_tibble <- corr.data(seatpos) %>%
  pivot_wider(
    id_cols = c(rowvars)
    , names_from = colvars
    , values_from = p.value
  ) %>% as.data.frame() %>% 
  column_to_rownames(var="rowvars") %>% 
  as.matrix() %>% Matrix::tril() %>% 
  as.matrix() %>% 
  reshape2::melt() %>% 
  magrittr::set_colnames(c('rowvars','colvars','p.value')) %>% 
  as_tibble()

corplot <- inner_join(
  cor_tibble, pvalue_tibble
  , by = c('rowvars','colvars')
) %>% mutate(
  color = ifelse(
    corr < 0.5
    , 'black'
    , 'white'
  ),
  color = ifelse(
    corr == 0
    , 'white'
    , color
  )
)

corplot %>% ggplot(aes(colvars, fct_rev(rowvars))) +
  geom_tile(colour="grey20", aes(fill=corr), size=0.5) +
  geom_text(aes(label=sprintf("%1.2f", corr)), position=position_nudge(y=0.2), 
            size=3, colour=corplot$color) +
  geom_text(aes(label=paste0("(",sprintf("%1.4f", p.value),")")), position=position_nudge(y=-0.2), 
            size=2.5, colour=corplot$color) +
  scale_fill_gradient2(low="red", mid="white", high="blue", midpoint=0, limits=c(-1,1)) +
  scale_size_continuous(range=c(8,12)) +
  labs(x="",y="") +
  theme_classic() +
  coord_fixed()

