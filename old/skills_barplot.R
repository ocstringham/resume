library(ggplot2)
library(tidyr)
library(dplyr)



skills_plot = function(df, font = 10){
  
  df = df %>% mutate(p2 = 100 - p) %>% 
    pivot_longer(-skill) %>% 
    mutate(skill = factor(skill, levels = rev(unique(skill))))
  
  p = 
  ggplot(df, aes(x = value, 
                 group = name, fill = name, 
                 y = skill)) + 
    geom_bar(stat='identity', color = NA, size = 1,
             width = 0.9,
             position =  position_stack(reverse=TRUE)) +
    scale_fill_manual(values = c("#0B7FAB", "gray80"), guide = "none") + 
    scale_x_continuous(limits = c(0,100)) + 
    # geom_label(aes(label = skill)) + 
    annotate("text", hjust = 0 , x = 3, y = nrow(df)/2, size = font, 
             label = df$skill[1], color = "gray80") +
    annotate("text", hjust = 0 , x = 3, y = nrow(df)/2 - 1, size = font, 
             label = df$skill[3], color = "gray80") +
    annotate("text", hjust = 0 , x = 3, y = nrow(df)/2 - 2, size = font, 
             label = df$skill[5], color = "gray80") +
    {if(nrow(df) == 8) annotate("text", hjust = 0 , x = 3, y = nrow(df)/2 - 3, size = font, 
                                label = df$skill[7], color = "gray80") } +
    theme_void() + 
    theme(panel.background = element_rect(fill = "transparent", colour = NA),  
          plot.background = element_rect(fill = "transparent", colour = NA))
  
  return(p)
  
}


# langugages
df = tribble(~skill, ~p, 
             "R", 90, 
             "Python", 82,
             "SQL", 60
             ) 

df$skill
df

skills_plot(df)

saveRDS(last_plot(), 'skills_bar.rds')  
ggsave(plot = last_plot(), filename = "skills_bar.png", dpi = 300,
       height = 2, width = 4, units = "in")




# Skills

# ML, text class
# GIS
# Web scraping
# 

df2 =  tribble(~skill, ~p, 
               "GIS", 95, 
               "Machine Learning", 83,
               "Web Scraping", 95,
               "Data Visualization", 90
               
) 

skills_plot(df2, 9)

saveRDS(last_plot(), 'skills_bar2.rds')  
ggsave(plot = last_plot(), filename = "skills_bar2.png", dpi = 300,
       height = 3, width = 4, units = "in")

