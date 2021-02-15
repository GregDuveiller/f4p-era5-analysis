
### PROTOSCRIPT TO REPROCESS THE DATA... based on those harvestd in the JEODPP

## STEPS TO ADD>>>

df_comb <- df_comb %>% 
  mutate(time = as.Date(x = paste(year, monthS, '15', sep = '-')))
# NOTE: This time is indicative, and based on the Northern Hemisphere


df_dum <- df_obs %>% 
  select(x, y, year, month, matches(varDFname_obs)) %>%
  # need to rotate cold months in Southern Hemisphere
  mutate(monthS = ifelse(sign(y) < 0, (month + 6) %% 12, month))  %>%
  mutate(monthS = ifelse(monthS == 0, 12, monthS)) %>% 
  left_join(df_sim %>%
              select(x, y, year, month, matches(varDFname_sim)) %>%
              # need to rotate cold months in Southern Hemisphere
              mutate(monthS = ifelse(sign(y) < 0, (month + 6) %% 12, month),
                     monthS = ifelse(monthS == 0, 12, monthS)), 
            by = c("x", "y", "month", "monthS", "year")) %>% 
  rename(obs = matches(varDFname_obs), 
         sim = matches(varDFname_sim)) 
df_comb <- bind_rows(df_comb, df_dum) 