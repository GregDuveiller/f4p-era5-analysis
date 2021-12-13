get_df_comb_lai <- function(target_var, 
                            xtrLbl_obs = NULL, xtrLbl_sim = NULL, 
                            src_obs, src_sim,
                            path_obs, path_sim, 
                            target_var_obs = NULL, target_var_sim = NULL,
                            spres = '025', out_path){
  
  require(dplyr)
  require(tidyr)
  
  dir.create(out_path, showWarnings = F, recursive = T)
  
  # in case target variable has an extra (more specific label) for sim or obs
  if(!is.null(xtrLbl_obs)){xtrLbl_obs_full <- paste0('_', xtrLbl_obs)} else {xtrLbl_obs_full = NULL}
  if(!is.null(xtrLbl_sim)){xtrLbl_sim_full <- paste0('_', xtrLbl_sim)} else {xtrLbl_sim_full = NULL}
  
  # # in case the name of the target variable is different in either sim or obs
  # if(is.null(target_var_obs)){target_var_obs <- target_var}
  # if(is.null(target_var_sim)){target_var_sim <- target_var}
  exists()
  
  # get list of files
  files_obs <- list.files(path = path_obs, pattern = paste0(target_var_obs, '_', src_obs, '_', spres, xtrLbl_obs_full), full.names = T)
  files_sim <- list.files(path = path_sim, pattern = paste0(target_var_sim, '_', src_sim, '_', spres, xtrLbl_sim_full), full.names = T)
  
  # init the empty dataframe
  df_comb <- data.frame()
  
  for(ifile in files_obs){
    
    print(paste(' |> working on file:', basename(ifile), ' <| '))
    
    # work on RS file 
    load(ifile)
    df_obs <- df
    
    # get equivalent from the Reanalysis
    load(files_sim[1]) 
    df_sim <- df
    
    # select and combine
    df_dum <- df_obs %>% 
      select(x, y, year, month, LAI) %>%
      inner_join(df_sim %>% 
                   mutate(LAI_sim = cvh * lai_hv + cvl * lai_lv) %>%
                   select(x, y, month, LAI_sim),
                 by = c("x", "y", "month")) %>% 
      rename(obs = LAI, sim = LAI_sim) 
    df_comb <- bind_rows(df_comb, df_dum) 
  }
  
  save('df_comb', file = paste0(out_path,'/df_comb___', target_var, xtrLbl_obs_full, '.RData'))
  print(paste(" ----> finished with:", target_var, xtrLbl_obs, "<----"))
  
}
