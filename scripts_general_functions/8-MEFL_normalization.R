# normalize to FITC 100, SULFO 50

# Subset the fluorescence data from the molecular fluorophores
find_molecular_normalizers <- function(.df)
{
  
  # concentrations in micro molar
  concentrations <- tibble(GFP = 100,
                           RFP = 50)
  
  # filter the molecular fluorophores 
  filter(.df,
         str_detect(Samples, 'FITC|SULFO|SULPHO')) %>% 
    
    # proceed only if data is present
    {if(!plyr::empty(.)) 
      
    {
      # retain only raw fluor columns
      select(., any_of (c('Samples', 'GFP', 'RFP'))) %>% 
        group_by(Samples) %>% 
        
        # Find mean of replicates
        summarise(across(any_of(c('GFP', 'RFP')),
                         mean)) %>% 
        group_by(.) %>% 
        
        # retain only the right values
        summarise(across(any_of(c('GFP', 'RFP')),
                         max)) %>% 
        # This step can be improved for when one of the fluorophores is missing --
        # -- but both fluor are measured (rare case)
        
        # correction for concentration (per micro molar)
        mutate(across(any_of(c('GFP', 'RFP')),
                      ~ ./concentrations[[cur_column()]]))
  
    } else .}
}

# Normalize the data to the fluorophores (per micro molar fluorophore equivalent fluor.)
normalize_molecules_equivalent <- function(.df)
  
{
  .normalizer <- find_molecular_normalizers(.df)
  
  .df %>% 
    mutate(across(any_of(c('GFP', 'RFP')),
                  ~ ./.normalizer[[cur_column()]] )) %>% 
    
    # remove the molecular fluorophore samples from the dataset before returning
    filter(!str_detect(Samples, 'FITC|SULFO|SULPHO'))
  
}
