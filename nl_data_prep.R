
# this script produces night light data for parliamentary constituency level normalised for the election periods
# it takes the ac level data and aggregates at the pc level
# at the pc level it computes the sum of total_light_calibrated and sum of total_cells for every pc for the period of 1991-2009 for each election year
# calculates the variable by taking the pixel level average and then normalising for the years between two elections

# avg_light_per_pixel = (sum(total_lights_calibrated)/sum(total_cells))/year_between_elections


  
# replace the data directory here

path = "/Users/najah/work/cpr/data/"


#reading the package 

suppressMessages(library(dplyr))
## reading the nl

nl = read.csv(paste0(path,"shrug/shrug-v1.5.samosa-con-shrug-csv/con_shrug_2007_nl_wide.csv"))


# - aggregating for the election periods - average night light of total night light during the election period
# - first we convert the wide data to long data (we will need this later we merge this with dyn data since it is long)


# keeping only total night lights caliberated

pattern = '^total_light_cal'

nl = cbind(nl[,1:2],nl[,c(grepl(pattern, names(nl)))])

## nl

## converting to long dataset

nl = tidyr::pivot_longer(nl, cols = -c(ac07_id, num_cells), 
                         names_to= 'year',
                         values_to = 'total_light')

## extracting the year


nl$year = as.numeric(gsub("[^0-9]", "",nl$year))




## aggregating for election years

# Define the breaks and labels for the categories
labels <- c(1994, 1996, 1998, 1999, 2004,2009)
breaks <- c( 1994,1996, 1998, 1999, 2004, 2009, 2014)

nl$year_el = cut(nl$year, breaks = breaks, labels = labels, include.lowest = TRUE)
#table(nl$year_el)

# night light average variable
#adds up the light during the years under the election regime and averages per pixel then normalises for per year

nl_el = nl %>% group_by(ac07_id,year_el) %>% 
  # creating a variable that counts for the years under each election regime
  summarise( years_under = max(year)-min(year)+1,
             total_lights = sum(total_light),
             num_cells = mean(num_cells)) %>% 
  ungroup()





# shrug state key

skey = read.csv(paste0(path,"/shrug/shrug/shrug-v1.5.samosa-keys-csv/shrug_pc01_state_key.csv"))
# getting the state code

state_id = unique(skey[, 1:2], drop = TRUE)

#removing the index
rownames(state_id) = NULL


# mergin with the nl data

# extracting the state id from ac07_id
nl_el$state_id = as.numeric(gsub(".*-(\\d+)-.*", "\\1", nl_el$ac07_id))

nl_el = left_join(nl_el, state_id, by = c("state_id" = "pc01_state_id"))

# now extracting the ac-id from the ac_id

nl_el$ac_id  = as.numeric(gsub(".*-(\\d{3})$", "\\1", nl_el$ac07_id))

# trimming the zeros before

#nl_el$ac_id = gsub("^0+", "", nl_el$ac_id)

# limiting data to 23 states

# now i can use the ac-pc mapping
# reading the file with ac-pc pre-delim data
ac_pc = read.csv(paste0(path,"RerunDta.csv"))



# keep only one value for every ac

ac_pc = ac_pc[!duplicated(ac_pc[, c("State_name_2001", "AC_no_2001")]),]


ac_pc = ac_pc[, c("State_number_2001","State_name_2001","AC_no_2001", "AC_name_2001","PC_no_2001",
                  "PC_name_2001" )]

#lowering the names for merging
ac_pc$State_name_2001 = tolower(ac_pc$State_name_2001)



# - Limiting analyses to the 23 states used in francesa's data.
state_list = unique(ac_pc$State_name_2001)

#write.csv(as.data.frame(state_list), "./data/states_list.csv")

nl_el = nl_el[nl_el$pc01_state_name %in% state_list,]

# francesca's list is missing 4 constituencies in manipur

nl_ac = left_join(nl_el, ac_pc, by = c("pc01_state_name"= "State_name_2001", "ac_id" = "AC_no_2001"))





## aggregating at pc level
# what measure do i use?
# is average of constituencies better than average at pc level?

nl_pc = nl_ac %>% group_by(pc01_state_name,State_number_2001,PC_no_2001, year_el ) %>% 
  mutate(
    avg_light_per_pixel =(sum(total_lights)/sum(num_cells))/years_under) %>% 
  ungroup()

write.csv(nl_pc, paste0(path,"nl_pc_india.csv"))











