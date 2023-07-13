
# this script produces night light data for parliamentary constituency level normalised for the election periods
# it takes the ac level data and aggregates at the pc level
# at the pc level it computes the sum of total_light_calibrated and sum of total_cells for every pc for the period of 1991-2009 for each  year
# then calculated the rate of change for each year
# then computes the average rate of growth for the given term


# avg_light_per_pixel_yr = (sum(total_lights_calibrated)/sum(total_cells))/year_between_elections

# but the problem is this - this is the light for that year. we need something that measures change in night lights over time.

  
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





# # Calculate changes within each ac07_id group
# nl <- nl %>%
#   group_by(ac07_id) %>%
#   arrange(ac07_id,year) %>%
#   mutate(change = total_light - lag(total_light, default = 0))
# 
# print(nl)


# shrug state key

skey = read.csv(paste0(path,"/shrug/shrug/shrug-v1.5.samosa-keys-csv/shrug_pc01_state_key.csv"))
# getting the state code

state_id = unique(skey[, 1:2], drop = TRUE)

#removing the index
rownames(state_id) = NULL


# mergin with the nl data

# extracting the state id from ac07_id
nl$state_id = as.numeric(gsub(".*-(\\d+)-.*", "\\1", nl$ac07_id))

nl = left_join(nl, state_id, by = c("state_id" = "pc01_state_id"))

# now extracting the ac-id from the ac_id

nl$ac_id  = as.numeric(gsub(".*-(\\d{3})$", "\\1", nl$ac07_id))

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

nl = nl[nl$pc01_state_name %in% state_list,]

# francesca's list is missing 4 constituencies in manipur

nl_ac = left_join(nl, ac_pc, by = c("pc01_state_name"= "State_name_2001", "ac_id" = "AC_no_2001"))

###



# night light average variable
#adds up the light during the years under the election regime and averages per pixel then normalises for per year

nl_pc = nl_ac %>% group_by(pc01_state_name,PC_no_2001, year) %>% 
  # creating a variable that counts for the years under each election regime
  summarise( 
             total_lights = sum(total_light),
             num_cells = mean(num_cells)) %>% 
  ungroup()


######

## calculate yearly change 


#present value - past value/past value = growth rate


#nl_sub <- nl[1:50,]
grouped_data <- split(nl_pc, nl_pc$PC_no_2001)
change <- vector("numeric", length = length(nl_pc$total_lights))
index <- 1
for (i in 1:length(grouped_data)) {
  nl_grp <- grouped_data[[i]]
  for (j in 1:length(nl_grp$total_lights)) {
    if (j ==1) {
      
      change[index] <- 0
      
    } else {
      if (nl_grp$total_lights[j-1]!=0){
        change[index] <- (nl_grp$total_lights[j] - nl_grp$total_lights[j-1])/nl_grp$total_lights[j-1]
      }else {
        change[index]<- 1
      }
      
      
      
    }
    index <- index+ 1
  }
}
# add it to the main data
nl_pc = cbind(nl_pc, change)

##########
# now calculate the average growthr rate normalised by year

## add election years

## aggregating for election years

# Define the breaks and labels for the categories
labels <- c(1994, 1996, 1998, 1999, 2004,2009)
breaks <- c( 1994,1996, 1998, 1999, 2004, 2009, 2014)

nl_pc$year_el = cut(nl_pc$year, breaks = breaks, labels = labels, include.lowest = TRUE)

nl_pc = nl_pc %>% 
  group_by(pc01_state_name,PC_no_2001,year_el) %>% 
  mutate(el_duration = max(year)-min(year)+1)




################


## aggregating at pc level



# what measure do i use?
# is average of constituencies better than average at pc level?

nl_pc_vals = nl_pc %>% group_by(pc01_state_name,PC_no_2001, year_el ) %>% 
  summarise(
    avg_yearly_growth = sum(change)/mean(el_duration)) %>% 
  ungroup()

write.csv(nl_pc_vals, paste0(path,"nl_pc_india.csv"))











