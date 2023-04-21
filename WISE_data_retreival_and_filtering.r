##############getting data
library(tidyverse)
library(dtplyr)
library(rFIA)

options(timeout=36000)
db_states <- c('CT','DE','IL','IN','IA','KS','ME','MD',
               'MA','MI','MN','MO','NE','NH','NJ','NY',
               'ND','OH','PA','RI','SD','VT','WV','WI',
               'AL','AR','FL','GA','KY','LA','MS','NC',
               'OK','SC','TN','VA', 'TX')

db_dir <- 'D:/FIA/rFIA'
#db_dir2 <- 'C:/Users/user/OneDrive - University of Kentucky/Mizzou OneDrive/Invasive'

getFIA(db_states, dir = db_dir) ####of course, this didnt work with the current state of the FIADB and rFIA so I manually downloaded each state from the FIA datamart
db <- readFIA(db_dir, states = db_states, inMemory = T, nCores=10)

PLOT <- list.files(db_dir,pattern="*_PLOT.csv",recursive=T,full.names=T) 
	PLOT <- PLOT[!grepl('OZONE',PLOT)]
	PLOT <- PLOT[!grepl('FVS',PLOT)]
	plt <- PLOT %>% map_df(~read_csv(.,col_types=cols(.default="c"))) %>% type_convert(.)  
#write_csv(plt,paste0(db_dir2,'/PLOT.csv'))
#rm(PLOT)

#PLOT <- data.table::fread(file=paste0(db_dir2,"/PLOT.csv"))
	PLOT0 <- plt %>% lazy_dt() %>%
		mutate(pltID = paste(STATECD, UNITCD, COUNTYCD, PLOT, sep = '_')) %>% #create a unique identifier to link plot locations through time
		mutate(PLT_CN=as.character(CN)) %>%
		group_by(pltID) %>% arrange(pltID,INVYR) %>% 
			filter(LON>=-100) %>% 				            #keep eastern US plots
			filter(MANUAL>=1) %>% 				            #keep only national design samples
			filter(!INVYR==9999) %>% 			            #an inventory year of 9999 is reserved for phase 3 plots in the western US, lose those
		mutate(samps=sum(PLOT_STATUS_CD==1)) %>% 		    #identify number of times each location sampled w/ forestland
			filter(samps>0) %>% filter(!is.na(samps)) %>% 	#keep only plots that ever had forestland sampled
		mutate(visits=n_distinct(PLT_CN)) %>%  		        #identify number of times each location visited
		mutate(rl=max(rle(PLOT_STATUS_CD==1)[[1]])) %>% 	#identify longest run of consecutively sampled w/ forestland visits per location
		mutate(full=sum(MANUAL>=2)) %>% 			        #identify number of times each location sampled w/o seedling censored @ 6 (underestimate for NCRS locales)
	    data.table::as.data.table()
#rm(PLOT)
#write_csv(PLOT0,paste0(db_dir2,'/PLOT0.csv'))

TREE <- list.files(db_dir,pattern="*_TREE.csv",recursive=T,full.names=T) 
	TREE <- TREE[!grepl('OZONE',TREE)]
	TREE <- TREE[!grepl('FVS',TREE)]
	tre <- TREE %>% map_df(~read.csv(.,header=T) %>% filter(PLT_CN %in% PLOT0$PLT_CN))  
#write_csv(tre,'C:/Users/lance/OneDrive - University of Kentucky/Mizzou OneDrive/Invasive/TREE.csv')
#rm(TREE)

SEEDLING <- list.files(db_dir,pattern="*_SEEDLING.csv",recursive=T,full.names=T) %>% map_df(~read_csv(.,col_types=cols(.default="c"))) %>% type_convert(.) 
#write_csv(SEEDLING,paste0(db_dir2,'/SEEDLING.csv'))
#rm(SEEDLING)



##############using data to begin filtering process
inv_spcd <- c(341, 712, 993, 994)

PLOT0 <- PLOT0 %>% lazy_dt() %>%
		mutate(CN=as.double(CN),PLT_CN=as.double(CN),pltID = paste(STATECD, UNITCD, COUNTYCD, PLOT, sep = '_')) %>%
		relocate(c(pltID,PLT_CN,INVYR)) %>%
		as_tibble()
		
	plots <- PLOT0$PLT_CN
	
TREE <- tre %>% lazy_dt() %>%
		mutate(CN=as.double(CN),PLT_CN=as.double(PLT_CN),pltID = paste(STATECD, UNITCD, COUNTYCD, PLOT, sep = '_'),treeID=paste(pltID,SUBP,TREE, sep = '_')) %>%
		filter(PLT_CN %in% plots) %>%
		relocate(c(pltID,PLT_CN,INVYR,treeID)) %>%
		as_tibble() 
		
SEEDLING <- SEEDLING %>% lazy_dt() %>% 
			mutate(CN=as.double(CN),PLT_CN=as.double(PLT_CN),pltID = paste(STATECD, UNITCD, COUNTYCD, PLOT, sep = '_')) %>%
			filter(PLT_CN %in% plots) %>%
			relocate(c(pltID,PLT_CN,INVYR)) %>%
			as_tibble() 

########document which plot locations had one of the invasive spp of interest on them at any point in time as a tree (dbh>=1")

	tree <- TREE %>% group_by(pltID,INVYR) %>% nest()
		t1 <- tree %>% mutate(invaded.t=map(data,~any(.$SPCD %in% inv_spcd,na.rm=T)),ailanthus.t=map(data,~any(.$SPCD==341,na.rm=T)),tallow.t=map(data,~any(.$SPCD==994,na.rm=T)),paulownia.t=map(data,~any(.$SPCD==712,na.rm=T)),chinaberry.t=map(data,~any(.$SPCD==993,na.rm=T))) %>%
				unnest(c(invaded.t,ailanthus.t,tallow.t,paulownia.t,chinaberry.t))
			t1 <- t1 %>% rename(tree.data=data)

########document which plot locations had one of the invasive spp of interest on them at any point in time as a seedling (height >=1',dbh<1")

	seed <- SEEDLING %>% group_by(pltID,INVYR) %>% nest()
		s1 <- seed %>% mutate(invaded.s=map(data,~any(.$SPCD %in% inv_spcd,na.rm=T)),ailanthus.s=map(data,~any(.$SPCD==341,na.rm=T)),tallow.s=map(data,~any(.$SPCD==994,na.rm=T)),paulownia.s=map(data,~any(.$SPCD==712,na.rm=T)),chinaberry.s=map(data,~any(.$SPCD==993,na.rm=T))) %>%
				unnest(c(invaded.s,ailanthus.s,tallow.s,paulownia.s,chinaberry.s))
			s1 <- s1 %>% rename(seed.data=data)
			
########document which plot locations had one of the invasive spp of interest on them at any point in time as either a tree or seedling from above

		st1 <- t1 %>% full_join(s1)
			st2 <- st1 %>% rowwise() %>% 
					mutate(invaded=invaded.t|invaded.s,ailanthus=ailanthus.t|ailanthus.s,tallow=tallow.t|tallow.s,paulownia=paulownia.t|paulownia.s,chinaberry=chinaberry.t|chinaberry.s) %>%
					ungroup() %>%
					mutate(across(.cols=c(invaded,ailanthus,tallow,paulownia,chinaberry),.fns=~replace_na(.,FALSE)))

########create an annotated nested tibble that retains all the tree and seedling data through time for storage and later uses				
			st2 <- st2 %>% left_join(PLOT0 %>% dplyr::select(pltID,LAT,LON,ECOSUBCD,MEASYEAR,INVYR,STATECD,UNITCD,COUNTYCD,PLOT,CN,PLT_CN,PREV_PLT_CN)) %>% mutate(PREV_PLT_CN=as.double(PREV_PLT_CN)) %>% relocate(seed.data,.after=tree.data)

########create an annotated nested tibble that discards all the tree and seedling data through time for quicker loading that doesnt require all tree and seedling records for later uses			
		st2p <- st2	%>% select(-tree.data,-seed.data) %>% relocate(c(CN,PREV_PLT_CN,INVYR,LAT,LON,STATECD,UNITCD,COUNTYCD,PLOT,ECOSUBCD,pltID,invaded,invaded.t,invaded.s))
		
		
#saveRDS(st2p,paste0(db_dir2,"/invasion_plts.RDS"))
#saveRDS(st2,paste0(db_dir2,"/invasion.RDS"))

########delineate the ecological subsections where each invasive species ever occured as the 'range' 

ailanthus.range <- st2p %>% filter(ailanthus) %>% select(ECOSUBCD) %>% distinct()
chinaberry.range <- st2p %>% filter(chinaberry) %>% select(ECOSUBCD) %>% distinct()
paulownia.range <- st2p %>% filter(paulownia) %>% select(ECOSUBCD) %>% distinct()
tallow.range <- st2p %>% filter(tallow) %>% select(ECOSUBCD) %>% distinct()

########re-join the data that documented how many times each plot location was measured and whether all of those measurements were consecutive (no interruptions)

st3p <- st2p %>% left_join(PLOT0 %>% select(pltID,PLT_CN,CN,visits,rl)) %>%
        select(pltID,INVYR,MEASYEAR,PLT_CN,tallow,ailanthus,paulownia,chinaberry,visits,rl,ECOSUBCD) %>% 
		mutate(after.tallow=tallow,after.ailanthus=ailanthus,after.paulownia=paulownia,after.chinaberry=chinaberry) %>% #renaming for convienence and improved clarity, this denotes whether the invasive was observed in this measurement period
        group_by(pltID) %>% 
		mutate(tallow=any(tallow),ailanthus=any(ailanthus),paulownia=any(paulownia),chinaberry=any(chinaberry)) %>%     #losing the NA's that were induced from those plots that had either seedling records in teh FIADB or trees records in the FIADB but not both (FIA does not record a 0 if no trees or seedlings are present)
		arrange(pltID,INVYR) %>%
		mutate(long.term=(rl>=3&rl==visits))                                                                            #I'm consdiring plots that were measured at least 3 times without interruption long-term measurements 
 
 
  
##########identify the plot locations that have long-term measurements post-invasion 

ailanthus.plots <- st3p %>% filter(long.term,ECOSUBCD %in% ailanthus.range$ECOSUBCD,ailanthus) %>%
					group_by(pltID,after.ailanthus) %>% summarise(vis=n_distinct(PLT_CN)) %>%
					filter(after.ailanthus & vis>=3) %>% 
					left_join(st3p %>% select(pltID,ECOSUBCD,visits,rl) %>% distinct())
write_csv(ailanthus.plots, './Data/ailanthus_plots.csv')
	
chinaberry.plots <- st3p %>% filter(long.term,ECOSUBCD %in% chinaberry.range$ECOSUBCD,chinaberry) %>%
					group_by(pltID,after.chinaberry) %>% summarise(vis=n_distinct(PLT_CN)) %>%
					filter(after.chinaberry & vis>=3) %>% 
					left_join(st3p %>% select(pltID,ECOSUBCD,visits,rl) %>% distinct())
write_csv(chinaberry.plots, './Data/chinaberry_plots.csv')

paulownia.plots <- st3p %>% filter(long.term,ECOSUBCD %in% paulownia.range$ECOSUBCD,paulownia) %>%
					group_by(pltID,after.paulownia) %>% summarise(vis=n_distinct(PLT_CN)) %>%
					filter(after.paulownia & vis>=3) %>% 
					left_join(st3p %>% select(pltID,ECOSUBCD,visits,rl) %>% distinct())
write_csv(paulownia.plots, './Data/paulownia_plots.csv')

tallow.plots <- st3p %>% filter(long.term,ECOSUBCD %in% tallow.range$ECOSUBCD,tallow) %>%
					group_by(pltID,after.tallow) %>% summarise(vis=n_distinct(PLT_CN)) %>%
					filter(after.tallow & vis>=3) %>% 
					left_join(st3p %>% select(pltID,ECOSUBCD,visits,rl) %>% distinct())
write_csv(tallow.plots, './Data/tallow_plots.csv')

	#####which individual plot records do long-term measurements post-invasion include for each plot location? 
	 #note this may includes some plot records after the required 3 consecutive presence measurements when the invasive is no longer present on the plot.
	 #to eliminate those and retain only the records where the invasive was present during the 'long-term' requirement simply add something like 'filter(after.ailanthus==T)' 
	
	ailanthus.cns <- st3p %>% filter(pltID %in% ailanthus.plots$pltID) %>% select(pltID,PLT_CN,INVYR,MEASYEAR,ECOSUBCD,after.ailanthus)
	
	chinaberry.cns <- st3p %>% filter(pltID %in% chinaberry.plots$pltID) %>% select(pltID,PLT_CN,INVYR,MEASYEAR,ECOSUBCD,after.chinaberry)
	
	paulownia.cns <- st3p %>% filter(pltID %in% paulownia.plots$pltID) %>% select(pltID,PLT_CN,INVYR,MEASYEAR,ECOSUBCD,after.paulownia)
	
	tallow.cns <- st3p %>% filter(pltID %in% tallow.plots$pltID) %>% select(pltID,PLT_CN,INVYR,MEASYEAR,ECOSUBCD,after.tallow)
	
	
