rankhospital <- function(state, outcome, num = "best"){
    library(dplyr);
    library(naniar);
    ##Read  outcome data
    datafile <- read.csv("data/outcome-of-care-measures.csv");
    
    ## Check that state and outcome are valid
    ## check state input validity
    valid_state <- state %in% (datafile$State);
    
    ## check outcome input validity
    checking_outcome <- c("heart attack", "heart failure", "pneumonia");
    valid_outcome <- outcome %in% checking_outcome;
    
    if ( valid_state && sum(valid_outcome) > 0){ ##if valid
        ## clean and select certain columns
        datas <- datafile %>%
            select(starts_with("Hospital"), -contains("Readmission"), State) %>%
            group_by("Hospital Name",State);
        
        ## find and replace all NAN values to NA
        na_strings <- c("NA", "N A", "N / A", "N/A", "N/ A", "Not Available", "NOt available");
        
        data_nona <- datas %>%
            replace_with_na_all(condition = ~.x %in% c("NA", "N A", "N / A", "N/A", "N/ A", "Not Available", "NOt available"));
        
        ## convert the data in column mortality to numeric
        datas2 <- data_nona %>%
            mutate_at(vars(contains("Mortality")), as.numeric);
        
        ## clean and get data belongs to the outcome input
        data_for_process <- c();
        if ( grepl("Heart.Attack", outcome, ignore.case = TRUE) ){
            data_for_process <- datas2 %>% select(1,contains("Heart.Attack"), State);
        } else if ( grepl("Heart.Failure", outcome, ignore.case = TRUE) ){
            data_for_process <- datas2 %>% select(1,contains("Heart.Failure"), State);
        } else if ( grepl("Pneumonia", outcome, ignore.case = TRUE) ){
            data_for_process <- datas2 %>% select(1,contains("Pneumonia"), State);
        }
        
        ## clean and remove the na values
        data_for_process <- na.omit(data_for_process[complete.cases(data_for_process), ]);
        data_for_process <- data_for_process %>% tidyr::drop_na();
        
        #rename for ease of sorting and selection
        names(data_for_process)[1] <- "Hospital";
        names(data_for_process)[2] <- "Data";
        names(data_for_process)[3] <- "State";
        
        ## get only relevant data based on state input
        data_to_sort <- data_for_process %>% filter(State == state, ignore.case = TRUE);
        
        ## order the data frame
        data_after_sort <- data.frame(data_to_sort[order(data_to_sort$Data, data_to_sort$Hospital),]);
        
        ## Return hospital name in that state with lowest 30-day death rate based on selected rank
        if ( num == "best" ){
            print(head(data_after_sort,1)[,1]);
        } else if ( num == "worst" ){
            print(tail(data_after_sort,1)[,1]);
        } else if ( as.numeric(num) > 0 && as.numeric(num) <= nrow(data_after_sort) ){
            print(data_after_sort[rownames(data_after_sort)==num,1]);
        } else {
            print("NA");
        }
        
    } else if ( !valid_state ){ ##if invalid state input
        stop(print("invalid state"))
    } else if ( sum(!valid_outcome) > 0){ ##if invalid outcome input
        stop(print("invalid outcome"))
    }
    
}