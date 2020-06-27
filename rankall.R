rankall <- function(outcome, num = "best"){
    library(Hmisc);
    library(dplyr);
    library(naniar);
    
    ##Read  outcome data
    datafile <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character");
    
    ## check outcome input validity
    checking_outcome <- c("heart attack", "heart failure", "pneumonia");
    valid_outcome <- outcome %in% checking_outcome;
    
    if ( sum(valid_outcome) > 0){ ##if valid outcome
        ## clean and select certain columns
        datas <- datafile %>%
            select(starts_with("Hospital"), -contains("Readmission"), State);
            ##%>% group_by(`Hospital.Name`,State);
        
        ## convert the data in column mortality to numeric
        datas2 <- datas %>%
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
        
        ##rename for ease of sorting and selection
        names(data_for_process)[1] <- "hospital";
        names(data_for_process)[2] <- "rate";
        names(data_for_process)[3] <- "state";
        
        ##rank the data
        ranking <- data_for_process %>%
            arrange(rate, hospital) %>%
            group_by(state)%>%
            arrange(state) %>%
            mutate(rank = row_number());
        
        ##do the ranking based on request
        if (num == "worst"){
            worst = dplyr::summarize(ranking %>% tidyr::drop_na(), rank = max(rank))
                 return(inner_join(ranking, worst, by = c("state", "rank")) 
                   %>% select(hospital, state))
        } else if (num == "best"){
            num = min(ranking$rank)
        } else {
            num= num
        }
        
        ##if ranking is not the worst .. proceed here
        ranking <- ranking %>%
            filter(rank == num)%>%
            select(hospital, state)
        
        statenames <- unique(data_for_process$state)
        subs <- statenames[statenames %nin% ranking$state]
        missing = NULL
        for (i in seq_along(subs)){
            hospital <-  "<NA>"
            state<-  subs[i]
            missing= bind_rows(missing, tibble(hospital,state))
        }
        
        result <- bind_rows(ranking,missing) %>% arrange(state)
        
        ## return the result
        result
    } else if ( sum(!valid_outcome) > 0){ ##if invalid outcome input
        stop(print("invalid outcome"))
    }
    
}