

# Examples: Import Dataset
example_dataset <- readRDS("data/ResultsObjectList.rds")
example_dataset[[13]]<-BCdata_EventStudy
example_dataset[[14]]<-LWdata_EventStudy
names(example_dataset[[13]])<-c("beta","sigma","tVec","referencePeriod","prePeriodIndices","postPeriodIndices")
example_dataset[[13]]$paper_formatted <- "Benzarti and Carloni (2019)"
names(example_dataset[[14]])<-c("beta","sigma","tVec","referencePeriod","prePeriodIndices","postPeriodIndices","stdErrors")
example_dataset[[14]]$paper_formatted <- "Lovenheim & Willen (2019)"
names(example_dataset)<-1:14
example_dataset<-example_dataset[c(1,13,2,4:9,14,10:12,3)]
names(example_dataset) <- NULL

# Define a function to read in multiple sheets of an excel file simultaneously
multiplesheets <- function(fname) {
  # getting info about all excel sheets
  sheets <- readxl::excel_sheets(fname)
  tibble <- lapply(sheets, function(x) readxl::read_excel(fname, sheet = x))
  data_frame <- lapply(tibble, as.data.frame)
  # assigning names to data frames
  names(data_frame) <- sheets
  # return data frame
  return(data_frame)
}



server <- function(input, output, session) {
  
  # *****************************************************************
  # ************* Examples Tab **************************************
  # *****************************************************************
  
  # ------------ Alerts----------------------------------------------
  # Alert below will trigger if both optional restrictions are selected when Bounding Relative Magnitudes is included in base Delta
  observe({
    if (all(input$example_delta !="2", input$example_sign != "1", input$example_monotonicity != "1")) {
      example_optional_delta_check <- "If bounding relative magnitudes is included in base Delta, it is not allowed to select both shape (aka monotonicity) and sign restrictions as additions."
      example_optional_delta_js_string <- 'alert("SOMETHING");'
      example_optional_delta_js_string <- sub("SOMETHING",example_optional_delta_check,example_optional_delta_js_string)
      session$sendCustomMessage(type='jsCode', list(value = example_optional_delta_js_string))
    }
  })
  
  # Alert below will trigger if FLCI or Conditional FLCI is chosen for any base restrictions other than single SD
  observe({
    if (all(input$example_method_rm %in% c("FLCI", "C-F"), 
            input$example_delta != "2")) {
      example_flci_check <- "Fixed Length Confidence Intervals or Conditional FLCI Hybrid is not suitable for base Delta that includes Bounding Relative Magnitudes, because it is proven that optimal FLCI has infinite length under those restrictions."
      example_flci_js_string <- 'alert("SOMETHING");'
      example_flci_js_string <- sub("SOMETHING",example_flci_check,example_flci_js_string)
      session$sendCustomMessage(type='jsCode', list(value = example_flci_js_string))
    }
  })

  # Alert below will trigger if l vector is wrongly formatted
  observe({
    if (
      any(NA %in% (mod(as.numeric(unlist(strsplit(input$example_lvec,","))),1)!=0), 
          any(mod(as.numeric(unlist(strsplit(input$example_lvec,","))),1)!=0),
          any(as.numeric(unlist(strsplit(input$example_lvec,",")))<1))
    ) {
      example_integer_check <- "If any, the vector that is used to determine parameter of interest must consist of numbers being seperated by commas. Numbers must be integers that are not less than 1. Please check examples in hints."
      example_integer_js_string <- 'alert("SOMETHING");'
      example_integer_js_string <- sub("SOMETHING",example_integer_check,example_integer_js_string)
      session$sendCustomMessage(type='jsCode', list(value = example_integer_js_string))
      }
    })
  
  # Alert below will trigger if textinput m vector is wrongly formatted
  observe({
    if (
      any(NA %in% (mod(as.numeric(unlist(strsplit(input$example_m_textinput,","))),1)!=0),
          any(as.numeric(unlist(strsplit(input$example_m_textinput,",")))<0))
    ) {
      example_m_textinput_check <- "The vector of M must consist of numbers being seperated by commas. Numbers must be non-negative. Please check examples in hints."
      example_m_textinput_js_string <- 'alert("SOMETHING");'
      example_m_textinput_js_string <- sub("SOMETHING",example_m_textinput_check,example_m_textinput_js_string)
      session$sendCustomMessage(type='jsCode', list(value = example_m_textinput_js_string))
    }
  })
  
  # Alert below will trigger if both textinput and sequence m vector are defined
  observe({
    if (
      all(input$example_m_textinput !="",
          any(!is.na(input$example_m_lower), !is.na(input$example_m_upper), !is.na(input$example_m_step)))
    ) {
      example_m_text_seq_check <- "You can enter the vector of M EITHER arbitrarily OR as a sequence, but NOT both."
      example_m_text_seq_js_string <- 'alert("SOMETHING");'
      example_m_text_seq_js_string <- sub("SOMETHING",example_m_text_seq_check,example_m_text_seq_js_string)
      session$sendCustomMessage(type='jsCode', list(value = example_m_text_seq_js_string))
    }
  })
  
  # Alert below will trigger if any of lower, upper, step in the sequence m vector is negative 
  observe({
    if (any(all(!is.na(input$example_m_lower), input$example_m_lower <0),
            all(!is.na(input$example_m_upper), input$example_m_upper <0),
            all(!is.na(input$example_m_step), input$example_m_step <0))) {
      example_m_seq_neg_check <- "Lower and upper bounds as well as step of the M sequence must be non-negative."
      example_m_seq_neg_js_string <- 'alert("SOMETHING");'
      example_m_seq_neg_js_string <- sub("SOMETHING",example_m_seq_neg_check,example_m_seq_neg_js_string)
      session$sendCustomMessage(type='jsCode', list(value = example_m_seq_neg_js_string))
    }
  })
  
  # Alert below will trigger if any of lower, upper, step in the sequence m vector is wrongly defined
  observe({
    if (any(all(!is.na(input$example_m_lower), !is.na(input$example_m_upper), input$example_m_lower > input$example_m_upper),
            all(!is.na(input$example_m_lower), !is.na(input$example_m_upper), !is.na(input$example_m_step), input$example_m_step > input$example_m_upper - input$example_m_lower))) {
      example_m_seq_step_check <- "Lower bound of the M sequence shouldn't be larger than upper bound, and step of the sequence shouldn't be larger than the difference between lower and upper bounds."
      example_m_seq_step_js_string <- 'alert("SOMETHING");'
      example_m_seq_step_js_string <- sub("SOMETHING",example_m_seq_step_check,example_m_seq_step_js_string)
      session$sendCustomMessage(type='jsCode', list(value = example_m_seq_step_js_string))
    }
  })
  
  # Alert below will trigger if textinput mbar vector is wrongly formatted
  observe({
    if (
      any(NA %in% (mod(as.numeric(unlist(strsplit(input$example_mbar_textinput,","))),1)!=0),
          any(as.numeric(unlist(strsplit(input$example_mbar_textinput,",")))<0))
    ) {
      example_mbar_textinput_check <- "The vector of Mbar must consist of numbers being seperated by commas. Numbers must be non-negative. Please check examples in hints."
      example_mbar_textinput_js_string <- 'alert("SOMETHING");'
      example_mbar_textinput_js_string <- sub("SOMETHING",example_mbar_textinput_check,example_mbar_textinput_js_string)
      session$sendCustomMessage(type='jsCode', list(value = example_mbar_textinput_js_string))
    }
  })
  
  # Alert below will trigger if both textinput and sequence mbar vector are defined
  observe({
    if (
      all(input$example_mbar_textinput !="",
          any(!is.na(input$example_mbar_lower), !is.na(input$example_mbar_upper), !is.na(input$example_mbar_step)))
    ) {
      example_mbar_text_seq_check <- "You can enter the vector of Mbar EITHER arbitrarily OR as a sequence, but NOT both."
      example_mbar_text_seq_js_string <- 'alert("SOMETHING");'
      example_mbar_text_seq_js_string <- sub("SOMETHING",example_mbar_text_seq_check,example_mbar_text_seq_js_string)
      session$sendCustomMessage(type='jsCode', list(value = example_mbar_text_seq_js_string))
    }
  })
  
  # Alert below will trigger if any of lower, upper, step in the sequence mbar vector is negative 
  observe({
    if (any(all(!is.na(input$example_mbar_lower), input$example_mbar_lower <0),
            all(!is.na(input$example_mbar_upper), input$example_mbar_upper <0),
            all(!is.na(input$example_mbar_step), input$example_mbar_step <0))) {
      example_mbar_seq_neg_check <- "Lower and upper bounds as well as step of the Mbar sequence must be non-negative."
      example_mbar_seq_neg_js_string <- 'alert("SOMETHING");'
      example_mbar_seq_neg_js_string <- sub("SOMETHING",example_mbar_seq_neg_check,example_mbar_seq_neg_js_string)
      session$sendCustomMessage(type='jsCode', list(value = example_mbar_seq_neg_js_string))
    }
  })
  
  # Alert below will trigger if any of lower, upper, step in the sequence mbar vector is wrongly defined
  observe({
    if (any(all(!is.na(input$example_mbar_lower), !is.na(input$example_mbar_upper), input$example_mbar_lower > input$example_mbar_upper),
            all(!is.na(input$example_mbar_lower), !is.na(input$example_mbar_upper), !is.na(input$example_mbar_step), input$example_mbar_step > input$example_mbar_upper - input$example_mbar_lower))) {
      example_mbar_seq_step_check <- "Lower bound of the Mbar sequence shouldn't be larger than upper bound, and step of the sequence shouldn't be larger than the difference between lower and upper bounds."
      example_mbar_seq_step_js_string <- 'alert("SOMETHING");'
      example_mbar_seq_step_js_string <- sub("SOMETHING",example_mbar_seq_step_check,example_mbar_seq_step_js_string)
      session$sendCustomMessage(type='jsCode', list(value = example_mbar_seq_step_js_string))
    }
  })

  # Alert below will trigger if bounds of grid used for test inversion is filled when boudning relative magnitudes is not included in base Delta
  observe({
    if (all(input$example_delta == "2", any(!is.na(input$example_grid_lb),!is.na(input$example_grid_ub)))) {
      example_grid_bound_check <- "Bounds of grid used for underlying test inversion are only needed for the case when bounding relative magnitudes is included in base Delta. You need to expose [ Step 9 ] by clicking Bounding Relative Magnitudes again, and erase the values of bounds for grid."
      example_grid_bound_js_string <- 'alert("SOMETHING");'
      example_grid_bound_js_string <- sub("SOMETHING",example_grid_bound_check,example_grid_bound_js_string)
      session$sendCustomMessage(type='jsCode', list(value = example_grid_bound_js_string))
    }
  })
  
  # Alert below will trigger if upper bound of grid is negative or lower bound of grid is positive
  observe({
    if (any(all(!is.na(input$example_grid_lb), input$example_grid_lb>0),
            all(!is.na(input$example_grid_ub), input$example_grid_ub<0))) {
      example_grid_sign_check <- "Lower bound of grid used for underlying test inversion should be non-positive, and upper bound should be non-negative."
      example_grid_sign_js_string <- 'alert("SOMETHING");'
      example_grid_sign_js_string <- sub("SOMETHING",example_grid_sign_check,example_grid_sign_js_string)
      session$sendCustomMessage(type='jsCode', list(value = example_grid_sign_js_string))
    }
  })

  # Alert below will trigger if number of grid points is empty
  observe({
    if (is.na(input$example_grid_points)) {
      example_grid_points_check <- "Number of grid points shouldn't be empty."
      example_grid_points_js_string <- 'alert("SOMETHING");'
      example_grid_points_js_string <- sub("SOMETHING",example_grid_points_check,example_grid_points_js_string)
      session$sendCustomMessage(type='jsCode', list(value = example_grid_points_js_string))
    }
  })
  
  
  # ------------ Observe Start or Reset-----------------------------
  example_v <- reactiveValues(result = NULL)
  
  # Observe Start
  observeEvent(input$example_start,execute_safely({
    example_data <- example_dataset[[as.numeric(input$paper)]]
    
    # Report Errors
    # example_sd_upper_mpre <- if (all(length(input$example_delta)==1, input$example_delta == "1", length(example_data$prePeriodIndices) > 1)) {
    #   DeltaSD_upperBound_Mpre(betahat = example_data$beta,
    #                           sigma = example_data$sigma,
    #                           numPrePeriods = length(example_data$prePeriodIndices),
    #                           alpha = input$example_alpha)
    # } else {
    #     if (all(length(input$example_delta)==1, input$example_delta == "1", length(example_data$prePeriodIndices) == 1)) {
    #       sqrt(example_data$sigma[example_data$prePeriodIndices[length(example_data$prePeriodIndices)],
    #                               example_data$prePeriodIndices[length(example_data$prePeriodIndices)]])
    #     }
    #   } 
    
    if (all(input$example_delta != "2", input$example_sign != "1", input$example_monotonicity != "1")) stop("If bounding relative magnitudes is included in base Delta, it is not allowed to select both shape (aka monotonicity) and sign restrictions as additions.")
    if (all(input$example_method_rm %in% c("FLCI", "C-F"), input$example_delta != "2")) stop("Fixed Length Confidence Intervals or Conditional FLCI Hybrid is not suitable for base Delta that includes Bounding Relative Magnitudes, because it is proven that optimal FLCI has infinite length under those restrictions.")
    
    if (any(NA %in% (mod(as.numeric(unlist(strsplit(input$example_m_textinput,","))),1)!=0),any(as.numeric(unlist(strsplit(input$example_m_textinput,",")))<0))) stop("The vector of M must consist of numbers being seperated by commas. Numbers must be inon-negative. Please check examples in hints.")
    if (all(input$example_m_textinput !="", any(!is.na(input$example_m_lower), !is.na(input$example_m_upper), !is.na(input$example_m_step)))) stop("You can enter the vector of M EITHER arbitrarily OR as a sequence, but NOT both.")
    if (any(all(!is.na(input$example_m_lower), input$example_m_lower <0),all(!is.na(input$example_m_upper), input$example_m_upper <0),all(!is.na(input$example_m_step), input$example_m_step <0))) stop("Lower and upper bounds as well as step of the M sequence must be non-negative.")
    if (any(all(!is.na(input$example_m_lower), !is.na(input$example_m_upper), input$example_m_lower > input$example_m_upper),all(!is.na(input$example_m_lower), !is.na(input$example_m_upper), !is.na(input$example_m_step), input$example_m_step > input$example_m_upper - input$example_m_lower))) stop("Lower bound of the M sequence shouldn't be larger than upper bound, and step of the sequence shouldn't be larger than the difference between lower and upper bounds.")
    if (sum(is.na(c(input$example_m_lower, input$example_m_upper, input$example_m_step))) %in% c(1,2)) stop("If you choose to define the vector of M by sequence, you need to fill in all blanks of Lower, Upper and Step. Otherwise, leave them all blank if you decide to define the vector by default.")

    if (any(NA %in% (mod(as.numeric(unlist(strsplit(input$example_mbar_textinput,","))),1)!=0),any(as.numeric(unlist(strsplit(input$example_mbar_textinput,",")))<0))) stop("The vector of Mbar must consist of numbers being seperated by commas. Numbers must be inon-negative. Please check examples in hints.")
    if (all(input$example_mbar_textinput !="", any(!is.na(input$example_mbar_lower), !is.na(input$example_mbar_upper), !is.na(input$example_mbar_step)))) stop("You can enter the vector of Mbar EITHER arbitrarily OR as a sequence, but NOT both.")
    if (any(all(!is.na(input$example_mbar_lower), input$example_mbar_lower <0),all(!is.na(input$example_mbar_upper), input$example_mbar_upper <0),all(!is.na(input$example_mbar_step), input$example_mbar_step <0))) stop("Lower and upper bounds as well as step of the Mbar sequence must be non-negative.")
    if (any(all(!is.na(input$example_mbar_lower), !is.na(input$example_mbar_upper), input$example_mbar_lower > input$example_mbar_upper),all(!is.na(input$example_mbar_lower), !is.na(input$example_mbar_upper), !is.na(input$example_mbar_step), input$example_mbar_step > input$example_mbar_upper - input$example_mbar_lower))) stop("Lower bound of the Mbar sequence shouldn't be larger than upper bound, and step of the sequence shouldn't be larger than the difference between lower and upper bounds.")
    if (sum(is.na(c(input$example_mbar_lower, input$example_mbar_upper, input$example_mbar_step))) %in% c(1,2)) stop("If you choose to define the vector of Mbar by sequence, you need to fill in all blanks of Lower, Upper and Step. Otherwise, leave them all blank if you decide to define the vector by default.")

    if (NA %in% (mod(as.numeric(unlist(strsplit(input$example_lvec,","))),1)!=0)) stop("The vector that is used to determine parameter of interest must consist of numbers, instead of characters")
    if (any(mod(as.numeric(unlist(strsplit(input$example_lvec,","))),1)!=0)) stop("The vector that is used to determine parameter of interest must consist of integers.")
    if (any(as.numeric(unlist(strsplit(input$example_lvec,",")))<1)) stop("The vector that is used to determine parameter of interest must consist of integers that are not less than 1.")
    if (max(as.numeric(unlist(strsplit(input$example_lvec,","))))>length(example_data$postPeriodIndices)) stop("Numbers in the vector that is used to determine parameter of interest shouldn't be larger than the total length of post treatment periods.")
    if (all(input$example_delta == "2", any(!is.na(input$example_grid_lb),!is.na(input$example_grid_ub)))) stop("Bounds of grid used for underlying test inversion are only needed for the case when bounding relative magnitudes is included in base Delta. You need to expose [ Step 9 ] by clicking Bounding Relative Magnitudes again, and erase the values of bounds for grid.")
    if (any(all(!is.na(input$example_grid_lb), input$example_grid_lb>0), all(!is.na(input$example_grid_ub), input$example_grid_ub<0))) stop("Lower bound of grid used for underlying test inversion should be non-positive, and upper bound should be non-negative.")
    if (is.na(input$example_grid_points)) stop("Number of grid points shouldn't be empty.")
    if (all(!is.na(input$example_grid_points),any(mod(input$example_grid_points,1)!=0,input$example_grid_points < 1))) stop("Number of grid points should be positive integer.")
    
    # Calculation
    if (input$example_lvec=="") {
      example_lvec <- unique(as.numeric(strsplit(input$example_lvec,",")[[1]]))
      example_weight <- rep(1,length(example_data$postPeriodIndices))/length(example_data$postPeriodIndices)
      } else {
      example_lvec <- unique(as.numeric(strsplit(input$example_lvec,",")[[1]]))
      example_weight <- rep(0,length(example_data$postPeriodIndices))
      example_weight[example_lvec] <- 1
      example_weight <- example_weight/length(example_lvec)
      }
    
    example_sign <- if (input$example_sign=="2") "positive" else {if(input$example_sign =="3") "negative"}
    example_monotonicity <- if (input$example_monotonicity == "2") "increasing" else {if(input$example_monotonicity =="3") "decreasing"}
    example_method_delta <- if (input$example_delta != "2") input$example_method_rm else input$example_method_sd

    example_m_vector <- if (input$example_m_textinput !="") {
      unique(as.numeric(strsplit(input$example_m_textinput,",")[[1]]))
      } else {
        if (all(input$example_m_textinput =="", sum(is.na(c(input$example_m_lower, input$example_m_upper, input$example_m_step)))==0)) {
          seq(from= input$example_m_lower, to= input$example_m_upper, by= input$example_m_step)
          }
        }
    
    example_mbar_vector <- if (input$example_mbar_textinput !="") {
      unique(as.numeric(strsplit(input$example_mbar_textinput,",")[[1]]))
      } else {
        if (all(input$example_mbar_textinput =="", sum(is.na(c(input$example_mbar_lower, input$example_mbar_upper, input$example_mbar_step)))==0)) {
          seq(from= input$example_mbar_lower, to= input$example_mbar_upper, by= input$example_mbar_step)
        } else {
            seq(from=0.25, to=1.25, by=0.25) # Default vector of mbar is seq(0.25,1.25,0.25)
          }
        }
    
    example_mmbar_vector <- if (input$example_delta != "2") example_mbar_vector else example_m_vector

    # original OLS betahat
    example_originalResults <- constructOriginalCS(
      betahat = example_data$beta, 
      sigma = example_data$sigma,
      numPrePeriods = length(example_data$prePeriodIndices), 
      numPostPeriods = length(example_data$postPeriodIndices), 
      l_vec = example_weight, 
      alpha = input$example_alpha
      )

    if (input$example_delta == "1") {
      example_delta_rm_results <- createSensitivityResults_relativeMagnitudes(
        betahat = example_data$beta, 
        sigma = example_data$sigma, 
        numPrePeriods = length(example_data$prePeriodIndices), 
        numPostPeriods = length(example_data$postPeriodIndices), 
        bound = "deviation from parallel trends",
        method = example_method_delta, 
        Mbarvec = example_mmbar_vector,  
        l_vec = example_weight, 
        biasDirection = example_sign, 
        monotonicityDirection = example_monotonicity, 
        alpha = input$example_alpha,
        parallel = as.logical(input$example_parallel),
        gridPoints = input$example_grid_points, 
        grid.lb = input$example_grid_lb, 
        grid.ub = input$example_grid_ub 
        )  
      example_v$result<-list(data=example_data,lvec=example_lvec,delta_rm_results=example_delta_rm_results, originalResults=example_originalResults, 
                             mmbar=example_mmbar_vector,weight=example_weight, sign=example_sign, monotonicity=example_monotonicity, paper = input$paper, alpha=input$example_alpha, method=example_method_delta, delta=input$example_delta, grid_points=input$example_grid_points, grid_lb=input$example_grid_lb, grid_ub=input$example_grid_ub, parallel=as.logical(input$example_parallel)) 
      } else {
        if (input$example_delta == "2") {
          example_delta_sd_results <- createSensitivityResults(
            betahat = example_data$beta, 
            sigma = example_data$sigma, 
            numPrePeriods = length(example_data$prePeriodIndices), 
            numPostPeriods = length(example_data$postPeriodIndices), 
            method = example_method_delta, 
            Mvec = example_mmbar_vector,  
            l_vec = example_weight, 
            monotonicityDirection = example_monotonicity, 
            biasDirection = example_sign, 
            alpha = input$example_alpha,
            parallel = as.logical(input$example_parallel)
            ) 
          example_v$result<-list(data=example_data,lvec=example_lvec,delta_sd_results=example_delta_sd_results, originalResults=example_originalResults, 
                                 mmbar=example_mmbar_vector, weight=example_weight, sign=example_sign, monotonicity=example_monotonicity, paper = input$paper, alpha=input$example_alpha, method=example_method_delta, delta=input$example_delta, parallel=as.logical(input$example_parallel))
          } else {
            example_delta_rm_results <- createSensitivityResults_relativeMagnitudes(
              betahat = example_data$beta, 
              sigma = example_data$sigma, 
              numPrePeriods = length(example_data$prePeriodIndices), 
              numPostPeriods = length(example_data$postPeriodIndices), 
              bound = "deviation from linear trend", 
              method = example_method_delta,
              Mbarvec = example_mmbar_vector, 
              l_vec = example_weight, 
              biasDirection = example_sign, 
              monotonicityDirection = example_monotonicity, 
              alpha = input$example_alpha,
              parallel = as.logical(input$example_parallel),
              gridPoints = input$example_grid_points,  
              grid.lb = input$example_grid_lb,
              grid.ub = input$example_grid_ub 
              ) 
            example_v$result<-list(data=example_data,lvec=example_lvec,delta_rm_results=example_delta_rm_results, originalResults=example_originalResults, 
                                   mmbar=example_mmbar_vector,weight=example_weight, sign=example_sign, monotonicity=example_monotonicity, paper = input$paper, alpha=input$example_alpha, method=example_method_delta, delta=input$example_delta, grid_points=input$example_grid_points, grid_lb=input$example_grid_lb, grid_ub=input$example_grid_ub, parallel=as.logical(input$example_parallel))  
          }
        }
    }))

  # Observe Reset
  observeEvent(input$example_reset, {
    example_v$result <- NULL
    reset("paper")
    reset("example_method")
    reset("example_delta")
    reset("example_sign")
    reset("example_monotonicity")
    reset("example_method_sd")
    reset("example_method_rm")
    reset("example_m_textinput")
    reset("example_m_lower")
    reset("example_m_upper")
    reset("example_m_step")
    reset("example_mbar_textinput")
    reset("example_mbar_lower")
    reset("example_mbar_upper")
    reset("example_mbar_step")
    reset("example_lvec")
    reset("example_alpha")
    reset("example_parallel")
    reset("example_grid_lb")
    reset("example_grid_ub")
    reset("example_grid_points")
  })
  
  # Observe Disconnect
  observeEvent(input$example_disconnect, {
    session$close()
  })
  
  # ------------ Outputs -----------------------------
  # Alert below will trigger if lower bounds of confidence sets are exactly the same for different Mbar
  observe({
    if (all(example_v$result$delta != "2",
            any(duplicated(example_v$result$delta_rm_results$lb)))) {
      example_grid_lb_error_check <- "WARNING: It is detected that the lower bounds of confidence sets at two or more different Mbar are exactly the same. This is due to the fact that the lower bound of grid used for underlying test inversion is too large. You need to decrease it on the left pane to get correct confidence sets. Please go to [ More ] > [ FAQ ] for details on how the lower bound of grid used for test inversion is defined by default."
      example_grid_lb_error_js_string <- 'alert("SOMETHING");'
      example_grid_lb_error_js_string <- sub("SOMETHING",example_grid_lb_error_check,example_grid_lb_error_js_string)
      session$sendCustomMessage(type='jsCode', list(value = example_grid_lb_error_js_string))
    }
  })
  
  # Alert below will trigger if upper bounds of confidence sets are exactly the same for different Mbar
  observe({
    if (all(example_v$result$delta != "2",
            any(duplicated(example_v$result$delta_rm_results$ub)))) {
      example_grid_ub_error_check <- "WARNING: It is detected that the upper bounds of confidence sets at two or more different Mbar are exactly the same. This is due to the fact that the upper bound of grid used for underlying test inversion is too small. You need to increase it on the left pane to get correct confidence sets. Please go to [ More ] > [ FAQ ] for details on how the upper bound of grid used for test inversion is defined by default."
      example_grid_ub_error_js_string <- 'alert("SOMETHING");'
      example_grid_ub_error_js_string <- sub("SOMETHING",example_grid_ub_error_check,example_grid_ub_error_js_string)
      session$sendCustomMessage(type='jsCode', list(value = example_grid_ub_error_js_string))
    }
  })

  # Clock
  output$example_currentTime <- renderText({
    invalidateLater(1000, session)
    example_tz <- Sys.timezone(location = TRUE)
    example_tz <- if (class(example_tz)!="character") "UTC" else example_tz
    # paste("[", lubridate::with_tz(Sys.time(), example_tz),"]")
    paste("[", Sys.time(), example_tz, "]")
  })
  
  # hide or show step of grid used for test inversion
  observeEvent(input$example_delta,  {
    if(input$example_delta != "2") {
      show("example_grid_hint")
      show("example_grid_note")
      show("example_grid_lb")
      show("example_grid_ub")
      show("example_grid_points")
    }
  })
  
  observeEvent(input$example_delta,  {
    if(input$example_delta == "2") {
      hide("example_grid_hint")
      hide("example_grid_note")
      hide("example_grid_lb")
      hide("example_grid_ub")
      hide("example_grid_points")
    }
  })

  # Event Study Plot
  example_betaplot <- reactive({
    if (is.null(example_v$result)) return()
    createEventStudyPlot(betahat = example_v$result$data$beta,
                         sigma = example_v$result$data$sigma,
                         numPrePeriods = length(example_v$result$data$prePeriodIndices),
                         numPostPeriods = length(example_v$result$data$postPeriodIndices),
                         alpha = example_v$result$alpha,
                         timeVec = example_v$result$data$tVec,
                         referencePeriod = example_v$result$data$referencePeriod,
                         useRelativeEventTime = T) + 
      labs(caption = paste(example_v$result$data$paper_formatted,": This plot is in relative event time, which normalizes the reference period to be 0.", sep=""))
    })
  
  output$example_betaplot <- renderPlot({
    input$example_start
    example_betaplot()
  })
  
  # Sensitivity Plot
  example_ssplot <- reactive({
    if (is.null(example_v$result)) return()
    
    plot <-  if(example_v$result$delta == "2") {
      createSensitivityPlot(example_v$result$delta_sd_results, example_v$result$originalResults)
      } else {
        createSensitivityPlot_relativeMagnitudes(example_v$result$delta_rm_results, example_v$result$originalResults)
      }
    
    target <- if (length(example_v$result$lvec) == 0) {
      paste(": Parameter of Interest: Average Causal Effect over all",
            length(example_v$result$data$postPeriodIndices) ,"Post-Treatment Periods. \n")
      } else {
        if (length(example_v$result$lvec) == 1) {
          paste(": Parameter of Interest: Causal Effect at the",
                case_when(example_v$result$lvec == 1 ~ "1st",
                          example_v$result$lvec == 2 ~"2nd",
                          example_v$result$lvec == 3 ~ "3rd",
                          TRUE ~ paste(example_v$result$lvec,"th",sep="")),
                "Period after treatment. \n")
          } else {
            paste(": Parameter of Interest: Average Causal Effect over Periods",
                  paste(example_v$result$lvec, collapse = ",") ,"after treatment. \n")
          }
        }
    plot + labs(caption = paste(example_v$result$data$paper_formatted,target, sep=""))
  })
  
  output$example_ssplot <- renderPlot({
    input$example_start
    example_ssplot()
  })
  
  # Sensitivity Note
  output$example_note <- renderText({
    input$example_start
    if (is.null(example_v$result)) return()
    "\n General Rule: In sensitivity plot, if the confidence interval under certain degree of violation on parallel trends doesn't cross 0, then we say, the paramter of interest is significant under that certain M or Mbar. Otherwise, insignificant."
    })
  
  # Grid Warning
  output$example_grid_warning <- renderText({
    input$example_start
    if (is.null(example_v$result)) return()
    example_gridwarning <- if (all(example_v$result$delta != "2", any(duplicated(example_v$result$delta_rm_results$lb)))) {
      "\n It is detected that the lower bounds of confidence sets at two or more different Mbar are exactly the same. This is due to the fact that the lower bound of grid used for underlying test inversion is too large. You need to decrease it on the left pane to get correct confidence sets. Please go to [ More ] > [ FAQ ] for details on how the lower bound of grid used for test inversion is defined by default."
    } else {
      if ((all(example_v$result$delta != "2", any(duplicated(example_v$result$delta_rm_results$ub))))) {
        "\n It is detected that the upper bounds of confidence sets at two or more different Mbar are exactly the same. This is due to the fact that the upper bound of grid used for underlying test inversion is too small. You need to increase it on the left pane to get correct confidence sets. Please go to [ More ] > [ FAQ ] for details on how the upper bound of grid used for test inversion is defined by default."
      } else {
        if (all(example_v$result$delta != "2", any(duplicated(example_v$result$delta_rm_results$lb)), any(duplicated(example_v$result$delta_rm_results$ub)))) {
          "\n It is detected that the bounds of confidence sets at two or more different Mbar are exactly the same. This is due to the fact that the lower (upper) bound of grid used for underlying test inversion is too large (small). You need to decrease (increase) it on the left pane to get correct confidence sets. Please go to [ More ] > [ FAQ ] for details on how bounds of grid used for test inversion is defined by default."
        } else {NULL}
      }
    }
    paste(example_gridwarning)
  })
  
  # Sensitivity Data
  example_ssdata <- reactive({
    if (is.null(example_v$result)) return()
    if(example_v$result$delta == "2") {   
      example_v$result$delta_sd_results
    } else {
      example_v$result$delta_rm_results
    }
  })
  
  output$example_ssdata <- renderTable({
    input$example_start
    example_ssdata()},  
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE,
    width = "100%",
    digits = 5
    )
  
  # Data File
  example_beta <- reactive({
    if (is.null(example_v$result)) return()
    t(example_v$result$data$beta)
  })
  
  example_sigma <- reactive({
    if (is.null(example_v$result)) return()
    example_v$result$data$sigma
  })
  
  example_tVec <- reactive({
    if (is.null(example_v$result)) return()
    t(example_v$result$data$tVec)
  })
  
  example_ref <- reactive({
    if (is.null(example_v$result)) return()
    t(example_v$result$data$referencePeriod)
  })
  
  example_pre <- reactive({
    if (is.null(example_v$result)) return()
    t(example_v$result$data$prePeriodIndices)
  })
  
  example_post <- reactive({
    if (is.null(example_v$result)) return()
    t(example_v$result$data$postPeriodIndices)
  })
  
  # R Code
  example_code <- reactive({
    if (is.null(example_v$result)) return()
    
    head <- c(" # Install HonestDiD package from https://github.com/asheshrambachan/HonestDiD, if not installed \n",
              "# install.packages('remotes') \n",
              "# Sys.setenv('R_REMOTES_NO_ERRORS_FROM_WARNINGS' = 'true') \n",
              "# remotes::install_github('asheshrambachan/HonestDiD') \n",
              "library(HonestDiD) \n \n",
              "options(warn = -1) \n \n")
    
    dataset <- c("# Import Example Data \n",
                 "# Dataset could be downloaded from https://github.com/ccfang2/HonestDiDSenAnlys \n",
                 "dataset <- readRDS('data/ResultsObjectList.rds') \n",
                 "dataset[[13]]<-BCdata_EventStudy \n",
                 "dataset[[14]]<-LWdata_EventStudy \n",
                 "names(dataset[[13]])<-c('beta','sigma','tVec','referencePeriod','prePeriodIndices','postPeriodIndices') \n",
                 "dataset[[13]]$paper_formatted <- 'Benzarti and Carloni (2019)' \n",
                 "names(dataset[[14]])<-c('beta','sigma','tVec','referencePeriod','prePeriodIndices','postPeriodIndices','stdErrors') \n",
                 "dataset[[14]]$paper_formatted <- 'Lovenheim & Willen (2019)' \n",
                 "names(dataset)<-1:14 \n",
                 "dataset<-dataset[c(1,13,2,4:9,14,10:12,3)] \n",
                 "names(dataset) <- NULL \n",
                 "data <- dataset[[as.numeric(",example_v$result$paper,")]] \n \n")
    
    eventPlot <- c("# Create Event Study Plot \n",
                   "event_study_plot <- createEventStudyPlot( \n",
                   "\40 \40 betahat = data$beta, \n",
                   "\40 \40 sigma = data$sigma, \n",
                   "\40 \40 numPrePeriods = length(data$prePeriodIndices), \n",
                   "\40 \40 numPostPeriods = length(data$postPeriodIndices), \n",
                   "\40 \40 alpha = ", example_v$result$alpha, ", \n",
                   "\40 \40 timeVec = data$tVec, \n",
                   "\40 \40 referencePeriod = data$referencePeriod, \n",
                   "\40 \40 useRelativeEventTime = TRUE \n",
                   ") \n",
                   "event_study_plot \n \n")
    
    originalCS <- c("# Construct Original Confidence Set \n",
                    "originalResults <- constructOriginalCS( \n",
                    "\40 \40 betahat = data$beta, \n",
                    "\40 \40 sigma = data$sigma, \n",
                    "\40 \40 numPrePeriods = length(data$prePeriodIndices), \n",
                    "\40 \40 numPostPeriods = length(data$postPeriodIndices), \n",
                    "\40 \40 l_vec = c(",paste(example_v$result$weight, collapse = ","),"), \n",
                    "\40 \40 alpha = ",example_v$result$alpha, "\n",
                    ") \n \n")
    
    sensitivityAnalysis <- if (example_v$result$delta == "1") {
      c("# Create Sensitivity Result \n",
        "delta_rm_results <- createSensitivityResults_relativeMagnitudes( \n",
        "\40 \40 betahat = data$beta, \n",
        "\40 \40 sigma = data$sigma, \n",
        "\40 \40 numPrePeriods = length(data$prePeriodIndices), \n",
        "\40 \40 numPostPeriods = length(data$postPeriodIndices), \n",
        "\40 \40 bound = 'deviation from parallel trends', \n",
        paste("\40 \40 method = '",example_v$result$method,"'", sep=""),", \n",
        "\40 \40 Mbarvec = ", if (is.null(example_v$result$mmbar)) NULL else paste("c(",paste(example_v$result$mmbar, collapse=","),")", sep=""), ", \n",  
        "\40 \40 l_vec = c(", paste(example_v$result$weight, collapse = ","), "), \n",
        "\40 \40 biasDirection = ", example_v$result$sign,", \n",
        "\40 \40 monotonicityDirection = ",example_v$result$monotonicity,", \n",
        "\40 \40 alpha = ", example_v$result$alpha,", \n",
        "\40 \40 parallel = ", example_v$result$parallel,", \n",
        "\40 \40 gridPoints = ",example_v$result$grid_points,", \n",  
        "\40 \40 grid.lb = ",example_v$result$grid_lb,", \n", 
        "\40 \40 grid.ub = ",example_v$result$grid_ub,"\n",  
        ") \n \n")
      } else {
        if (example_v$result$delta == "2") {
          c("# Create Sensitivity Result \n",
            "delta_sd_results <- createSensitivityResults( \n",
            "\40 \40 betahat = data$beta, \n",
            "\40 \40 sigma = data$sigma, \n",
            "\40 \40 numPrePeriods = length(data$prePeriodIndices), \n",
            "\40 \40 numPostPeriods = length(data$postPeriodIndices), \n",
            paste("\40 \40 method = '",example_v$result$method,"'", sep=""),", \n",
            "\40 \40 Mvec = ", if (is.null(example_v$result$mmbar)) NULL else paste("c(",paste(example_v$result$mmbar, collapse=","),")", sep=""), ", \n",  
            "\40 \40 l_vec = c(", paste(example_v$result$weight, collapse = ","), "), \n",
            "\40 \40 biasDirection = ", example_v$result$sign,", \n",
            "\40 \40 monotonicityDirection = ", example_v$result$monotonicity,", \n",
            "\40 \40 alpha = ", example_v$result$alpha,", \n",
            "\40 \40 parallel = ", example_v$result$parallel,"\n",
            ") \n \n")
        } else {
          c("# Create Sensitivity Result \n",
            "delta_rm_results <- createSensitivityResults_relativeMagnitudes( \n",
            "\40 \40 betahat = data$beta, \n",
            "\40 \40 sigma = data$sigma, \n",
            "\40 \40 numPrePeriods = length(data$prePeriodIndices), \n",
            "\40 \40 numPostPeriods = length(data$postPeriodIndices), \n",
            "\40 \40 bound = 'deviation from linear trend', \n",
            paste("\40 \40 method = '",example_v$result$method,"'", sep=""),", \n",
            "\40 \40 Mbarvec = ", if (is.null(example_v$result$mmbar)) NULL else paste("c(",paste(example_v$result$mmbar, collapse=","),")", sep=""), ", \n",  
            "\40 \40 l_vec = c(", paste(example_v$result$weight, collapse = ","), "), \n",
            "\40 \40 biasDirection = ", example_v$result$sign,", \n",
            "\40 \40 monotonicityDirection = ", example_v$result$monotonicity,", \n",
            "\40 \40 alpha = ", example_v$result$alpha,", \n",
            "\40 \40 parallel = ", example_v$result$parallel,", \n",
            "\40 \40 gridPoints = ",example_v$result$grid_points,", \n",  
            "\40 \40 grid.lb = ",example_v$result$grid_lb,", \n", 
            "\40 \40 grid.ub = ",example_v$result$grid_ub,"\n",  
            ") \n \n")
        }
      }
        
    plot <- if(example_v$result$delta == "2") {
      c("# Create Sensitivity Plot \n",
        "sensitivity_plot <- createSensitivityPlot(delta_sd_results, originalResults) \n",
        "sensitivity_plot \n \n")
      } else {
        c("# Create Sensitivity Plot \n",
          "sensitivity_plot <- createSensitivityPlot_relativeMagnitudes(delta_rm_results, originalResults) \n",
          "sensitivity_plot \n \n")
        }
    
    tail <- c("options(warn = 0) \n")
    
    paste0(c(head, dataset, eventPlot, originalCS, sensitivityAnalysis, plot, tail), sep="")
  })
  
  output$example_code <- renderText({
    input$example_start
    example_code()
  })
  
  # ------------ Downloads -----------------------------
  # event-study plot download
  output$example_betaplotdownload <- downloadHandler(
    filename <- function(){
      paste("Event_Study_Plot.png",sep="")
      },
    content <- function(file){
      ggsave(file, plot=example_betaplot(),device = "png")
      }
    )
  
  # sensitivity plot download
  output$example_ssplotdownload <- downloadHandler(
    filename <- function(){
      paste("Sensitivity_Plot.png",sep="")
      },
    content <- function(file){
      ggsave(file, plot=example_ssplot(),device = "png")
      }
    )
  
  # sensitivity data download
  output$example_ssdatadownload <- downloadHandler(
    filename = function(){
      paste("Sensitivity_Result.xlsx",sep="")
      }, 
    content = function(file){
      WriteXLS(list(Sensitivity_Result=as.data.frame(example_ssdata())), file)
      }
    )
  
  # data files download
  output$example_inputdownload <- downloadHandler(
    filename = function(){
      paste("Data_Files_Examples.xlsx",sep="")
      }, 
    content = function(file){
      WriteXLS(list(beta = as.data.frame(example_beta()), 
                    sigma = as.data.frame(example_sigma()),
                    tVec = as.data.frame(example_tVec()),
                    referencePeriod = as.data.frame(example_ref()),
                    prePeriodIndices = as.data.frame(example_pre()),
                    postPeriodIndices = as.data.frame(example_post())
      ), file)
      }
    )
  
  
  # *****************************************************************
  # ************* Own Data Tab **************************************
  # *****************************************************************
  
  # ------------ Alerts----------------------------------------------
  # Alert below will trigger if both optional restrictions are selected when Bounding Relative Magnitudes is included in base Delta
  observe({
    if (all(input$owndata_delta != "2", input$owndata_sign != "1", input$owndata_monotonicity != "1")) {
      owndata_optional_delta_check <- "If bounding relative magnitudes is included in base Delta, it is not allowed to select both shape (aka monotonicity) and sign restrictions as additions."
      owndata_optional_delta_js_string <- 'alert("SOMETHING");'
      owndata_optional_delta_js_string <- sub("SOMETHING",owndata_optional_delta_check,owndata_optional_delta_js_string)
      session$sendCustomMessage(type='jsCode', list(value = owndata_optional_delta_js_string))
    }
  })
  
  # Alert below will trigger if FLCI or Conditional FLCI is chosen for any base restrictions other than single SD
  observe({
    if (all(input$owndata_method_rm %in% c("FLCI", "C-F"), 
            input$owndata_delta != "2")) {
      owndata_flci_check <- "Fixed Length Confidence Intervals or Conditional FLCI Hybrid is not suitable for base Delta that includes Bounding Relative Magnitudes, because it is proven that optimal FLCI has infinite length under those restrictions."
      owndata_flci_js_string <- 'alert("SOMETHING");'
      owndata_flci_js_string <- sub("SOMETHING",owndata_flci_check,owndata_flci_js_string)
      session$sendCustomMessage(type='jsCode', list(value = owndata_flci_js_string))
    }
  })
  
  # Alert below will trigger if file uploaded is not excel
  resetOwnDataFileUpload <- reactiveVal(FALSE)
  output$owndata_fileupload <- renderUI({
    resetOwnDataFileUpload() # reactive dependency
    resetOwnDataFileUpload(FALSE)
    fileInput("owndata_file", 
              p("Upload your own data in xlsx format", span(a("[ Example File ]", href="https://raw.githubusercontent.com/ccfang2/HonestDiDSenAnlys/main/SenAnlysDataFormat.xlsx"))),
              accept = ".xlsx")
  })
  
  observeEvent(input$owndata_file, {
    if(file_ext(input$owndata_file$name) != "xlsx"){
      resetOwnDataFileUpload(TRUE)
      showModal(modalDialog("That's not an .xlsx file"))
    }
  })
  
  # Alert below will trigger if l vector is wrongly formatted
  observe({
    if (
      any(NA %in% (mod(as.numeric(unlist(strsplit(input$owndata_lvec,","))),1)!=0), 
          any(mod(as.numeric(unlist(strsplit(input$owndata_lvec,","))),1)!=0),
          any(as.numeric(unlist(strsplit(input$owndata_lvec,",")))<1))
    ) {
      owndata_integer_check <- "If any, the vector that is used to determine parameter of interest must consist of numbers being seperated by commas. Numbers must be integers that are not less than 1. Please check examples in hints."
      owndata_integer_js_string <- 'alert("SOMETHING");'
      owndata_integer_js_string <- sub("SOMETHING",owndata_integer_check,owndata_integer_js_string)
      session$sendCustomMessage(type='jsCode', list(value = owndata_integer_js_string))
    }
  })

  # Alert below will trigger if textinput m vector is wrongly formatted
  observe({
    if (
      any(NA %in% (mod(as.numeric(unlist(strsplit(input$owndata_m_textinput,","))),1)!=0),
          any(as.numeric(unlist(strsplit(input$owndata_m_textinput,",")))<0))
    ) {
      owndata_m_textinput_check <- "The vector of M must consist of numbers being seperated by commas. Numbers must be non-negative. Please check examples in hints."
      owndata_m_textinput_js_string <- 'alert("SOMETHING");'
      owndata_m_textinput_js_string <- sub("SOMETHING",owndata_m_textinput_check,owndata_m_textinput_js_string)
      session$sendCustomMessage(type='jsCode', list(value = owndata_m_textinput_js_string))
    }
  })
  
  # Alert below will trigger if both textinput and sequence m vector are defined
  observe({
    if (
      all(input$owndata_m_textinput !="",
          any(!is.na(input$owndata_m_lower), !is.na(input$owndata_m_upper), !is.na(input$owndata_m_step)))
    ) {
      owndata_m_text_seq_check <- "You can enter the vector of M EITHER arbitrarily OR as a sequence, but NOT both."
      owndata_m_text_seq_js_string <- 'alert("SOMETHING");'
      owndata_m_text_seq_js_string <- sub("SOMETHING",owndata_m_text_seq_check,owndata_m_text_seq_js_string)
      session$sendCustomMessage(type='jsCode', list(value = owndata_m_text_seq_js_string))
    }
  })
  
  # Alert below will trigger if any of lower, upper, step in the sequence m vector is negative 
  observe({
    if (any(all(!is.na(input$owndata_m_lower), input$owndata_m_lower <0),
            all(!is.na(input$owndata_m_upper), input$owndata_m_upper <0),
            all(!is.na(input$owndata_m_step), input$owndata_m_step <0))) {
      owndata_m_seq_neg_check <- "Lower and upper bounds as well as step of the M sequence must be non-negative."
      owndata_m_seq_neg_js_string <- 'alert("SOMETHING");'
      owndata_m_seq_neg_js_string <- sub("SOMETHING",owndata_m_seq_neg_check,owndata_m_seq_neg_js_string)
      session$sendCustomMessage(type='jsCode', list(value = owndata_m_seq_neg_js_string))
    }
  })
  
  # Alert below will trigger if any of lower, upper, step in the sequence m vector is wrongly defined
  observe({
    if (any(all(!is.na(input$owndata_m_lower), !is.na(input$owndata_m_upper), input$owndata_m_lower > input$owndata_m_upper),
            all(!is.na(input$owndata_m_lower), !is.na(input$owndata_m_upper), !is.na(input$owndata_m_step), input$owndata_m_step > input$owndata_m_upper - input$owndata_m_lower))) {
      owndata_m_seq_step_check <- "Lower bound of the M sequence shouldn't be larger than upper bound, and step of the sequence shouldn't be larger than the difference between lower and upper bounds."
      owndata_m_seq_step_js_string <- 'alert("SOMETHING");'
      owndata_m_seq_step_js_string <- sub("SOMETHING",owndata_m_seq_step_check,owndata_m_seq_step_js_string)
      session$sendCustomMessage(type='jsCode', list(value = owndata_m_seq_step_js_string))
    }
  })
  
  # Alert below will trigger if textinput mbar vector is wrongly formatted
  observe({
    if (
      any(NA %in% (mod(as.numeric(unlist(strsplit(input$owndata_mbar_textinput,","))),1)!=0),
          any(as.numeric(unlist(strsplit(input$owndata_mbar_textinput,",")))<0))
    ) {
      owndata_mbar_textinput_check <- "The vector of Mbar must consist of numbers being seperated by commas. Numbers must be non-negative. Please check examples in hints."
      owndata_mbar_textinput_js_string <- 'alert("SOMETHING");'
      owndata_mbar_textinput_js_string <- sub("SOMETHING",owndata_mbar_textinput_check,owndata_mbar_textinput_js_string)
      session$sendCustomMessage(type='jsCode', list(value = owndata_mbar_textinput_js_string))
    }
  })
  
  # Alert below will trigger if both textinput and sequence mbar vector are defined
  observe({
    if (
      all(input$owndata_mbar_textinput !="",
          any(!is.na(input$owndata_mbar_lower), !is.na(input$owndata_mbar_upper), !is.na(input$owndata_mbar_step)))
    ) {
      owndata_mbar_text_seq_check <- "You can enter the vector of Mbar EITHER arbitrarily OR as a sequence, but NOT both."
      owndata_mbar_text_seq_js_string <- 'alert("SOMETHING");'
      owndata_mbar_text_seq_js_string <- sub("SOMETHING",owndata_mbar_text_seq_check,owndata_mbar_text_seq_js_string)
      session$sendCustomMessage(type='jsCode', list(value = owndata_mbar_text_seq_js_string))
    }
  })
  
  # Alert below will trigger if any of lower, upper, step in the sequence mbar vector is negative 
  observe({
    if (any(all(!is.na(input$owndata_mbar_lower), input$owndata_mbar_lower <0),
            all(!is.na(input$owndata_mbar_upper), input$owndata_mbar_upper <0),
            all(!is.na(input$owndata_mbar_step), input$owndata_mbar_step <0))) {
      owndata_mbar_seq_neg_check <- "Lower and upper bounds as well as step of the Mbar sequence must be non-negative."
      owndata_mbar_seq_neg_js_string <- 'alert("SOMETHING");'
      owndata_mbar_seq_neg_js_string <- sub("SOMETHING",owndata_mbar_seq_neg_check,owndata_mbar_seq_neg_js_string)
      session$sendCustomMessage(type='jsCode', list(value = owndata_mbar_seq_neg_js_string))
    }
  })
  
  # Alert below will trigger if any of lower, upper, step in the sequence mbar vector is wrongly defined
  observe({
    if (any(all(!is.na(input$owndata_mbar_lower), !is.na(input$owndata_mbar_upper), input$owndata_mbar_lower > input$owndata_mbar_upper),
            all(!is.na(input$owndata_mbar_lower), !is.na(input$owndata_mbar_upper), !is.na(input$owndata_mbar_step), input$owndata_mbar_step > input$owndata_mbar_upper - input$owndata_mbar_lower))) {
      owndata_mbar_seq_step_check <- "Lower bound of the Mbar sequence shouldn't be larger than upper bound, and step of the sequence shouldn't be larger than the difference between lower and upper bounds."
      owndata_mbar_seq_step_js_string <- 'alert("SOMETHING");'
      owndata_mbar_seq_step_js_string <- sub("SOMETHING",owndata_mbar_seq_step_check,owndata_mbar_seq_step_js_string)
      session$sendCustomMessage(type='jsCode', list(value = owndata_mbar_seq_step_js_string))
    }
  })

  # Alert below will trigger if bounds of grid used for test inversion is filled when boudning relative magnitudes is not included in base Delta
  observe({
    if (all(input$owndata_delta == "2", any(!is.na(input$owndata_grid_lb),!is.na(input$owndata_grid_ub)))) {
      owndata_grid_bound_check <- "Bounds of grid used for underlying test inversion are only needed for the case when bounding relative magnitudes is included in base Delta. You need to expose [ Step 9 ] by clicking Bounding Relative Magnitudes again, and erase the values of bounds for grid."
      owndata_grid_bound_js_string <- 'alert("SOMETHING");'
      owndata_grid_bound_js_string <- sub("SOMETHING",owndata_grid_bound_check,owndata_grid_bound_js_string)
      session$sendCustomMessage(type='jsCode', list(value = owndata_grid_bound_js_string))
    }
  })
  
  # Alert below will trigger if upper bound of grid is negative or lower bound of grid is positive
  observe({
    if (any(all(!is.na(input$owndata_grid_lb), input$owndata_grid_lb>0),
            all(!is.na(input$owndata_grid_ub), input$owndata_grid_ub<0))) {
      owndata_grid_sign_check <- "Lower bound of grid used for underlying test inversion should be non-positive, and upper bound should be non-negative."
      owndata_grid_sign_js_string <- 'alert("SOMETHING");'
      owndata_grid_sign_js_string <- sub("SOMETHING",owndata_grid_sign_check,owndata_grid_sign_js_string)
      session$sendCustomMessage(type='jsCode', list(value = owndata_grid_sign_js_string))
    }
  })
  
  # Alert below will trigger if number of grid points is empty
  observe({
    if (is.na(input$owndata_grid_points)) {
      owndata_grid_points_check <- "Number of grid points shouldn't be empty."
      owndata_grid_points_js_string <- 'alert("SOMETHING");'
      owndata_grid_points_js_string <- sub("SOMETHING",owndata_grid_points_check,owndata_grid_points_js_string)
      session$sendCustomMessage(type='jsCode', list(value = owndata_grid_points_js_string))
    }
  })

  # ------------ Observe Start or Reset-----------------------------
  owndata_v <- reactiveValues(result = NULL)
  
  # Read in Excel Data
  read_data <- reactive({
    ext <- tools::file_ext(input$owndata_file$datapath)
    req(input$owndata_file)
    validate(need(ext == "xlsx", "Please upload an xlsx file"))
    return(multiplesheets(input$owndata_file$datapath))
  })
  
  # Observe Start
  observeEvent(input$owndata_start,execute_safely({
    
    # Check if data file is missing
    if (length(input$owndata_file)==0) stop("Please upload an .xlsx data file.")
    
    # Reformat Data
    owndata_data <- c(list(sigma=as.matrix(read_data()$sigma)), 
                      lapply(read_data()[c("beta", "tVec", "referencePeriod", "prePeriodIndices", "postPeriodIndices")], unlist))
    
    # Report Errors
    # owndata_sd_upper_mpre <- if (all(length(input$owndata_delta)==1, input$owndata_delta == "1", length(owndata_data$prePeriodIndices) > 1)) {
    #   DeltaSD_upperBound_Mpre(betahat = owndata_data$beta,
    #                           sigma = owndata_data$sigma,
    #                           numPrePeriods = length(owndata_data$prePeriodIndices),
    #                           alpha = input$owndata_alpha)
    # } else {
    #   if (all(length(input$owndata_delta)==1, input$owndata_delta == "1", length(owndata_data$prePeriodIndices) == 1)) {
    #     sqrt(owndata_data$sigma[owndata_data$prePeriodIndices[length(owndata_data$prePeriodIndices)],
    #                             owndata_data$prePeriodIndices[length(owndata_data$prePeriodIndices)]])
    #   }
    # } 

    if (all(input$owndata_delta != "2", input$owndata_sign != "1", input$owndata_monotonicity != "1")) stop("If bounding relative magnitudes is included in base Delta, it is not allowed to select both shape (aka monotonicity) and sign restrictions as additions.")
    if (all(input$owndata_method_rm %in% c("FLCI", "C-F"), input$owndata_delta != "2")) stop("Fixed Length Confidence Intervals or Conditional FLCI Hybrid is not suitable for base Delta that includes Bounding Relative Magnitudes, because it is proven that optimal FLCI has infinite length under those restrictions.")

    if (any(NA %in% (mod(as.numeric(unlist(strsplit(input$owndata_m_textinput,","))),1)!=0),any(as.numeric(unlist(strsplit(input$owndata_m_textinput,",")))<0))) stop("The vector of M must consist of numbers being seperated by commas. Numbers must be inon-negative. Please check examples in hints.")
    if (all(input$owndata_m_textinput !="", any(!is.na(input$owndata_m_lower), !is.na(input$owndata_m_upper), !is.na(input$owndata_m_step)))) stop("You can enter the vector of M EITHER arbitrarily OR as a sequence, but NOT both.")
    if (any(all(!is.na(input$owndata_m_lower), input$owndata_m_lower <0),all(!is.na(input$owndata_m_upper), input$owndata_m_upper <0),all(!is.na(input$owndata_m_step), input$owndata_m_step <0))) stop("Lower and upper bounds as well as step of the M sequence must be non-negative.")
    if (any(all(!is.na(input$owndata_m_lower), !is.na(input$owndata_m_upper), input$owndata_m_lower > input$owndata_m_upper),all(!is.na(input$owndata_m_lower), !is.na(input$owndata_m_upper), !is.na(input$owndata_m_step), input$owndata_m_step > input$owndata_m_upper - input$owndata_m_lower))) stop("Lower bound of the M sequence shouldn't be larger than upper bound, and step of the sequence shouldn't be larger than the difference between lower and upper bounds.")
    if (sum(is.na(c(input$owndata_m_lower, input$owndata_m_upper, input$owndata_m_step))) %in% c(1,2)) stop("If you choose to define the vector of M by sequence, you need to fill in all blanks of Lower, Upper and Step. Otherwise, leave them all blank if you decide to define the vector by default.")
    
    if (any(NA %in% (mod(as.numeric(unlist(strsplit(input$owndata_mbar_textinput,","))),1)!=0),any(as.numeric(unlist(strsplit(input$owndata_mbar_textinput,",")))<0))) stop("The vector of Mbar must consist of numbers being seperated by commas. Numbers must be inon-negative. Please check examples in hints.")
    if (all(input$owndata_mbar_textinput !="", any(!is.na(input$owndata_mbar_lower), !is.na(input$owndata_mbar_upper), !is.na(input$owndata_mbar_step)))) stop("You can enter the vector of Mbar EITHER arbitrarily OR as a sequence, but NOT both.")
    if (any(all(!is.na(input$owndata_mbar_lower), input$owndata_mbar_lower <0),all(!is.na(input$owndata_mbar_upper), input$owndata_mbar_upper <0),all(!is.na(input$owndata_mbar_step), input$owndata_mbar_step <0))) stop("Lower and upper bounds as well as step of the Mbar sequence must be non-negative.")
    if (any(all(!is.na(input$owndata_mbar_lower), !is.na(input$owndata_mbar_upper), input$owndata_mbar_lower > input$owndata_mbar_upper),all(!is.na(input$owndata_mbar_lower), !is.na(input$owndata_mbar_upper), !is.na(input$owndata_mbar_step), input$owndata_mbar_step > input$owndata_mbar_upper - input$owndata_mbar_lower))) stop("Lower bound of the Mbar sequence shouldn't be larger than upper bound, and step of the sequence shouldn't be larger than the difference between lower and upper bounds.")
    if (sum(is.na(c(input$owndata_mbar_lower, input$owndata_mbar_upper, input$owndata_mbar_step))) %in% c(1,2)) stop("If you choose to define the vector of Mbar by sequence, you need to fill in all blanks of Lower, Upper and Step. Otherwise, leave them all blank if you decide to define the vector by default.")

    if (NA %in% (mod(as.numeric(unlist(strsplit(input$owndata_lvec,","))),1)!=0)) stop("The vector that is used to determine parameter of interest must consist of numbers, instead of characters")
    if (any(mod(as.numeric(unlist(strsplit(input$owndata_lvec,","))),1)!=0)) stop("The vector that is used to determine parameter of interest must consist of integers.")
    if (any(as.numeric(unlist(strsplit(input$owndata_lvec,",")))<1)) stop("The vector that is used to determine parameter of interest must consist of integers that are not less than 1.")
    if (max(as.numeric(unlist(strsplit(input$owndata_lvec,","))))>length(owndata_data$postPeriodIndices)) stop("Numbers in the vector that is used to determine parameter of interest shouldn't be larger than the total length of post treatment periods.")
    if (all(input$owndata_delta == "2", any(!is.na(input$owndata_grid_lb),!is.na(input$owndata_grid_ub)))) stop("Bounds of grid used for underlying test inversion are only needed for the case when bounding relative magnitudes is included in base Delta. You need to expose [ Step 9 ] by clicking Bounding Relative Magnitudes again, and erase the values of bounds for grid.")
    if (any(all(!is.na(input$owndata_grid_lb), input$owndata_grid_lb>0), all(!is.na(input$owndata_grid_ub), input$owndata_grid_ub<0))) stop("Lower bound of grid used for underlying test inversion should be non-positive, and upper bound should be non-negative.")
    if (is.na(input$owndata_grid_points)) stop("Number of grid points shouldn't be empty.")
    if (all(!is.na(input$owndata_grid_points),any(mod(input$owndata_grid_points,1)!=0,input$owndata_grid_points < 1))) stop("Number of grid points should be positive integer.")
    
    # Calculation
    if (input$owndata_lvec=="") {
      owndata_lvec <- unique(as.numeric(strsplit(input$owndata_lvec,",")[[1]]))
      owndata_weight <- rep(1,length(owndata_data$postPeriodIndices))/length(owndata_data$postPeriodIndices)
    } else {
      owndata_lvec <- unique(as.numeric(strsplit(input$owndata_lvec,",")[[1]]))
      owndata_weight <- rep(0,length(owndata_data$postPeriodIndices))
      owndata_weight[owndata_lvec] <- 1
      owndata_weight <- owndata_weight/length(owndata_lvec)
    }
    
    owndata_sign <- if (input$owndata_sign=="2") "positive" else {if(input$owndata_sign =="3") "negative"}
    owndata_monotonicity <- if (input$owndata_monotonicity == "2") "increasing" else {if(input$owndata_monotonicity =="3") "decreasing"}
    owndata_method_delta <- if (input$owndata_delta != "2") input$owndata_method_rm else input$owndata_method_sd
    
    owndata_m_vector <- if (input$owndata_m_textinput !="") {
      unique(as.numeric(strsplit(input$owndata_m_textinput,",")[[1]]))
    } else {
      if (all(input$owndata_m_textinput =="", sum(is.na(c(input$owndata_m_lower, input$owndata_m_upper, input$owndata_m_step)))==0)) {
        seq(from= input$owndata_m_lower, to= input$owndata_m_upper, by= input$owndata_m_step)
      }
    }
    
    owndata_mbar_vector <- if (input$owndata_mbar_textinput !="") {
      unique(as.numeric(strsplit(input$owndata_mbar_textinput,",")[[1]]))
    } else {
      if (all(input$owndata_mbar_textinput =="", sum(is.na(c(input$owndata_mbar_lower, input$owndata_mbar_upper, input$owndata_mbar_step)))==0)) {
        seq(from= input$owndata_mbar_lower, to= input$owndata_mbar_upper, by= input$owndata_mbar_step)
      } else {
        seq(from=0.25, to=1.25, by=0.25)  # Default vector of mbar is seq(0.25,1.25,0.25)
      }
    }
    
    owndata_mmbar_vector <- if (input$owndata_delta != "2") owndata_mbar_vector else owndata_m_vector
    
    # original OLS betahat
    owndata_originalResults <- constructOriginalCS(
      betahat = owndata_data$beta, 
      sigma = owndata_data$sigma,
      numPrePeriods = length(owndata_data$prePeriodIndices), 
      numPostPeriods = length(owndata_data$postPeriodIndices), 
      l_vec = owndata_weight, 
      alpha = input$owndata_alpha
    )
    
    if (input$owndata_delta == "1") {
      owndata_delta_rm_results <- createSensitivityResults_relativeMagnitudes(
        betahat = owndata_data$beta, 
        sigma = owndata_data$sigma, 
        numPrePeriods = length(owndata_data$prePeriodIndices), 
        numPostPeriods = length(owndata_data$postPeriodIndices), 
        bound = "deviation from parallel trends",
        method = owndata_method_delta, 
        Mbarvec = owndata_mmbar_vector,  
        l_vec = owndata_weight, 
        biasDirection = owndata_sign, 
        monotonicityDirection = owndata_monotonicity, 
        alpha = input$owndata_alpha,
        parallel = as.logical(input$owndata_parallel),
        gridPoints = input$owndata_grid_points,  
        grid.lb = input$owndata_grid_lb, 
        grid.ub = input$owndata_grid_ub 
      )
      owndata_v$result<-list(data=owndata_data,lvec=owndata_lvec,delta_rm_results=owndata_delta_rm_results, originalResults=owndata_originalResults, 
                             mmbar=owndata_mmbar_vector, weight=owndata_weight, sign=owndata_sign, monotonicity=owndata_monotonicity, alpha=input$owndata_alpha, method=owndata_method_delta, delta=input$owndata_delta, name=input$owndata_file$name, grid_points=input$owndata_grid_points, grid_lb=input$owndata_grid_lb, grid_ub=input$owndata_grid_ub, parallel=as.logical(input$owndata_parallel))
    } else {
      if (input$owndata_delta == "2") {
        owndata_delta_sd_results <- createSensitivityResults(
          betahat = owndata_data$beta, 
          sigma = owndata_data$sigma, 
          numPrePeriods = length(owndata_data$prePeriodIndices), 
          numPostPeriods = length(owndata_data$postPeriodIndices), 
          method = owndata_method_delta, 
          Mvec = owndata_mmbar_vector,  
          l_vec = owndata_weight, 
          monotonicityDirection = owndata_monotonicity, 
          biasDirection = owndata_sign, 
          alpha = input$owndata_alpha,
          parallel = as.logical(input$owndata_parallel)
        )
        owndata_v$result<-list(data=owndata_data,lvec=owndata_lvec,delta_sd_results=owndata_delta_sd_results, originalResults=owndata_originalResults, 
                               mmbar=owndata_mmbar_vector, weight=owndata_weight, sign=owndata_sign, monotonicity=owndata_monotonicity, alpha=input$owndata_alpha, method=owndata_method_delta, delta=input$owndata_delta, name=input$owndata_file$name, parallel=as.logical(input$owndata_parallel))
      } else {
        owndata_delta_rm_results <- createSensitivityResults_relativeMagnitudes(
          betahat = owndata_data$beta, 
          sigma = owndata_data$sigma, 
          numPrePeriods = length(owndata_data$prePeriodIndices), 
          numPostPeriods = length(owndata_data$postPeriodIndices), 
          bound = "deviation from linear trend", 
          method = owndata_method_delta,
          Mbarvec = owndata_mmbar_vector,  
          l_vec = owndata_weight, 
          biasDirection = owndata_sign, 
          monotonicityDirection = owndata_monotonicity, 
          alpha = input$owndata_alpha,
          parallel = as.logical(input$owndata_parallel),
          gridPoints = input$owndata_grid_points,  
          grid.lb = input$owndata_grid_lb, 
          grid.ub = input$owndata_grid_ub 
        )
        owndata_v$result<-list(data=owndata_data,lvec=owndata_lvec,delta_rm_results=owndata_delta_rm_results, originalResults=owndata_originalResults, 
                               mmbar=owndata_mmbar_vector, weight=owndata_weight, sign=owndata_sign, monotonicity=owndata_monotonicity, alpha=input$owndata_alpha, method=owndata_method_delta, delta=input$owndata_delta, name=input$owndata_file$name, grid_points=input$owndata_grid_points, grid_lb=input$owndata_grid_lb, grid_ub=input$owndata_grid_ub, parallel=as.logical(input$owndata_parallel))
      }
    }
  }))
  
  # Observe Reset
  observeEvent(input$owndata_reset, {
    owndata_v$result <- NULL
    reset("owndata_fileupload")
    reset("owndata_method")
    reset("owndata_delta")
    reset("owndata_sign")
    reset("owndata_monotonicity")
    reset("owndata_method_sd")
    reset("owndata_method_rm")
    reset("owndata_m_textinput")
    reset("owndata_m_lower")
    reset("owndata_m_upper")
    reset("owndata_m_step")
    reset("owndata_mbar_textinput")
    reset("owndata_mbar_lower")
    reset("owndata_mbar_upper")
    reset("owndata_mbar_step")
    reset("owndata_lvec")
    reset("owndata_alpha")
    reset("owndata_parallel")
    reset("owndata_grid_lb")
    reset("owndata_grid_ub")
    reset("owndata_grid_points")
  })
  
  # Observe Disconnect
  observeEvent(input$owndata_disconnect, {
    session$close()
  })
  
  # ------------ Outputs -----------------------------
  # Alert below will trigger if lower bounds of confidence sets are exactly the same for different Mbar
  observe({
    if (all(owndata_v$result$delta != "2",
            any(duplicated(owndata_v$result$delta_rm_results$lb)))) {
      owndata_grid_lb_error_check <- "WARNING: It is detected that the lower bounds of confidence sets at two or more different Mbar are exactly the same. This is due to the fact that the lower bound of grid used for underlying test inversion is too large. You need to adjust it on the left pane to get correct confidence sets. Please go to [ More ] > [ FAQ ] for details on how the lower bound of grid used for test inversion is defined by default."
      owndata_grid_lb_error_js_string <- 'alert("SOMETHING");'
      owndata_grid_lb_error_js_string <- sub("SOMETHING",owndata_grid_lb_error_check,owndata_grid_lb_error_js_string)
      session$sendCustomMessage(type='jsCode', list(value = owndata_grid_lb_error_js_string))
    }
  })
  
  # Alert below will trigger if upper bounds of confidence sets are exactly the same for different Mbar
  observe({
    if (all(owndata_v$result$delta != "2",
            any(duplicated(owndata_v$result$delta_rm_results$ub)))) {
      owndata_grid_ub_error_check <- "WARNING: It is detected that the upper bounds of confidence sets at two or more different Mbar are exactly the same. This is due to the fact that the upper bound of grid used for underlying test inversion is too small. You need to adjust it on the left pane to get correct confidence sets. Please go to [ More ] > [ FAQ ] for details on how the upper bound of grid used for test inversion is defined by default."
      owndata_grid_ub_error_js_string <- 'alert("SOMETHING");'
      owndata_grid_ub_error_js_string <- sub("SOMETHING",owndata_grid_ub_error_check,owndata_grid_ub_error_js_string)
      session$sendCustomMessage(type='jsCode', list(value = owndata_grid_ub_error_js_string))
    }
  })
  
  # Clock
  output$owndata_currentTime <- renderText({
    invalidateLater(1000, session)
    owndata_tz <- Sys.timezone(location = TRUE)
    owndata_tz <- if (class(owndata_tz)!="character") "UTC" else owndata_tz
    #paste("[", lubridate::with_tz(Sys.time(), owndata_tz),"]")
    paste("[", Sys.time(),owndata_tz, "]")
  })

  # Hide or show step of grid used for test inversion
  observeEvent(input$owndata_delta,  {
    if(input$owndata_delta != "2"){
      show("owndata_grid_hint")
      show("owndata_grid_note")
      show("owndata_grid_lb")
      show("owndata_grid_ub")
      show("owndata_grid_points")
    }
  })
  
  observeEvent(input$owndata_delta,  {
    if(input$owndata_delta == "2"){
      hide("owndata_grid_hint")
      hide("owndata_grid_note")
      hide("owndata_grid_lb")
      hide("owndata_grid_ub")
      hide("owndata_grid_points")
    }
  })
  
  # Event Study Plot
  owndata_betaplot <- reactive({
    if (is.null(owndata_v$result)) return()
    createEventStudyPlot(betahat = owndata_v$result$data$beta,
                         sigma = owndata_v$result$data$sigma,
                         numPrePeriods = length(owndata_v$result$data$prePeriodIndices),
                         numPostPeriods = length(owndata_v$result$data$postPeriodIndices),
                         alpha = owndata_v$result$alpha,
                         timeVec = owndata_v$result$data$tVec,
                         referencePeriod = owndata_v$result$data$referencePeriod,
                         useRelativeEventTime = T) + 
      labs(caption = "This plot is in relative event time, which normalizes the reference period to be 0.")
  })
  
  output$owndata_betaplot <- renderPlot({
    input$owndata_start
    owndata_betaplot()
  })
  
  # Sensitivity Plot
  owndata_ssplot <- reactive({
    if (is.null(owndata_v$result)) return()
    
    plot <-  if(owndata_v$result$delta == "2") {
      createSensitivityPlot(owndata_v$result$delta_sd_results, owndata_v$result$originalResults)
    } else {
      createSensitivityPlot_relativeMagnitudes(owndata_v$result$delta_rm_results, owndata_v$result$originalResults)
    }
    
    target <- if (length(owndata_v$result$lvec) == 0) {
      paste("Parameter of Interest: Average Causal Effect over all",
            length(owndata_v$result$data$postPeriodIndices) ,"Post-Treatment Periods. \n")
    } else {
      if (length(owndata_v$result$lvec) == 1) {
        paste("Parameter of Interest: Causal Effect at the",
              case_when(owndata_v$result$lvec == 1 ~ "1st",
                        owndata_v$result$lvec == 2 ~"2nd",
                        owndata_v$result$lvec == 3 ~ "3rd",
                        TRUE ~ paste(owndata_v$result$lvec,"th",sep="")),
              "Period after treatment. \n")
      } else {
        paste("Parameter of Interest: Average Causal Effect over Periods",
              paste(owndata_v$result$lvec, collapse = ",") ,"after treatment. \n")
      }
    }
    plot + labs(caption = target)
  })
  
  output$owndata_ssplot <- renderPlot({
    input$owndata_start
    owndata_ssplot()
  })

  # Sensitivity Note
  output$owndata_note <- renderText({
    input$owndata_start
    if (is.null(owndata_v$result)) return()
    "\n General Rule: In sensitivity plot, if the confidence interval under certain degree of violation on parallel trends doesn't cross 0, then we say, the paramter of interest is significant under that certain M or Mbar. Otherwise, insignificant."
  })
  
  # Grid Warning
  output$owndata_grid_warning <- renderText({
    input$owndata_start
    if (is.null(owndata_v$result)) return()
    owndata_gridwarning <- if (all(owndata_v$result$delta != "2", any(duplicated(owndata_v$result$delta_rm_results$lb)))) {
      "\n It is detected that the lower bounds of confidence sets at two or more different Mbar are exactly the same. This is due to the fact that the lower bound of grid used for underlying test inversion is too large. You need to adjust it on the left pane to get correct confidence sets. Please go to [ More ] > [ FAQ ] for details on how the lower bound of grid used for test inversion is defined by default."
    } else {
      if ((all(owndata_v$result$delta != "2", any(duplicated(owndata_v$result$delta_rm_results$ub))))) {
        "\n It is detected that the upper bounds of confidence sets at two or more different Mbar are exactly the same. This is due to the fact that the upper bound of grid used for underlying test inversion is too small. You need to adjust it on the left pane to get correct confidence sets. Please go to [ More ] > [ FAQ ] for details on how the upper bound of grid used for test inversion is defined by default."
      } else {
        if (all(owndata_v$result$delta != "2", any(duplicated(owndata_v$result$delta_rm_results$lb)), any(duplicated(owndata_v$result$delta_rm_results$ub)))) {
          "\n It is detected that the bounds of confidence sets at two or more different Mbar are exactly the same. This is due to the fact that the lower (upper) bound of grid used for underlying test inversion is too large (small). You need to adjust it on the left pane to get correct confidence sets. Please go to [ More ] > [ FAQ ] for details on how bounds of grid used for test inversion is defined by default."
        } else {NULL}
      }
    }
    paste(owndata_gridwarning)
  })
  
  # Sensitivity Data
  owndata_ssdata <- reactive({
    if (is.null(owndata_v$result)) return()
    if(owndata_v$result$delta == "2") {   
      owndata_v$result$delta_sd_results
    } else {
      owndata_v$result$delta_rm_results
    }
  })
  
  output$owndata_ssdata <- renderTable({
    input$owndata_start
    owndata_ssdata()},  
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE,
    width = "100%",
    digits = 5
  )
  
  # R Code
  owndata_code <- reactive({
    if (is.null(owndata_v$result)) return()
    
    head <- c(" # Install HonestDiD package from https://github.com/asheshrambachan/HonestDiD, if not installed \n",
              "# install.packages('remotes') \n",
              "# Sys.setenv('R_REMOTES_NO_ERRORS_FROM_WARNINGS' = 'true') \n",
              "# remotes::install_github('asheshrambachan/HonestDiD') \n",
              "library(HonestDiD) \n",
              "library(readxl) \n \n",
              "options(warn = -1) \n \n")

    msheet <- c("# Define a function to read in multiple sheets of an excel file simultaneously \n",
                "multiplesheets <- function(fname) { \n",
                "\40 \40 sheets <- readxl::excel_sheets(fname) \n",
                "\40 \40 tibble <- lapply(sheets, function(x) readxl::read_excel(fname, sheet = x)) \n",
                "\40 \40 data_frame <- lapply(tibble, as.data.frame) \n",
                "\40 \40 names(data_frame) <- sheets \n",
                "\40 \40 return(data_frame) \n",
                "} \n \n")
    
    dataset <- c("# Read in Excel Data \n",
                 paste("# Complete Your Data Path if '", owndata_v$result$name,"' is not in Your Working Directory \n", sep=""),
                 paste("data <- multiplesheets('", owndata_v$result$name,"') \n \n", sep=""),
                 "# Reformat Data \n",
                 "data <- c(list(sigma=as.matrix(data$sigma)), \n",
                 "\40 \40 lapply(data[c('beta', 'tVec', 'referencePeriod', 'prePeriodIndices', 'postPeriodIndices')], unlist)) \n \n"
                 )
    
    eventPlot <- c("# Create Event Study Plot \n",
                   "event_study_plot <- createEventStudyPlot( \n",
                   "\40 \40 betahat = data$beta, \n",
                   "\40 \40 sigma = data$sigma, \n",
                   "\40 \40 numPrePeriods = length(data$prePeriodIndices), \n",
                   "\40 \40 numPostPeriods = length(data$postPeriodIndices), \n",
                   "\40 \40 alpha = ", owndata_v$result$alpha, ", \n",
                   "\40 \40 timeVec = data$tVec, \n",
                   "\40 \40 referencePeriod = data$referencePeriod, \n",
                   "\40 \40 useRelativeEventTime = TRUE \n",
                   ") \n",
                   "event_study_plot \n \n")
    
    originalCS <- c("# Construct Original Confidence Set \n",
                    "originalResults <- constructOriginalCS( \n",
                    "\40 \40 betahat = data$beta, \n",
                    "\40 \40 sigma = data$sigma, \n",
                    "\40 \40 numPrePeriods = length(data$prePeriodIndices), \n",
                    "\40 \40 numPostPeriods = length(data$postPeriodIndices), \n",
                    "\40 \40 l_vec = c(",paste(owndata_v$result$weight, collapse = ","),"), \n",
                    "\40 \40 alpha = ",owndata_v$result$alpha, "\n",
                    ") \n \n")
    
    sensitivityAnalysis <- if (owndata_v$result$delta == "1") {
      c("# Create Sensitivity Result \n",
        "delta_rm_results <- createSensitivityResults_relativeMagnitudes( \n",
        "\40 \40 betahat = data$beta, \n",
        "\40 \40 sigma = data$sigma, \n",
        "\40 \40 numPrePeriods = length(data$prePeriodIndices), \n",
        "\40 \40 numPostPeriods = length(data$postPeriodIndices), \n",
        "\40 \40 bound = 'deviation from parallel trends', \n",
        paste("\40 \40 method = '",owndata_v$result$method,"'", sep=""),", \n",
        "\40 \40 Mbarvec = ", if (is.null(owndata_v$result$mmbar)) NULL else paste("c(",paste(owndata_v$result$mmbar, collapse=","),")", sep=""), ", \n",  
        "\40 \40 l_vec = c(", paste(owndata_v$result$weight, collapse = ","), "), \n",
        "\40 \40 biasDirection = ", owndata_v$result$sign,", \n",
        "\40 \40 monotonicityDirection = ",owndata_v$result$monotonicity,", \n",
        "\40 \40 alpha = ", owndata_v$result$alpha,", \n",
        "\40 \40 parallel = ", owndata_v$result$parallel,", \n",
        "\40 \40 gridPoints = ",owndata_v$result$grid_points,", \n",  
        "\40 \40 grid.lb = ",owndata_v$result$grid_lb,", \n", 
        "\40 \40 grid.ub = ",owndata_v$result$grid_ub,"\n",  
        ") \n \n")
    } else {
      if (owndata_v$result$delta == "2") {
        c("# Create Sensitivity Result \n",
          "delta_sd_results <- createSensitivityResults( \n",
          "\40 \40 betahat = data$beta, \n",
          "\40 \40 sigma = data$sigma, \n",
          "\40 \40 numPrePeriods = length(data$prePeriodIndices), \n",
          "\40 \40 numPostPeriods = length(data$postPeriodIndices), \n",
          paste("\40 \40 method = '",owndata_v$result$method,"'", sep=""),", \n",
          "\40 \40 Mvec = ", if (is.null(owndata_v$result$mmbar)) NULL else paste("c(",paste(owndata_v$result$mmbar, collapse=","),")", sep=""), ", \n",   
          "\40 \40 l_vec = c(", paste(owndata_v$result$weight, collapse = ","), "), \n",
          "\40 \40 biasDirection = ", owndata_v$result$sign,", \n",
          "\40 \40 monotonicityDirection = ", owndata_v$result$monotonicity,", \n",
          "\40 \40 alpha = ", owndata_v$result$alpha,", \n",
          "\40 \40 parallel = ", owndata_v$result$parallel,"\n",
          ") \n \n")
      } else {
        c("# Create Sensitivity Result \n",
          "delta_rm_results <- createSensitivityResults_relativeMagnitudes( \n",
          "\40 \40 betahat = data$beta, \n",
          "\40 \40 sigma = data$sigma, \n",
          "\40 \40 numPrePeriods = length(data$prePeriodIndices), \n",
          "\40 \40 numPostPeriods = length(data$postPeriodIndices), \n",
          "\40 \40 bound = 'deviation from linear trend', \n",
          paste("\40 \40 method = '",owndata_v$result$method,"'", sep=""),", \n",
          "\40 \40 Mbarvec = ", if (is.null(owndata_v$result$mmbar)) NULL else paste("c(",paste(owndata_v$result$mmbar, collapse=","),")", sep=""), ", \n",  
          "\40 \40 l_vec = c(", paste(owndata_v$result$weight, collapse = ","), "), \n",
          "\40 \40 biasDirection = ", owndata_v$result$sign,", \n",
          "\40 \40 monotonicityDirection = ", owndata_v$result$monotonicity,", \n",
          "\40 \40 alpha = ", owndata_v$result$alpha,", \n",
          "\40 \40 parallel = ", owndata_v$result$parallel,", \n",
          "\40 \40 gridPoints = ",owndata_v$result$grid_points,", \n",  
          "\40 \40 grid.lb = ",owndata_v$result$grid_lb,", \n", 
          "\40 \40 grid.ub = ",owndata_v$result$grid_ub,"\n",  
          ") \n \n")
      }
    }
    
    plot <- if(owndata_v$result$delta == "2") {
      c("# Create Sensitivity Plot \n",
        "sensitivity_plot <- createSensitivityPlot(delta_sd_results, originalResults) \n",
        "sensitivity_plot \n \n")
    } else {
      c("# Create Sensitivity Plot \n",
        "sensitivity_plot <- createSensitivityPlot_relativeMagnitudes(delta_rm_results, originalResults) \n",
        "sensitivity_plot \n \n")
    }
    
    tail <- c("options(warn = 0) \n")
    
    paste0(c(head, msheet, dataset, eventPlot, originalCS, sensitivityAnalysis, plot, tail), sep="")
  })
  
  output$owndata_code <- renderText({
    input$owndata_start
    owndata_code()
  })
  
  # ------------ Downloads -----------------------------
  # event-study plot download
  output$owndata_betaplotdownload <- downloadHandler(
    filename <- function(){
      paste("Event_Study_Plot.png",sep="")
    },
    content <- function(file){
      ggsave(file, plot=owndata_betaplot(),device = "png")
    }
  )
  
  # sensitivity plot download
  output$owndata_ssplotdownload <- downloadHandler(
    filename <- function(){
      paste("Sensitivity_Plot.png",sep="")
    },
    content <- function(file){
      ggsave(file, plot=owndata_ssplot(),device = "png")
    }
  )
  
  # sensitivity data download
  output$owndata_ssdatadownload <- downloadHandler(
    filename = function(){
      paste("Sensitivity_Result.xlsx",sep="")
    }, 
    content = function(file){
      WriteXLS(list(Sensitivity_Result=as.data.frame(owndata_ssdata())), file)
    }
  )
  

  # *****************************************************************
  # ************* Help Tab: Non Stagger Panel************************
  # *****************************************************************
  
  # ------------ Alerts----------------------------------------------
  # Alert below will trigger if file uploaded is not excel
  resetNonStaggerFileUpload <- reactiveVal(FALSE)
  output$nonstagger_fileupload <- renderUI({
    resetNonStaggerFileUpload() # reactive dependency
    resetNonStaggerFileUpload(FALSE)
    fileInput("nonstagger_file",
              p("Upload your raw data in xlsx format", span(a("[ Example File ]", href="https://raw.githubusercontent.com/ccfang2/HonestDiDSenAnlys/main/NonStaggeredDataFormat.xlsx"))),
              accept = ".xlsx")
  })

  observeEvent(input$nonstagger_file, {
    if(file_ext(input$nonstagger_file$name) != "xlsx"){
      resetNonStaggerFileUpload(TRUE)
      showModal(modalDialog("That's not an .xlsx file"))
    }
  })

  # Alert below will trigger if input value in step 2 is not numeric scalar
  observe({
    if (is.na(input$nonstagger_ref)) {
      nonstagger_ref_null_check <- "Reference period shouldn't be empty."
      nonstagger_ref_null_js_string <- 'alert("SOMETHING");'
      nonstagger_ref_null_js_string <- sub("SOMETHING",nonstagger_ref_null_check,nonstagger_ref_null_js_string)
      session$sendCustomMessage(type='jsCode', list(value = nonstagger_ref_null_js_string))
    }
  })

  # Alert below will trigger if input value in step 2 is decimal
  observe({
    if (!is.integer(input$nonstagger_ref)) {
      nonstagger_integer_check <- "Reference period must be integer."
      nonstagger_integer_js_string <- 'alert("SOMETHING");'
      nonstagger_integer_js_string <- sub("SOMETHING",nonstagger_integer_check,nonstagger_integer_js_string)
      session$sendCustomMessage(type='jsCode', list(value = nonstagger_integer_js_string))
    }
  })
  
  # ------------ Observe Start or Reset-----------------------------
  nonstagger_v <- reactiveValues(result = NULL)

  # Read in Excel Data
  nonstagger_read_data <- reactive({
    ext <- tools::file_ext(input$nonstagger_file$datapath)
    req(input$nonstagger_file)
    validate(need(ext == "xlsx", "Please upload an xlsx file"))
    return(read_excel(input$nonstagger_file$datapath))
  })

  # Observe Start
  observeEvent(input$nonstagger_start,execute_safely({
    
    # Check if data file is missing
    if (length(input$nonstagger_file)==0) stop("Please upload an .xlsx data file.")
    
    # Reformat Data
    nonstagger_data <- nonstagger_read_data()

    # Report Errors
    if (is.na(input$nonstagger_ref)) stop("Reference period shouldn't be empty.")
    if (!is.integer(input$nonstagger_ref)) stop("Reference period must be integer.")
    if (!(input$nonstagger_ref %in% unique(nonstagger_data)$time)) stop("Reference period shouldn't be outside of the time periods that are shown in your raw data.")
    
    nonstagger_ref<-input$nonstagger_ref
    
    # Calculation
    glm_family_options <- c("binomial","gaussian","Gamma","inverse.gaussian","poisson")
    mle_family_options <- c("poisson","negbin","logit","gaussian")

    if (input$nonstagger_est_model == "1") {
         nonstagger_ols <- fixest::feols(y ~ i(time, D, ref=nonstagger_ref) | id + time,
                                     cluster = "id",
                                     data = nonstagger_data)
         nonstagger_v$result<-list(data=nonstagger_data, twfe=nonstagger_ols, ref=input$nonstagger_ref, alpha=input$nonstagger_alpha, name=input$nonstagger_file$name, est_model = input$nonstagger_est_model)
    } else {
      if (input$nonstagger_est_model == "2") {
        nonstagger_glm <- fixest::feglm(y ~ i(time, D, ref=nonstagger_ref) | id + time,
                                        cluster = "id",
                                        family = glm_family_options[as.numeric(input$nonstagger_glm)],
                                        data = nonstagger_data)
        nonstagger_v$result<-list(data=nonstagger_data, twfe=nonstagger_glm, family=glm_family_options[as.numeric(input$nonstagger_glm)], ref=input$nonstagger_ref, alpha=input$nonstagger_alpha, name=input$nonstagger_file$name, est_model = input$nonstagger_est_model)
      } else {
        nonstagger_mle <- fixest::femlm(y ~ i(time, D, ref=nonstagger_ref) | id + time,
                                        cluster = "id",
                                        family = mle_family_options[as.numeric(input$nonstagger_mlm)],
                                        data = nonstagger_data)
        nonstagger_v$result<-list(data=nonstagger_data, twfe=nonstagger_mle, family=mle_family_options[as.numeric(input$nonstagger_mlm)], ref=input$nonstagger_ref, alpha=input$nonstagger_alpha, name=input$nonstagger_file$name, est_model = input$nonstagger_est_model)
      }
    }
    }))
  
  # Observe Reset
  observeEvent(input$nonstagger_reset, {
    nonstagger_v$result <- NULL
    reset("nonstagger_fileupload")
    reset("nonstagger_est_model")
    reset("nonstagger_glm")
    reset("nonstagger_mlm")
    reset("nonstagger_ref")
    reset("nonstagger_alpha")
  })
  
  # Observe Disconnect
  observeEvent(input$nonstagger_disconnect, {
    session$close()
  })
  
  # ------------ Outputs -----------------------------
  # Clock
  output$nonstagger_currentTime <- renderText({
    invalidateLater(1000, session)
    nonstagger_tz <- Sys.timezone(location = TRUE)
    nonstagger_tz <- if (class(nonstagger_tz)!="character") "UTC" else nonstagger_tz
    # paste("[", lubridate::with_tz(Sys.time(), nonstagger_tz),"]")
    paste("[", Sys.time(),nonstagger_tz,"]")
  })

  # Hide or show family buttons
  observeEvent(input$nonstagger_est_model,  {
    if(input$nonstagger_est_model == "2"){
      show("nonstagger_glm")
    }
  })
  
  observeEvent(input$nonstagger_est_model,  {
    if(input$nonstagger_est_model != "2"){
      hide("nonstagger_glm")
    }
  })
  
  observeEvent(input$nonstagger_est_model,  {
    if(input$nonstagger_est_model == "3"){
      show("nonstagger_mlm")
    }
  })
  
  observeEvent(input$nonstagger_est_model,  {
    if(input$nonstagger_est_model != "3"){
      hide("nonstagger_mlm")
    }
  })

  # TWFE Estimation Output File
  nonstagger_beta <- reactive({
    if (is.null(nonstagger_v$result)) return()
    beta <- t(nonstagger_v$result$twfe$coefficients)
    colnames(beta) <- NULL
    beta
  })
  
  nonstagger_sigma <- reactive({
    if (is.null(nonstagger_v$result)) return()
    sigma <- nonstagger_v$result$twfe$cov.scaled
    rownames(sigma) <- NULL
    colnames(sigma) <- NULL
    sigma
  })
  
  nonstagger_tVec <- reactive({
    if (is.null(nonstagger_v$result)) return()
    t(unique(nonstagger_v$result$data$time)[-which(unique(nonstagger_v$result$data$time)==input$nonstagger_ref)])
  })
  
  nonstagger_ref <- reactive({
    if (is.null(nonstagger_v$result)) return()
    t(input$nonstagger_ref)
  })
  
  nonstagger_pre <- reactive({
    if (is.null(nonstagger_v$result)) return()
    t(which((unique(nonstagger_v$result$data$time)[-which(unique(nonstagger_v$result$data$time)==input$nonstagger_ref)]<input$nonstagger_ref) == TRUE))
  })
  
  nonstagger_post <- reactive({
    if (is.null(nonstagger_v$result)) return()
    t(which((unique(nonstagger_v$result$data$time)[-which(unique(nonstagger_v$result$data$time)==input$nonstagger_ref)]<input$nonstagger_ref) == FALSE))
  })
  
  # Event Study Plot
  
  nonstagger_betaplot <- reactive({
    if (is.null(nonstagger_v$result)) return()
    createEventStudyPlot(betahat = nonstagger_v$result$twfe$coefficients,
                         sigma = nonstagger_v$result$twfe$cov.scaled,
                         numPrePeriods = length(unique(nonstagger_v$result$data$time)[unique(nonstagger_v$result$data$time)< nonstagger_v$result$ref]),
                         numPostPeriods = length(unique(nonstagger_v$result$data$time)[unique(nonstagger_v$result$data$time)> nonstagger_v$result$ref]),
                         alpha = nonstagger_v$result$alpha,
                         timeVec = unique(nonstagger_v$result$data$time)[-which(unique(nonstagger_v$result$data$time)==nonstagger_v$result$ref)],
                         referencePeriod = nonstagger_v$result$ref,
                         useRelativeEventTime = F)
  })
  
  output$nonstagger_betaplot <- renderPlot({
    input$nonstagger_start
    nonstagger_betaplot()
  })
  
  # R Code
  nonstagger_code <- reactive({
    if (is.null(nonstagger_v$result)) return()

    head <- c(" # Install HonestDiD package from https://github.com/asheshrambachan/HonestDiD, if not installed \n",
              "# install.packages('remotes') \n",
              "# Sys.setenv('R_REMOTES_NO_ERRORS_FROM_WARNINGS' = 'true') \n",
              "# remotes::install_github('asheshrambachan/HonestDiD') \n",
              "# Install fixest package, if not installed \n",
              "# install.packages('fixest') \n",
              "library(HonestDiD) \n",
              "library(fixest) \n",
              "library(WriteXLS) \n",
              "library(readxl) \n \n",
              "options(warn = -1) \n \n")

    dataset <- c("# Read in Excel Data \n",
                 paste("# Complete Your Data Path if '", nonstagger_v$result$name,"' is not in Your Working Directory \n", sep=""),
                 paste("data <- read_excel('", nonstagger_v$result$name,"') \n \n", sep="")
                 )
    
    twfe <- if (nonstagger_v$result$est_model == "1") {
      c("# Nonstaggered DiD Estimation using TWFE OLS \n",
        "nonstaggerTwfe <- fixest::feols(y ~ i(time, D,", nonstagger_v$result$ref, ") | id + time, \n",
        "\40 \40 cluster = 'id', \n",
        "\40 \40 data = data \n",
        ") \n \n"
      )
    } else {
      if (nonstagger_v$result$est_model == "2") {
        c("# Nonstaggered DiD Estimation using TWFE GLM \n",
          "nonstaggerTwfe <- fixest::feglm(y ~ i(time, D,", nonstagger_v$result$ref, ") | id + time, \n",
          "\40 \40 cluster = 'id', \n",
          paste("\40 \40 family = '", nonstagger_v$result$family,"', \n", sep=""),
          "\40 \40 data = data \n",
          ") \n \n"
        )
      } else {
        c("# Nonstaggered DiD Estimation using TWFE MLE \n",
          "nonstaggerTwfe <- fixest::femlm(y ~ i(time, D,", nonstagger_v$result$ref, ") | id + time, \n",
          "\40 \40 cluster = 'id', \n",
          paste("\40 \40 family = '", nonstagger_v$result$family,"', \n", sep=""),
          "\40 \40 data = data \n",
          ") \n \n"
        )
      }
    }

    eventPlot <- c("# Create event study plot \n",
                   "# Method 1: command 'iplot' from package 'fixest' \n",
                   "event_study_iplot <- fixest::iplot(nonstaggerTwfe, \n",
                   paste("\40 \40 ci_level = ", 1-nonstagger_v$result$alpha,", \n", sep=""),
                   "\40 \40 main = '', \n",
                   "\40 \40 xlab = '' \n",
                   ") \n",
                   "event_study_iplot \n \n",
                   "# Method 2: command 'createEventStudyPlot' from package 'HonestDiD' \n",
                   "event_study_plot <- createEventStudyPlot(betahat = nonstaggerTwfe$coefficients, \n",
                   "\40 \40 \40 sigma = nonstaggerTwfe$cov.scaled, \n",
                   "\40 \40 \40 numPrePeriods = length(unique(data$time)[unique(data$time)<",nonstagger_v$result$ref,"]), \n",
                   "\40 \40 \40 numPostPeriods = length(unique(data$time)[unique(data$time)>",nonstagger_v$result$ref,"]), \n",
                   "\40 \40 \40 alpha = ",nonstagger_v$result$alpha,", \n",
                   "\40 \40 \40 timeVec = unique(data$time)[-which(unique(data$time)==",nonstagger_v$result$ref,")], \n",
                   "\40 \40 \40 referencePeriod = ",nonstagger_v$result$ref,", \n",
                   "\40 \40 \40 useRelativeEventTime = FALSE) \n",
                   "event_study_plot \n \n"
                   )
    
    outputTable <- c("# Create TWFE Estimation Output Table File to Working Directory \n",
                     "beta <- t(nonstaggerTwfe$coefficients) \n",
                     "colnames(beta) <- NULL \n",
                     "sigma <- nonstaggerTwfe$cov.scaled \n",
                     "rownames(sigma) <- NULL \n",
                     "colnames(sigma) <- NULL \n",
                     "WriteXLS(list(beta=as.data.frame(beta), \n",
                     "\40 \40 \40 sigma =as.data.frame(sigma), \n",
                     paste("\40 \40 \40 tVec = as.data.frame(t(unique(data$time)[-which(unique(data$time)==", nonstagger_v$result$ref, ")])), \n", sep=""),
                     paste("\40 \40 \40 referencePeriod = as.data.frame(t(", nonstagger_v$result$ref, ")), \n", sep=""),
                     paste("\40 \40 \40 prePeriodIndices = as.data.frame(t(which((unique(data$time)[-which(unique(data$time)==", nonstagger_v$result$ref, ")]<", nonstagger_v$result$ref, ") == TRUE))), \n", sep=""),
                     paste("\40 \40 \40 postPeriodIndices = as.data.frame(t(which((unique(data$time)[-which(unique(data$time)==", nonstagger_v$result$ref, ")]<", nonstagger_v$result$ref, ") == FALSE)))), \n", sep=""),
                     "\40 \40 'Nonstaggered_TWFE.xlsx') \n \n"
                     )

    tail <- c("options(warn = 0) \n")

    paste0(c(head, dataset, twfe, eventPlot, outputTable, tail), sep="")
  })
  
  output$nonstagger_code <- renderText({
    input$nonstagger_start
    nonstagger_code()
  })

  # ------------ Downloads -----------------------------
  # TWFE estimation output file download
  output$nonstagger_outputdownload <- downloadHandler(
    filename = function(){
      paste("Nonstaggered_TWFE.xlsx",sep="")
    },
    content = function(file){
      WriteXLS(list(beta = as.data.frame(nonstagger_beta()),
                    sigma = as.data.frame(nonstagger_sigma()),
                    tVec = as.data.frame(nonstagger_tVec()),
                    referencePeriod = as.data.frame(nonstagger_ref()),
                    prePeriodIndices = as.data.frame(nonstagger_pre()),
                    postPeriodIndices = as.data.frame(nonstagger_post())
      ), file)
    }
  )
  
  # event-study plot download
  output$nonstagger_betaplotdownload <- downloadHandler(
    filename <- function(){
      paste("Event_Study_Plot.png",sep="")
    },
    content <- function(file){
      ggsave(file, plot=nonstagger_betaplot(),device = "png")
    }
  )


  # *****************************************************************
  # ************* Help Tab: Stagger Panel************************
  # *****************************************************************
  
  # ------------ Alerts----------------------------------------------
  # Alert below will trigger if file uploaded is not excel
  resetStaggerFileUpload <- reactiveVal(FALSE)
  output$stagger_fileupload <- renderUI({
    resetStaggerFileUpload() # reactive dependency
    resetStaggerFileUpload(FALSE)
    fileInput("stagger_file",
              p("Upload your raw data in xlsx format", span(a("[ Example File ]", href="https://raw.githubusercontent.com/ccfang2/HonestDiDSenAnlys/main/StaggeredDataFormat.xlsx"))),
              accept = ".xlsx")
  })
  
  observeEvent(input$stagger_file, {
    if(file_ext(input$stagger_file$name) != "xlsx"){
      resetStaggerFileUpload(TRUE)
      showModal(modalDialog("That's not an .xlsx file"))
    }
  })
  
  # Alert below will trigger if stagger_min_e or stagger_max_e is decimal
  observe({
    if (any((!is.na(input$stagger_min_e) & !is.integer(input$stagger_min_e)), (!is.na(input$stagger_max_e) & !is.integer(input$stagger_max_e)))) {
      stagger_integer_check <- "Smallest or Largest Relative Event Time must be integer, if any."
      stagger_integer_js_string <- 'alert("SOMETHING");'
      stagger_integer_js_string <- sub("SOMETHING",stagger_integer_check,stagger_integer_js_string)
      session$sendCustomMessage(type='jsCode', list(value = stagger_integer_js_string))
    }
  })
  
  # Alert below will trigger if stagger_min_e is larger than -1 or stagger_max_e is smaller than -1
  observe({
    if (any(all(!is.na(input$stagger_min_e), input$stagger_min_e> -1),all(!is.na(input$stagger_max_e), input$stagger_max_e< -1))) {
      stagger_e_compare_check <- "Smallest Event Time must be smaller than Base Event Time (= -1) because 'universal' base period is chosen in estimation; Largest Event Time must be larger than Base Event Time (= -1)."
      stagger_e_compare_js_string <- 'alert("SOMETHING");'
      stagger_e_compare_js_string <- sub("SOMETHING",stagger_e_compare_check,stagger_e_compare_js_string)
      session$sendCustomMessage(type='jsCode', list(value = stagger_e_compare_js_string))
    }
  })
  
  # ------------ Observe Start or Clear-----------------------------
  stagger_v <- reactiveValues(result = NULL)
  
  # Read in Excel Data
  stagger_read_data <- reactive({
    ext <- tools::file_ext(input$stagger_file$datapath)
    req(input$stagger_file)
    validate(need(ext == "xlsx", "Please upload an xlsx file"))
    return(read_excel(input$stagger_file$datapath))
  })
  
  # Observe Start
  observeEvent(input$stagger_start,execute_safely({
    
    # Check if data file is missing
    if (length(input$stagger_file)==0) stop("Please upload an .xlsx data file.")
    
    # Reformat Data
    stagger_data <- stagger_read_data()
    
    # Report Errors
    if (any((!is.na(input$stagger_min_e) & !is.integer(input$stagger_min_e)), (!is.na(input$stagger_max_e) & !is.integer(input$stagger_max_e))))
      stop("Smallest or Largest Relative Event Time must be integer, if any.")
    if (any(all(!is.na(input$stagger_min_e), input$stagger_min_e> -1),all(!is.na(input$stagger_max_e), input$stagger_max_e< -1)))
      stop("Smallest Event Time must be smaller than Base Event Time (= -1) because 'universal' base period is chosen in estimation; Largest Event Time must be larger than Base Event Time (= -1).")
    
    # reformat min_e and max_e
    stagger_min_e <- if (is.na(input$stagger_min_e)) {-Inf} else {input$stagger_min_e}
    stagger_max_e <- if (is.na(input$stagger_max_e)) {+Inf} else {input$stagger_max_e}
    
    # Calculation
    stagger_cs_results <- att_gt(yname = "y",
                                 tname = "time",
                                 idname = "id", 
                                 gname = "g", 
                                 data = stagger_data %>% mutate(g = ifelse(is.na(g), 2*max(as.numeric(time)), g)),
                                 allow_unbalanced_panel = as.logical(input$stagger_unbalance),
                                 control_group = input$stagger_control,
                                 est_method = input$stagger_method,
                                 alp = input$stagger_alpha,
                                 base_period = "universal")
    
    stagger_es <- aggte(stagger_cs_results, type = "dynamic", min_e = stagger_min_e, max_e = stagger_max_e)
   
    # recover influence function for event study estimates
    stagger_es_inf_func <- stagger_es$inf.function$dynamic.inf.func.e
    
    # recover variance-covariance matrix
    stagger_n <- nrow(stagger_es_inf_func)
    stagger_V <- t(stagger_es_inf_func) %*% stagger_es_inf_func / (stagger_n*stagger_n) 
    
    #Remove the coefficient normalized to zero
    stagger_referencePeriodIndex <- which(stagger_es$egt == -1)
    stagger_V <- stagger_V[-stagger_referencePeriodIndex,-stagger_referencePeriodIndex]
    stagger_beta <- stagger_es$att.egt[-stagger_referencePeriodIndex]
    
    # Result
    # Reference period has to be normalized in staggered timing, because the actual treatment period varies across individuals
    stagger_v$result<-list(data=stagger_data,
                           beta = stagger_beta,
                           sigma = stagger_V,
                           tVec = stagger_es$egt[-stagger_referencePeriodIndex],
                           referencePeriod = stagger_es$egt[stagger_referencePeriodIndex],
                           prePeriodIndices = stagger_es$egt[stagger_es$egt < -1],
                           postPeriodIndices = stagger_es$egt[stagger_es$egt > -1],
                           allow_unbalanced_panel = as.logical(input$stagger_unbalance),
                           control_group = input$stagger_control,
                           est_method = input$stagger_method,
                           alpha = input$stagger_alpha, 
                           min_e = stagger_min_e, 
                           max_e = stagger_max_e,
                           name = input$stagger_file$name)
  }))
  
  # Observe Reset
  observeEvent(input$stagger_reset, {
    stagger_v$result <- NULL
    reset("stagger_fileupload")
    reset("stagger_unbalance")
    reset("stagger_control")
    reset("stagger_method")
    reset("stagger_min_e")
    reset("stagger_max_e")
    reset("stagger_max_e")
    reset("stagger_alpha")
  })
  
  # Observe Disconnect
  observeEvent(input$stagger_disconnect, {
    session$close()
  })
  
  # ------------ Outputs -----------------------------
  
  # Clock
  output$stagger_currentTime <- renderText({
    invalidateLater(1000, session)
    stagger_tz <- Sys.timezone(location = TRUE)
    stagger_tz <- if (class(stagger_tz)!="character") "UTC" else stagger_tz
    # paste("[", lubridate::with_tz(Sys.time(), stagger_tz),"]")
    paste("[", Sys.time(),stagger_tz, "]")
  })
  
  # Group-Time ATT Estimation Output File
  stagger_beta <- reactive({
    if (is.null(stagger_v$result)) return()
    t(stagger_v$result$beta)
  })
  
  stagger_sigma <- reactive({
    if (is.null(stagger_v$result)) return()
    stagger_v$result$sigma
  })
  
  stagger_tVec <- reactive({
    if (is.null(stagger_v$result)) return()
    t(stagger_v$result$tVec)
  })
  
  stagger_ref <- reactive({
    if (is.null(stagger_v$result)) return()
    t(stagger_v$result$referencePeriod)
  })
  
  stagger_pre <- reactive({
    if (is.null(stagger_v$result)) return()
    t(stagger_v$result$prePeriodIndices)
  })
  
  stagger_post <- reactive({
    if (is.null(stagger_v$result)) return()
    t(stagger_v$result$postPeriodIndices)
  })
  
  # Event Study Plot
  stagger_betaplot <- reactive({
    if (is.null(stagger_v$result)) return()
    createEventStudyPlot(betahat = stagger_v$result$beta,
                         sigma = stagger_v$result$sigma,
                         numPrePeriods = length(stagger_v$result$prePeriodIndices),
                         numPostPeriods = length(stagger_v$result$postPeriodIndices),
                         alpha = stagger_v$result$alpha,
                         timeVec = stagger_v$result$tVec,
                         referencePeriod = stagger_v$result$referencePeriod,
                         useRelativeEventTime = F) + 
      labs(caption = "Base event time is -1, as 'universal' base period is chosen in estimation.")
  })
  
  output$stagger_betaplot <- renderPlot({
    input$stagger_start
    stagger_betaplot()
  })
  
  # R Code
  stagger_code <- reactive({
    if (is.null(stagger_v$result)) return()

    head <- c(" # Install HonestDiD package from https://github.com/asheshrambachan/HonestDiD, if not installed \n",
              "# install.packages('remotes') \n",
              "# Sys.setenv('R_REMOTES_NO_ERRORS_FROM_WARNINGS' = 'true') \n",
              "# remotes::install_github('asheshrambachan/HonestDiD') \n",
              "# Install did package, if not installed \n",
              "# install.packages('did') \n",
              "library(HonestDiD) \n",
              "library(did) \n",
              "library(readxl) \n \n",
              "options(warn = -1) \n \n")

    dataset <- c("# Read in Excel Data \n",
                 paste("# Complete Your Data Path if '", stagger_v$result$name,"' is not in Your Working Directory \n", sep=""),
                 paste("data <- read_excel('", stagger_v$result$name,"') \n \n", sep="")
    )

    attgt <- c("# Staggered DiD Estimation using Group-Time ATT \n",
               "stagger_cs_results <- att_gt(yname = 'y', \n",
               "\40 \40 \40 tname = 'time', \n",
               "\40 \40 \40 idname = 'id', \n",
               "\40 \40 \40 gname = 'g', \n",
               "\40 \40 \40 data = data %>% mutate(g = ifelse(is.na(g), 2*max(as.numeric(time)), g)), \n",
               paste("\40 \40 \40 allow_unbalanced_panel = ",stagger_v$result$allow_unbalanced_panel,", \n", sep=""),
               paste("\40 \40 \40 control_group = '",stagger_v$result$control_group,"', \n", sep=""),
               paste("\40 \40 \40 est_method = '",stagger_v$result$est_method,"', \n", sep=""),
               paste("\40 \40 \40 alp = ",stagger_v$result$alpha,", \n", sep=""),
               "\40 \40 \40 base_period = 'universal' \n",
               ") \n \n",
               paste("stagger_es <- aggte(stagger_cs_results, type = 'dynamic', min_e = ",stagger_v$result$min_e,", max_e = ",stagger_v$result$max_e,") \n \n", sep=""),
               "# recover influence function for event study estimates \n",
               "stagger_es_inf_func <- stagger_es$inf.function$dynamic.inf.func.e \n \n",
               "# recover variance-covariance matrix \n",
               "stagger_n <- nrow(stagger_es_inf_func) \n",
               "stagger_V <- t(stagger_es_inf_func) %*% stagger_es_inf_func / (stagger_n*stagger_n) \n \n",
               "#Remove the coefficient normalized to zero \n",
               "stagger_referencePeriodIndex <- which(stagger_es$egt == -1) \n",
               "stagger_V <- stagger_V[-stagger_referencePeriodIndex,-stagger_referencePeriodIndex] \n",
               "stagger_beta <- stagger_es$att.egt[-stagger_referencePeriodIndex] \n",
               "stagger_tVec <- stagger_es$egt[-stagger_referencePeriodIndex] \n",
               "stagger_referencePeriod <- stagger_es$egt[stagger_referencePeriodIndex] \n",
               "stagger_prePeriodIndices <- stagger_es$egt[stagger_es$egt < -1] \n",
               "stagger_postPeriodIndices <- stagger_es$egt[stagger_es$egt > -1] \n \n"
    )

    eventPlot <- c("# Create Event Study Plot \n",
                   "event_study_plot <- createEventStudyPlot(betahat = stagger_beta, \n",
                   "\40 \40 \40 sigma = stagger_V, \n",
                   "\40 \40 \40 numPrePeriods = length(stagger_prePeriodIndices), \n",
                   "\40 \40 \40 numPostPeriods = length(stagger_postPeriodIndices), \n",
                   paste("\40 \40 \40 alpha = ", stagger_v$result$alpha, ", \n",sep=""),
                   "\40 \40 \40 timeVec = stagger_tVec, \n",
                   "\40 \40 \40 referencePeriod = stagger_referencePeriod, \n",
                   "\40 \40 \40 useRelativeEventTime = F) + \n",
                   "\40 \40 labs(caption = 'Base event time is -1, as universal base period is chosen in estimation.') \n",
                   "event_study_plot \n \n"
    )

    outputTable <- c("# Create Group-Time ATT Estimation Output Table File to Working Directory \n",
                     "WriteXLS(list(beta=as.data.frame(t(stagger_beta)), \n",
                     "\40 \40 \40 sigma =as.data.frame(stagger_V), \n",
                     "\40 \40 \40 tVec = as.data.frame(t(stagger_tVec)), \n",
                     "\40 \40 \40 referencePeriod = as.data.frame(t(stagger_referencePeriod)), \n",
                     "\40 \40 \40 prePeriodIndices = as.data.frame(t(stagger_prePeriodIndices)), \n",
                     "\40 \40 \40 postPeriodIndices = as.data.frame(t(stagger_prePeriodIndices))), \n",
                     "\40 \40 'Staggered_Group_Time_ATT.xlsx') \n \n"
    )

    tail <- c("options(warn = 0) \n")

    paste0(c(head, dataset, attgt, eventPlot, outputTable, tail), sep="")
  })

  output$stagger_code <- renderText({
    input$stagger_start
    stagger_code()
  })
  
  # ------------ Downloads -----------------------------
  # Group-Time ATT estimation output file download
  output$stagger_outputdownload <- downloadHandler(
    filename = function(){
      paste("Staggered_Group_Time_ATT.xlsx",sep="")
    },
    content = function(file){
      WriteXLS(list(beta = as.data.frame(stagger_beta()),
                    sigma = as.data.frame(stagger_sigma()),
                    tVec = as.data.frame(stagger_tVec()),
                    referencePeriod = as.data.frame(stagger_ref()),
                    prePeriodIndices = as.data.frame(stagger_pre()),
                    postPeriodIndices = as.data.frame(stagger_post())
      ), file)
    }
  )
  
  # event-study plot download
  output$stagger_betaplotdownload <- downloadHandler(
    filename <- function(){
      paste("Event_Study_Plot.png",sep="")
    },
    content <- function(file){
      ggsave(file, plot=stagger_betaplot(),device = "png")
    }
  )
  
}

