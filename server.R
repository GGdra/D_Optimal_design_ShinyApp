library(shiny)
library(shinycssloaders)
library(ggplot2)
library(tibble)
library(openxlsx)
library(lme4)
library(car)


source("rutgers.optimal.cat.blocks.random.ver.1.R")
function(input, output) {
  
  rv <- reactiveValues()
  
  # Logic for rutgers.optimal function
  results <- eventReactive(input$calc, {
    
    # Record the start time
    start_time <- Sys.time()
    
    
    # Store last inputs in reactive values
    rv$n_fact <- input$n_fact
    rv$n_cat <- input$n_cat
    rv$in_frml <- input$in_frml
    rv$size <- input$size
    rv$blocks <- input$blocks
    rv$r_starts <- input$r_starts
    rv$iter <- input$iter
    rv$random_blocks <- input$random_blocks
    
    
    req(input$n_fact, input$n_cat, input$in_frml, input$size, 
        input$blocks, input$r_starts, input$iter)
    resu<-tryCatch({
      rutgers.optimal.cat.blocks.random(n.fact = input$n_fact, 
                                 n.cat = input$n_cat,
                                 in.frml = input$in_frml,
                                 size = input$size,
                                 blocks = input$blocks,
                                 r.starts = input$r_starts,
                                 iter = input$iter,
                                 random.blocks = input$random_blocks)
      
      
      
    }, error = function(e) {
      NULL  # Handle errors
    }
    )
    rv$resu <- resu
    end_time <- Sys.time()  # Capture the end time
    duration <- end_time - start_time  # Calculate duration
    list(result = resu, duration = duration)
  })
  
  # Optionally, display the timing information
  output$time_lapsed <- renderText({
    res <- results()
    if (!is.null(res$duration)) {
      duration_seconds <- as.numeric(res$duration, units = "secs")  # Convert duration to numeric seconds
      rounded_duration <- round(duration_seconds, 2)  # Round to 2 decimal places
      paste("Calculation time:", rounded_duration, "seconds")
    }
  })
  
  
  # Render the results for Rutgers Optimal
  output$rutgers_result <- renderUI({
    res <- results()$result
    if (is.null(res)) {
      return(HTML("Invalid input or error in calculation"))
    }
    
    # Separate rendering for Defficiency and power
    #tagList(
    #  h4("Defficiency"),
    #  verbatimTextOutput("deff_output"),
    #  h4("Power"),
    #  tableOutput("power_output")
    #)
  })
  
  output$deff_output <- renderText({
    res <- results()$result
    if (!is.null(res)) {
      res$Deff
    }
  })
  
  output$aeff_output <- renderText({
    res <- results()$result
    if (!is.null(res)) {
      res$Aeff
    }
  })
  
  output$geff_output <- renderText({
    res <- results()$result
    if (!is.null(res)) {
      res$Geff
    }
  })
  
  output$power_output <- renderTable({
    res <- results()$result
    if (!is.null(res) && !is.null(res$power)) {
      as.data.frame(res$power)
    }
  })
  
  output$xbase_output <- renderTable({
    res <- results()$result
    if (!is.null(res) && !is.null(res$X)) {
      res$X
    }
  })
  
  output$download_excel <- downloadHandler(
    filename = function() {
      paste("DOE_results", ".xlsx", sep = "")
    },
    content = function(file) {
      res <- results()$result
      if (!is.null(res)) {
        write.xlsx(x = res, file = file)
      }
    }
  )    
    output$download_excel2 <- downloadHandler(
      
    filename = function() {
      paste("Test_results", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      # Create a new workbook
      wb <- createWorkbook()
      
      # Add sheets for ANOVA results if they're not null
      if (!is.null(result()$anova_fixed)) {
        addWorksheet(wb, "ANOVA Fixed")
        writeData(wb, "ANOVA Fixed", as.data.frame(result()$anova_fixed))
      }
      if (!is.null(result()$anova_random)) {
        addWorksheet(wb, "ANOVA Random")
        writeData(wb, "ANOVA Random", as.data.frame(result()$anova_random))
      }
      
      # Save the plot to a temporary file
      plot_file <- tempfile(fileext = ".png")
      ggsave(plot_file, plot = result()$plot, width = 7, height = 5, dpi = 300)
      
      # Add the plot image to a new sheet in the workbook
      addWorksheet(wb, "Plot")
      insertImage(wb, "Plot", plot_file)
      
      # Save the workbook to the specified file
      saveWorkbook(wb, file, overwrite = TRUE)
      
      # Clean up the temporary plot file
      unlink(plot_file)
    }
    
  )
  
  
  result <- eventReactive(input$exp, {
    
    my.des <- rv$resu
    
    resp <- input$resp
    #catf <- input$catf
    rblock <- input$rblock
    #catf_input <- gsub("[()]", "", input$catf)
    catf <- eval(parse(text = paste("c", input$catf, sep="")))
    
    # blocks are truly random
    set.seed(234)
    X = my.des$X
    X$y <- eval(parse(text=resp)) 
    X$y <- X$y + catf[as.numeric(X$c)]
    actual.blocks = sort(rnorm(rv$blocks,0,1))
    X$y = X$y + sort(rep(actual.blocks,rv$size)) +rnorm(input$rblock,0,1) # add random block effect
    #
    # Ensure this is done before model fitting
    X$Blocks <- factor(rep(1:rv$blocks, each = rv$size))
    
    formula_fixed <- as.formula(paste("y", rv$in_frml, "+Blocks"))
    m.fixed <- lm(formula_fixed, data = X)
    anova_result_fixed <- Anova(m.fixed, test = "F", type = 2)
    
    # Fit a random effect for blocks
    formula_random <- as.formula(paste("y", rv$in_frml, "+(1|Blocks)"))
    m.random <- lmer(formula_random, data = X)
    anova_result_random <- Anova(m.random, test = "F", type = 2)
    
    b.f = coef(m.fixed)[c(6:9)]
    b.f = c(b.f[1:4],-sum(b.f[1:4])) # contrasts contr.sum used
    b.r = lme4::ranef(m.random)$Blocks[,1]
    #
    block.comp = data.frame(set=c(rep("Actual",5),rep("Fixed",5),rep("Random",5)),
                            block=rep(1:5,3),values=c(actual.blocks,b.f,b.r))
    
    plot <- ggplot(block.comp,aes(x=block,y=values,group=set,color=set)) +
      geom_hline(yintercept=0,color="gray48") +
      geom_point(size=2) + geom_line() + ggsci::scale_color_nejm() +
      labs(x="Block",y="Value or Estimated Value",
           title = "Block Estimate Comparisons",color="")
    
    # Return a list that includes everything you want to use outside this reactive context
    list(X = X, anova_fixed = anova_result_fixed, anova_random = anova_result_random, plot = plot)
    
  })
  output$anovaFixedOutput <- renderTable({
    result()$anova_fixed
  })
  
  output$anovaRandomOutput <- renderTable({
    result()$anova_random
  })
  output$plot <- renderPlot({
    result()$plot
  })
  
}