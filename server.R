library(shiny)
library(shinyAce)
library(irr)
library(psych)
library(vcd)
library(lattice)
library(reshape2)
library(DescTools)



shinyServer(function(input, output) {


#----------------------------------------------------
# 1. 2 Raters (Nominal) ------
#----------------------------------------------------

    data1 <- reactive({
        
        dat <- read.csv(text=input$text1, sep="", na.strings=c("","NA","."))
        
          dat[,1] <- as.character(dat[,1])
          dat[,2] <- as.character(dat[,2])
          z <- (c(dat[,1], dat[,2]))
          lvls <- unique(z)
          levels(dat[,1]) <- unique(lvls)
          levels(dat[,2]) <- unique(lvls)
          a <- with(dat, table(dat[,1], dat[,2]))
          names(dimnames(a)) <- colnames(dat)
          x <- a
          x <- addmargins(x)
          print(x)
        
          cat("\n", "----------", "\n") 
         
          # percentage agreement
          pctagree <- agree(dat)
          cat("\n")  
          print(pctagree)  
        
          # percentage
          pct <- round(a/sum(a),2)
          cat("\n")  
          print(pct)
        
    })
    
    output$data1.out <- renderPrint({
          data1()
    })
    
    
    
    
    
    test1 <- reactive({
        
        dat <- read.csv(text=input$text1, sep="", na.strings=c("","NA","."))
        
          res1 <- kappa2(dat)
          print(res1)  
          
          cat("\n")
          cat("95% confidence interval (CI) of kappa", "\n")
        
          dat[,1] <- as.character(dat[,1])
          dat[,2] <- as.character(dat[,2])
          z <- (c(dat[,1], dat[,2]))
          lvls <- unique(z)
          levels(dat[,1]) <- unique(lvls)
          levels(dat[,2]) <- unique(lvls)
          a <- with(dat, table(dat[,1], dat[,2]))
          names(dimnames(a)) <- colnames(dat)
          kappa <- cohen.kappa(a)
          res2 <- kappa[8]$confid[1,]
          print(res2)
        
          cat("\n", "----------", "\n") 
        
          cat("\n")
        
          res3 <- kappa2(dat, "squared")
          print(res3)
        
          cat("\n")
          cat("95% confidence interval (CI) of kappa", "\n")
        
          res4 <- kappa[8]$confid[2,]
          print(res4)
       
    })
    
    output$test1.out <- renderPrint({
        test1()
    })





    Krippendorff1 <- reactive({
  
        dat <- read.csv(text=input$text1, sep="", na.strings=c("","NA","."))
  
        options(warn=-1)
        kripp.alpha(t(dat), method="nominal")
  
    })

    output$Krippendorff1.out <- renderPrint({
        Krippendorff1()
    })





    makepPlot1 <- function(){
        
        dat <- read.csv(text=input$text1, sep="", na.strings=c("","NA","."))
        
          dat[,1] <- as.character(dat[,1])
          dat[,2] <- as.character(dat[,2])
          z <- (c(dat[,1], dat[,2]))
          lvls <- unique(z)
          levels(dat[,1]) <- unique(lvls)
          levels(dat[,2]) <- unique(lvls)
          a <- with(dat, table(dat[,1], dat[,2]))
          names(dimnames(a)) <- colnames(dat)
                
          agreementplot(t(a))
        
    }
    
    output$pPlot1 <- renderPlot({
        print(makepPlot1())
    })
    
    
    
  
  
     info1 <- reactive({
        info1 <- paste("This analysis was conducted with ", strsplit(R.version$version.string, " \\(")[[1]][1], ".", sep = "")
        info2 <- paste("It was executed on ", date(), ".", sep = "")
        cat(sprintf(info1), "\n")
        cat(sprintf(info2), "\n")
    })
    
    output$info1.out <- renderPrint({
        info1()
    })










#----------------------------------------------------
# 2. 2 Raters (Ordinal) ------
#----------------------------------------------------

    data2 <- reactive({
        
        dat <- read.csv(text=input$text2, sep="", na.strings=c("","NA","."))
                
        dat[,1] <- as.character(dat[,1])
        dat[,2] <- as.character(dat[,2])
        z <- (c(dat[,1], dat[,2]))
        lvls <- unique(z)
        levels(dat[,1]) <- unique(lvls)
        levels(dat[,2]) <- unique(lvls)
        a <- with(dat, table(dat[,1], dat[,2]))
        names(dimnames(a)) <- colnames(dat)
        x <- a
        x <- addmargins(x)
        print(x)
        
        cat("\n", "----------", "\n") 
        
        # percentage agreement
        pctagree <- agree(dat)
        cat("\n")  
        print(pctagree)  
        
        # percentage
        pct <- round(a/sum(a),2)
        cat("\n")  
        print(pct)

        })
    
        output$data2.out <- renderPrint({
            data2()
        })
    
    
    
    
    
    test2 <- reactive({
        
        dat <- read.csv(text=input$text2, sep="", na.strings=c("","NA","."))
        
        res1 <- kappa2(dat, "squared")
        print(res1)  
        
        cat("\n")
        cat("95% confidence interval (CI) of kappa", "\n")
        
        dat[,1] <- as.character(dat[,1])
        dat[,2] <- as.character(dat[,2])
        z <- (c(dat[,1], dat[,2]))
        lvls <- unique(z)
        levels(dat[,1]) <- unique(lvls)
        levels(dat[,2]) <- unique(lvls)
        a <- with(dat, table(dat[,1], dat[,2]))
        names(dimnames(a)) <- colnames(dat)
        kappa <- cohen.kappa(a)
        res2 <- kappa[8]$confid[2,]
        print(res2)
            
    })
    
    output$test2.out <- renderPrint({
        test2()
    })
    
    
    
    
    correl2 <- reactive({
  
      dat <- read.csv(text=input$text2, sep="", na.strings=c("","NA","."))
      
        type <- switch(input$method,
                     Spearman = "spearman",
                     Kendall = "kendall")
      
        r <- cor(dat[,1], dat[,2], method = type, use = "complete")
        n <- length(dat[,1])
      
        if (input$method == "Kendall") {
          # [Kendall CI] http://www.stat.umn.edu/geyer/5601/examp/corr.html
          conf.level <- 0.95
          signs <- sign(outer(dat[,1], dat[,1], "-") * outer(dat[,2], dat[,2], "-"))
          tau <- mean(signs[lower.tri(signs)])
          cvec <- apply(signs, 1, sum)
          nn <- length(cvec)
          sigsq <- (2 / (nn * (nn - 1))) *
                    (((2 * (nn - 2)) / (nn * (nn - 1))) * var(cvec)
                    + 1 - tau^2)
          zcrit <- qnorm((1 + conf.level) / 2)
          ci <- tau + c(-1, 1) * zcrit * sqrt(sigsq)
        
        } else {
          ci <- round(r.con(r, n), 3)
          # [Spearman CI] http://www.statsdirect.com/help/default.htm#nonparametric_methods/spearman.htm
        }
      
        pvl <- cor.test(dat[,1], dat[,2], method = type)
      
        if (input$method == "Spearman") {
              cortype <- c("Spearman's ρ =")
        } else {
              cortype <- c("Kendall's τ =")
        }
      
        cat("Correlation between", colnames(dat)[1], "and", colnames(dat)[2], ":", "\n",
            cortype, round(r, 3), "\n",
           "95% CI [lower, upper] =", round(ci, 3), "\n",
           "p-value =", round(pvl$p.value, 3), "\n")
  
    })

    output$correl2.out <- renderPrint({
        correl2()
    })





    Krippendorff2 <- reactive({
  
        dat <- read.csv(text=input$text2, sep="", na.strings=c("","NA","."))
  
        options(warn=-1)
        kripp.alpha(t(dat), method="ordinal")
        
    })

    output$Krippendorff2.out <- renderPrint({
         Krippendorff2()
    })





    makepPlot2 <- function(){
        
        dat <- read.csv(text=input$text2, sep="", na.strings=c("","NA","."))
          
          dat[,1] <- as.character(dat[,1])
          dat[,2] <- as.character(dat[,2])
          z <- (c(dat[,1], dat[,2]))
          lvls <- unique(z)
          levels(dat[,1]) <- unique(lvls)
          levels(dat[,2]) <- unique(lvls)
          a <- with(dat, table(dat[,1], dat[,2]))
          names(dimnames(a)) <- colnames(dat)
        
          agreementplot(t(a))        
    
    }
    
    output$pPlot2 <- renderPlot({
        print(makepPlot2())
    })
    
    
    
    
      
    makemPlot2 <- function(){
        
        dat <- read.csv(text=input$text2, sep="", na.strings=c("","NA","."))
                
        person <- c(1:nrow(dat))
        x <- data.frame(person, dat)
        x[,1] <- factor(x[,1])
        data.long <- melt(x, idvars=x[,1])
        
        xyplot(value~variable, group=person, type="b", data=data.long, 
              main = list(label="Agreement of ratings given by each rater", cex=1.5), 
              xlab=list(label=""), ylab=list(label="Rating", cex=1.5), cex=1.5,
              scales = list(y=list(at=c(0:round(max(dat, na.rm=TRUE)+1,0)), cex=1.5), x=list(cex=1.5))
        )
        
        
    }
    
    output$mPlot2 <- renderPlot({
        print(makemPlot2())
    })





    info2 <- reactive({
        info1 <- paste("This analysis was conducted with ", strsplit(R.version$version.string, " \\(")[[1]][1], ".", sep = "")
        info2 <- paste("It was executed on ", date(), ".", sep = "")
        cat(sprintf(info1), "\n")
        cat(sprintf(info2), "\n")
    })
    
    output$info2.out <- renderPrint({
        info2()
    })










#----------------------------------------------------
# 3. 3 or More Raters (Nominal)
#----------------------------------------------------

    data3 <- reactive({
                
        dat <- read.csv(text=input$text3, sep="", na.strings=c("","NA","."))
        
        freq.c <- function(x) {
          uniq <- c()
          for (i in 1:length(x)) {
            x[,i] <- as.character(x[,i]) 
            fr <- sort(unique(x[,i]))
            uniq <- c(uniq, fr)
          }
          lbls <- sort(unique(uniq))
        }
        ctg <- freq.c(dat)
        
        l <- apply(dat, 2, table)
        
        if (is.matrix(l)) { # if 0 observation DOES NOT exist
          
          x <- l 
          x <- addmargins(x)
          print(x)
        
        } else { # if 0 observation exists
          
          x2 <- lapply(l, FUN=function(X) X[ctg])
          mat <- do.call("cbind", x2)
          row.names(mat) <- ctg
          mat[is.na(mat)] <- 0
          x <- addmargins(mat)
          print(x)
        
        }
        
    })
      
    output$data3.out <- renderPrint({
        data3()
    })
      
      
      
      
      
    test3 <- reactive({
        
        dat <- read.csv(text=input$text3, sep="", na.strings=c("","NA","."))
        
        res1 <- kappam.fleiss(dat)
        print(res1)
        
        cat("\n")
        cat("95% confidence interval (CI) of kappa", "\n")
        
        res2 <- Kappam(dat, method = "Fleiss", conf.level=0.95)
        print(res2)
        
    })
      
    output$test3.out <- renderPrint({
        test3()
    })
      
      
      
    

    Krippendorff3 <- reactive({
        
        dat <- read.csv(text=input$text3, sep="", na.strings=c("","NA","."))
        
        options(warn=-1)
        kripp.alpha(t(dat), method="nominal")
        
    })
      
    output$Krippendorff3.out <- renderPrint({
        Krippendorff3()
    })
      
      
      
      
      
    makepPlot3 <- function(){
        
        dat <- read.csv(text=input$text3, sep="", na.strings=c("","NA","."))  
        
        dat <- as.data.frame(lapply(dat,as.numeric))
        person <- c(1:nrow(dat))
        dat <- data.frame(person, dat)
        dat[,1] <- factor(dat[,1])
        data.long <- melt(dat, idvars=dat[,1])
        
        xyplot(value~variable, group=person, type="b", data=data.long, 
               main = list(label="Agreement of ratings given by each rater", cex=1.5), 
               xlab=list(label=""), ylab=list(label="Rating", cex=1.5), cex=1.5,
               scales = list(y=list(at=c(0:round(max(data.long$value, na.rm=TRUE)+1,0)), cex=1.5), x=list(cex=1.5))
              )
    }

    output$pPlot3 <- renderPlot({
        print(makepPlot3())
    })
      
      
      
      
      
    makemPlot3 <- function(){
  
        dat <- read.csv(text=input$text3, sep="", na.strings=c("","NA","."))
  
        freq.c <- function(x) {
          uniq <- c()
          for (i in 1:length(x)) {
             x[,i] <- as.character(x[,i]) 
             fr <- sort(unique(x[,i]))
             uniq <- c(uniq, fr)
          }
            lbls <- sort(unique(uniq))
        }
        ctg <- freq.c(dat)
  
        l <- apply(dat, 2, table)
  
        if (is.matrix(l)) {
            mat <- l
        } else {
            x2 <- lapply(l, FUN=function(X) X[ctg])
            mat <- do.call("cbind", x2)
            row.names(mat) <- ctg
            mat[is.na(mat)] <- 0.1 # change 0 observatios into 0.1 to show in a graph
        }
          
        par(mar=c(3, 5, 2, 8), xpd=TRUE)
        barplot(t(mat), beside=T, yaxt="n", ylab="Frequency", col=c(1:ncol(mat)))
        my.at <- round(c(0:max(mat),0))
        axis(2, at = my.at, las=1, labels = my.at)
        legend("topright", inset=c(-0.2, 0), legend = colnames(mat), fill=c(1:ncol(mat))) # Add legend to top right, outside plot region
        
    }

    output$mPlot3 <- renderPlot({
        print(makemPlot3())
    })





    info3 <- reactive({
        info1 <- paste("This analysis was conducted with ", strsplit(R.version$version.string, " \\(")[[1]][1], ".", sep = "")
        info2 <- paste("It was executed on ", date(), ".", sep = "")
        cat(sprintf(info1), "\n")
        cat(sprintf(info2), "\n")
    })
    
    output$info3.out <- renderPrint({
        info3()
    })










#----------------------------------------------------
# 4. 3 or More Raters (Ordinal)
#----------------------------------------------------

    data4 <- reactive({
  
       dat <- read.csv(text=input$text4, sep="", na.strings=c("","NA","."))
       
      freq.c <- function(x) {
        uniq <- c()
         for (i in 1:length(x)) {
            x[,i] <- as.character(x[,i]) 
            fr <- sort(unique(x[,i]))
            uniq <- c(uniq, fr)
          }
        lbls <- sort(unique(uniq))
      }
      ctg <- freq.c(dat)
  
      l <- apply(dat, 2, table)
  
      if (is.matrix(l)) { # if 0 observation DOES NOT exist
    
      x <- l 
      x <- addmargins(x)
      print(x)
    
      } else { # if 0 observation exists
    
      x2 <- lapply(l, FUN=function(X) X[ctg])
      mat <- do.call("cbind", x2)
      row.names(mat) <- ctg
      mat[is.na(mat)] <- 0
      x <- addmargins(mat)
      print(x)
    
      }
  
    })

    output$data4.out <- renderPrint({
      data4()
    })





  test4 <- reactive({
  
    dat <- read.csv(text=input$text4, sep="", na.strings=c("","NA","."))
  
    res1 <- kappam.fleiss(dat)
    print(res1)
  
    cat("\n")
    cat("95% confidence interval (CI) of kappa", "\n")
  
    res2 <- Kappam(dat, method = "Fleiss", conf.level=0.95)
    print(res2)
  
  })

  output$test4.out <- renderPrint({
    test4()
  })





  W <- reactive({
    
    dat <- read.csv(text=input$text4, sep="", na.strings=c("","NA","."))
    
    kendall(dat, TRUE)
    
  })
  
  output$W.out <- renderPrint({
    W()
  })





  Krippendorff4 <- reactive({
  
    dat <- read.csv(text=input$text4, sep="", na.strings=c("","NA","."))
  
      options(warn=-1)
      kripp.alpha(t(dat), "ordinal")
  
  })

  output$Krippendorff4.out <- renderPrint({
      Krippendorff4()
  })





  makepPlot4 <- function(){
  
  dat <- read.csv(text=input$text4, sep="", na.strings=c("","NA","."))  
  
    dat <- as.data.frame(lapply(dat,as.numeric))
    person <- c(1:nrow(dat))
    dat <- data.frame(person, dat)
    dat[,1] <- factor(dat[,1])
    data.long <- melt(dat, idvars=dat[,1])
  
    xyplot(value~variable, group=person, type="b", data=data.long, 
          main = list(label="Agreement of ratings given by each rater", cex=1.5), 
          xlab=list(label=""), ylab=list(label="Rating", cex=1.5), cex=1.5,
          scales = list(y=list(at=c(0:round(max(data.long$value, na.rm=TRUE)+1,0)), cex=1.5), x=list(cex=1.5))
    )
  }

  output$pPlot4 <- renderPlot({
      print(makepPlot4())
  })





  makemPlot4 <- function(){
  
  dat <- read.csv(text=input$text4, sep="", na.strings=c("","NA","."))
  
  freq.c <- function(x) {
    uniq <- c()
    for (i in 1:length(x)) {
      x[,i] <- as.character(x[,i]) 
      fr <- sort(unique(x[,i]))
      uniq <- c(uniq, fr)
    }
    lbls <- sort(unique(uniq))
  }
  ctg <- freq.c(dat)
  
  l <- apply(dat, 2, table)
  
  if (is.matrix(l)) {
    mat <- l
  } else {
    x2 <- lapply(l, FUN=function(X) X[ctg])
    mat <- do.call("cbind", x2)
    row.names(mat) <- ctg
    mat[is.na(mat)] <- 0.1 # change 0 observatios into 0.1 to show in a graph
  }
    
  par(mar=c(3, 5, 2, 8), xpd=TRUE)
  barplot(t(mat), beside=T, yaxt="n", ylab="Frequency", col=c(1:ncol(mat)))
  my.at <- round(c(0:max(mat),0))
  axis(2, at = my.at, las=1, labels = my.at)
  legend("topright", inset=c(-0.2, 0), legend = colnames(mat), fill=c(1:ncol(mat))) # Add legend to top right, outside plot region
  
}

output$mPlot4 <- renderPlot({
  print(makemPlot4())
})





  info4 <- reactive({
    info1 <- paste("This analysis was conducted with ", strsplit(R.version$version.string, " \\(")[[1]][1], ".", sep = "")
    info2 <- paste("It was executed on ", date(), ".", sep = "")
    cat(sprintf(info1), "\n")
    cat(sprintf(info2), "\n")
  })

  output$info4.out <- renderPrint({
    info4()
  })






})
