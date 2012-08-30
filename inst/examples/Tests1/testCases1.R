
makeOptions <- function(x, base = ".")
  {
    opt <- rep("-I 1", nrow(x))
    opt <- ifelse(x$bands, paste(opt, "-u", x$bands), opt)
    opt <- ifelse(x$cf != .25, paste(opt, "-c", x$cf*100), opt)
    opt <- ifelse(x$winnow, paste(opt, "-w"), opt)
    opt <- ifelse(x$subset, paste(opt, "-s"), opt)
    opt <- ifelse(x$rules,  paste(opt, "-r"), opt)    
    opt <- ifelse(x$fuzzy,  paste(opt, "-p"), opt)
    opt <- ifelse(x$noGlobal, paste(opt, "-g"), opt)
    opt <- ifelse(x$trials > 1, paste(opt, "-b -t", x$trials), opt)
    opt <- ifelse(x$sample > 0, paste(opt, "-S", x$sample*100), opt)
    opt <- ifelse(x$minCases > 2, paste(opt, "-m", x$minCases), opt)      
    call <- paste(base, "/c5.0 -f TestData ", opt, sep = "")
    call
  }

makePredictOptions <- function(x , c50predict) {
  opt <- "-f TestData"
  opt <- ifelse(x$rules,  paste(opt, "-r"), opt)
  call <-paste(c50predict , opt)
  call
}


makeControl <- function(x)
  {
    C5.0Control(seed = 1, 
                winnow = x$winnow,
                subset = x$subset,
                fuzzyThreshold = x$fuzzy,
                noGlobalPruning = x$noGlobal,
                sample = x$sample,
                minCases = x$minCases,
                CF = x$cf, bands = x$bands)
  }


trimOutput <- function(x, split = TRUE)
  {
    if(split) x <- strsplit(x, "\n")[[1]]
    bottomIndex <- grep("^Read", x)
    x <- x[-(1:bottomIndex)]    
    topIndex <- grep("^\tAttribute usage", x)
    x <- x[-((topIndex+1):length(x))]        
    x
  }

makeHeader <- function(x, i = "")
  {
    cat("\nTest Case ", i, ": ", ifelse(x$rules, "rules, ", ""),
        ifelse(x$subset, "subsetting, ", ""),
        ifelse(x$winnow, "winnowing, ", ""),
        ifelse(x$fuzzy, "fuzzy thresholds, ", ""),
        ifelse(x$noGlobal, "no global pruning, ", ""),
        ifelse(x$bands > 0, "bands, ", ""),
        ifelse(x$cf != 0.25, "CF 0.75, ", ""),
        ifelse(x$trials > 1, "boosting, ", ""),
        ifelse(x$sample > 0, "sampling, ", ""),
        ifelse(x$minCases > 2, "min. cases, ", ""),         
        "\n", sep = "")
  }

parsePredictions <- function(x, lvl = NULL)
  {
    startIndex <- grep("   1\t\t", x, fixed = TRUE)
    x <- x[-(1:(startIndex - 1))]
    lineStart <- regexpr("\t\t", x[length(x)])
    x <- substring(x, lineStart+2)
    x <- gsub("(\\[)|(\\])", "", x)
    x <- strsplit(x, "[[:space:]]")
    x <- lapply(x, function(x) x [x!= ""])
    data.frame(obs = factor(unlist(lapply(x, function(x) x[1])), levels = lvl),
               pred = factor(unlist(lapply(x, function(x) x[2])), levels = lvl),
               conf = as.numeric(unlist(lapply(x, function(x) x[3]))))
  }



## path to command line version
c50Path    <- "/Users/yorick/C50_original"
c50Predict <- "/Users/yorick/C50_original/tmp1/sample"

c50Path    <- "/Users/kuhna03/Code/C50clean"
c50Predict <- "/Users/kuhna03/Code/C5predict/sample"
testPath   <- "/Users/kuhna03/Code/rulebasedmodels/pkg/C5/inst/examples/Tests1"

setwd(testPath)



library(C50)

raw <- read.delim("http://www.sgi.com/tech/mlc/db/german.all", sep = ",", header = FALSE)

factorCols <- unlist(lapply(raw, is.factor))
factorCols <- names(factorCols)[factorCols]

for(i in factorCols) raw[,i] <- factor(letters[as.numeric(raw[,i])])

tmp <- C5.0(raw[, -ncol(raw)], raw$V21)
summary(tmp)


nms <- C50:::makeNamesFile(raw[, -ncol(raw)], raw$V21)
dat <- C50:::makeDataFile(raw[, -ncol(raw)], raw$V21)

cat(nms, file = file.path(testPath, "TestData.names"))
cat(dat, file = file.path(testPath, "TestData.data"))
cat(dat, file = file.path(testPath, "TestData.cases"))
TestData <- raw



combos <- expand.grid(bands = c(0, 3),
                      cf = c(.25, .75),
                      winnow = c(TRUE, FALSE),
                      subset = c(TRUE, FALSE),
                      rules = c(TRUE, FALSE),
                      fuzzy = c(TRUE, FALSE),
                      noGlobal = c(TRUE, FALSE),
                      trials = c(1, 12),
                      minCases = c(2, 10),
                      sample = c(0, .50))

throwOut <- combos$bands & !combos$rules
combos <- combos[!throwOut,]

outputs <- vector(mode = "list", length = nrow(combos))

combos$class <- NA
combos$prob <- NA
combos$case <- 1:nrow(combos)

for(i in 1:nrow(combos))
  {
    makeHeader(combos[i,], i)
    cat(rep("-", 60), "\n", sep = "")
    cat("   ", makeOptions(combos[i,], c50Path), "\n")
    output <- system(makeOptions(combos[i,], c50Path), intern = TRUE)
    ctrl <-  makeControl(combos[i,])
    fit <- C5.0(TestData[,-ncol(TestData)], TestData$V21,
                trials = combos[i,"trials"],
                rules = combos[i,"rules"],
                control = ctrl)

    cat("   ", makePredictOptions(combos[i,], c50Predict), "\n")

    expected <- system(makePredictOptions(combos[i,], c50Predict), intern = TRUE)
    expected <- parsePredictions(expected, lvl = levels(TestData$V21))
    expConf <- expected$conf

    pred <- predict.C5.0(fit, TestData[,-ncol(TestData)])

    cat("   Classes:")
    print(all.equal(expected$pred, pred))
    if(is.logical(all.equal(expected$pred, pred))) combos$class[i] <- ifelse(all.equal(expected$pred, pred), 1, -1)
    if(!is.logical(all.equal(expected$pred, pred))) combos$class[i] <- 0
    
    predConf <- predict.C5.0(fit, TestData[,-ncol(TestData)], type = "prob")
    totalConf <- apply(predConf, 1, sum)
    maxConf <- apply(predConf, 1, max)
    names(maxConf) <- NULL
    cat("   Probabilities:")
    probCheck <- all.equal(round(maxConf, 4), round(expConf, 4))
    print(probCheck)
    if(class(probCheck)[1] == "character" && combos[i,"trials"] ==1 && combos[i,"rules"])
      {
        cat("That's ok, we fixed an issue with C5\n")
         combos$prob[i] <- -1
      }
    pDiff <- mean(abs(maxConf - expConf))
    if(is.logical(probCheck)) combos$prob[i] <- ifelse(probCheck, 1, pDiff)
    if(!is.logical(probCheck)) combos$prob[i] <- pDiff
    
    if(any(totalConf < .99) | (any(totalConf > 1.01)))
      {
        cat(" *do not add to one*\n")
        print(summary(totalConf))
      }
    if(any(totalConf== 0)) cat("some have all probs == 0\n")    
    cat("\n")

    rm(fit, expected, expConf, pred, predConf, maxConf, totalConf)
    unlink(c("TestData.rules", "TestData.tree", "TestData.tmp"))
  }
combos[combos$rules & combos$trials == 1, "prob"] <- -1


