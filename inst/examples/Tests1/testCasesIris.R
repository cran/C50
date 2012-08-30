
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
    call <- paste(base, "/c5.0 -f irisTestCase ", opt, sep = "")
    call
  }

makePredictOptions <- function(x , c50predict) {
  opt <- "-f irisTestCase"
  opt <- ifelse(x$rules,  paste(opt, "-r"), opt)
  call <-paste(c50predict , opt)
  call
}


makeControl <- function(x)
  {
    C5.0Control(seed = 1, 
                winnow = x$winnow, subset = x$subset,
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
        ifelse(x$fuzzyThreshold, "fuzzy thresholds, ", ""),
        ifelse(x$noGlobalPruning, "no global pruning, ", ""),
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


library(C50)
data(churn)

## path to command line version
c50Path    <- "/Users/yorick/C50_original"
c50Predict <- "/Users/yorick/C50_original/tmp1/sample"

c50Path    <- "/Users/kuhna03/Code/C50clean"
c50Predict <- "/Users/kuhna03/Code/C5predict/sample"
testPath   <- "/Users/kuhna03/Code/rulebasedmodels/pkg/C5/inst/examples/Tests1"

setwd(testPath)

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


for(i in 1:nrow(combos))
  {
    makeHeader(combos[i,], i)
    cat(rep("-", 60), "\n", sep = "")
    cat("   ", makeOptions(combos[i,], c50Path), "\n")
    expected <- system(makeOptions(combos[i,], c50Path), intern = TRUE)
    fit <- C5.0(iris[,-5], iris$Species,
                trials = combos[i,"trials"],
                rules = combos[i,"rules"],
                control = makeControl(combos[i,]))
    outputs[[i]] <- list(expected = expected, observed = strsplit(fit$output, "\n")[[1]])

    obs <- trimOutput(fit$output)
    expected <- trimOutput(expected, split = FALSE)
    
    results <- all.equal(obs, expected)

    cat("   ", makePredictOptions(combos[i,], c50Predict), "\n")

    expected <- system(makePredictOptions(combos[i,], c50Predict), intern = TRUE)
    expected <- parsePredictions(expected, lvl = levels(iris$Species))
    expConf <- expected$conf

    pred <- predict.C5.0(fit, iris[,-5])

    cat("   Classes:")
    print(all.equal(expected$pred, pred))
    if(is.logical(all.equal(expected$pred, pred))) combos$class[i] <- ifelse(all.equal(expected$pred, pred), 1, -1)
    if(!is.logical(all.equal(expected$pred, pred))) combos$class[i] <- 0
    
    predConf <- predict.C5.0(fit, iris[,-5], type = "prob")
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
    unlink(c("irisTestCase.rules", "irisTestCase.tree", "irisTestCase.tmp"))
  }
combos[combos$rules & combos$trials == 1, "prob"] <- -1


