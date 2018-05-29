
# This file is a generated template, your changes will not be overwritten


twoIndependentMeansClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "twoIndependentMeansClass",
    inherit = twoIndependentMeansBase,
    private = list(
        .run = function() {

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)


          level1 = levels(self$data[[self$options$group]])[2]
          level2 = levels(self$data[[self$options$group]])[1]
          gnames = c(level2,level1)
          
          results <- estimate.two.means(pdata = self$data, dep = self$options$dep, group = self$options$group, conf = self$options$ciWidth/100, varEq = self$options$varEq, nhst = self$options$nhst)
          
          if(results$error) {
            self$results$text$setContent(results$error_msg) 
            return()
          }
          
          notetext = paste("Confidence level = ", self$options$ciWidth, "%")
          if(self$options$nhst) {
            notetext = paste(notetext, "\n", results$nhst_res)
          }
          notetext = paste(notetext, "\n", replication_sample(results))
          notetext = paste(notetext, "\n", "This module is still in beta; note Cohen's d and its CI are not adjusted for upward bias")
          self$results$text$setContent(notetext)
          
                    
          table = self$results$meantable
          table$setRow(rowNo=1, values = list(
            Group = results$level1,
            M = results$m1,
            CI_low = results$m1_low,
            CI_high = results$m1_high,
            s = results$s1,
            N = results$n1
            )
          )
          table$setRow(rowNo=2, values = list(
            Group = results$level2,
            M = results$m2,
            CI_low = results$m2_low,
            CI_high = results$m2_high,
            s = results$s2,
            N = results$n2
          )
          )
          table$setRow(rowNo=3, values = list(
            Group = "Difference",
            M = results$mdiff,
            CI_low = results$mdiff_low,
            CI_high = results$mdiff_high,
            s = NULL,
            N = NULL
          )
          )
          table$setRow(rowNo=5, values = list(
            Group = "Standardized Difference (Cohen's d)",
            N = NULL,
            M = results$d,
            CI_low = results$d_low,
            CI_high = results$d_high,
            s = NULL
          )
          )
          
          
        },
        .plot = function(image, ...) {
          
          if (is.null(self$options$dep) || is.null(self$options$group))
            return(FALSE)
            
          data  = self$data
          group = data[[self$options$group]]
          dep   = data[[self$options$dep]]
          groupNames = rev(levels(group))
          conf = self$options$ciWidth / 100
          fmla = as.formula(jmvcore::constructFormula(self$options$dep, self$options$group))
            
          multicon::diffPlot(fmla, data, paired=FALSE, conf=conf, grp.names=groupNames, ylab=self$options$dep, xlab='')
          
          TRUE
        })
)

replication_sample = function(result) {
  n_mid = power.t.test(sd = 1, delta=abs(result$d), type = "two.sample", sig.level = 1-result$conf, power=0.90)
  n_low = power.t.test(sd = 1, delta=abs(result$d_low), type = "two.sample", sig.level = 1-result$conf, power=0.90)
  n_high = power.t.test(sd = 1, delta=abs(result$d_high), type = "two.sample", sig.level = 1-result$conf, power=0.90)
  
  res = paste("To replicate this study with 90% power you would need N=", trimws(format(n_mid$n, digits=0)), "/group [", trimws(format(n_low$n,digits=0)), ", ", trimws(n_high$n), "]")
  res
  
}

estimate.two.means = function(pdata, dep, group, conf = .95, varEq = TRUE, nhst = FALSE) {
  error = FALSE
  error_msg = ""
  
  if(is.null(pdata) | is.null(dep) | is.null(group)) {
    error = TRUE
    error_msg = "No data yet"
    return(list(error, error_msg))
  }
  
  keeps = c(dep, group)
  pdata = pdata[keeps]
  pdata = pdata[!is.na(pdata[[dep]]), ]
  pdata = pdata[!is.na(pdata[[group]]), ]
  
  
  if(nrow(pdata) <2) {
    error = TRUE
    error_msg = "Less than 2 complete cases"
    return(list(error, error_msg))
  }

  
  level1 = levels(pdata[[group]])[2]
  level2 = levels(pdata[[group]])[1]
  
  group1 = pdata[pdata[[group]] == level1, ]
  group2 = pdata[pdata[[group]] == level2, ]
  
  if(nrow(group1) < 2) {
    error = TRUE
    error_msg = paste("Less than 2 valid cases in group: ", level1)
    return(list(error, error_msg))
  }
  
  if(nrow(group2) < 2) {
    error = TRUE
    error_msg = paste("Less than 2 valid cases in group: ", level2)
    return(list(error, error_msg))
  }
  
  n1 = nrow(group1)
  n2 = nrow(group2)
  m1 = mean(group1[[dep]])
  m2 = mean(group2[[dep]])
  s1 = sd(group1[[dep]])
  s2 = sd(group2[[dep]])
  
  m1res = t.test(group1[[dep]], conf.level=conf)
  m1ci = m1res$conf.int
  m1_low = m1ci[1]
  m1_high = m1ci[2]
  
  m2res = t.test(group2[[dep]], conf.level=conf)
  m2ci = m2res$conf.int
  m2_low = m2ci[1]
  m2_high = m2ci[2]
  
  
  tres = t.test(group1[[dep]], group2[[dep]], paired = FALSE, conf.level = conf, var.equal = varEq)
  mdiff = m1 - m2
  ci = tres$conf.int
  mdiff_low = ci[1]
  mdiff_high = ci[2]
  nhst_res = paste("t(", trimws(tres$parameter), ") = ", trimws(format(tres$statistic, digits=3)), ", p = ", format.pval(tres$p.value))
  
  
  sdp = (s1*s1*n1) + (s2*s2*n2)
  sdp = sdp/ (n1+n2)
  sdp = sqrt(sdp)
  
  d = 1
  d_low = 0.5
  d_high = 1.5
  
  d = psych::t2d(t = tres$statistic, n2=n2, n1=n1)
  cd = psych::d.ci(d = d, n2=n2, n1=n1, alpha=1-conf)
  d_low = cd[1]
  d_high = cd[3]
  
  res = list(
    error = error,
    error_msg = error_msg,
    level1 = level1,
    level2 = level2,
    m1 = m1,
    m2 = m2,
    n1 = n1,
    n2 = n2,
    s1 = s1,
    s2 = s2,
    m1_low = m1_low,
    m1_high = m1_high,
    m2_low = m2_low,
    m2_high = m2_high,
    mdiff = mdiff,
    mdiff_low = mdiff_low,
    mdiff_high = mdiff_high,
    d = d,
    d_low = d_low,
    d_high = d_high,
    conf = conf,
    nhst_res = nhst_res
  )
  res
}
