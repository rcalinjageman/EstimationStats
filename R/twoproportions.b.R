
# This file is a generated template, your changes will not be overwritten

twoproportionsClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "twoproportionsClass",
    inherit = twoproportionsBase,
    private = list(
        .run = function() {



          results <- estimate.diffproportions.rawdata(pdata = self$data,
                  conditioncolumn = self$options$group,
                  resultcolumn = self$options$dep, 
                  eventdefined = self$options$eventcode,
                  conf = self$options$ciWidth/100
          )

          notetext = paste("Condidence level = ", self$options$ciWidth, "%")
          self$results$text$setContent(notetext)
          
          table = self$results$propd
          table$setRow(rowNo=1, values = list(
            Group = results[["name1"]],
            r_n = paste(results[["r1"]], "/", results[["n1"]]),
            P = results[["p1"]],
            CI_low = results[["p1_low"]],
            CI_high = results[["p1_high"]]
            )
          )
          table$setRow(rowNo=2, values = list(
            Group = results[["name2"]],
            r_n = paste(results[["r2"]], "/", results[["n2"]]),
            P = results[["p2"]],
            CI_low = results[["p2_low"]],
            CI_high = results[["p2_high"]]
          )
          )
          table$setRow(rowNo=3, values = list(
            Group = results[["name3"]],
            P = results[["pdiff"]],
            CI_low = results[["plow"]],
            CI_high = results[["phigh"]]
          )
          )
          
          image <- self$results$plot
          image$setState(results)

          
                    
        },
        
        .plot = function(image, ...) {
          results = image$state
          plot = estimate.diffproportions.plot(p1 = results[["p1"]], p1_low = results[["p1_low"]], p1_high = results[["p1_high"]],
                                        p2 = results[["p2"]], p2_low = results[["p2_low"]], p2_high = results[["p2_high"]],
                                        pdiff = results[["pdiff"]], plow = results[["plow"]], phigh = results[["phigh"]],
                                        measure= results$measure,
                                        groupnames = c(results[["name1"]], results[["name2"]], results[["name3"]])
                                        ) 
                   
          print(plot)
          TRUE
        })
)




estimate.oneproportion = function(r, n, conf=0.95) {
  p = r/n
  q = 1-p
  z = qnorm(1-(1-conf)/2)
  
  A = 2*r+z^2
  B = z*sqrt(z^2+(4*r*q))
  C = 2*(n+z^2)
  
  p_low = (A-B) / C
  p_high = (A+B) / C
  
  res = c(P = p, CI_low = p_low, CI_high = p_high)
  res
}

estimate.diffproportions.plot = function(p1, p1_low, p1_high, p2, p2_low, p2_high, pdiff, plow, phigh, measure, groupnames) {
  points = c(p1, p2, p1)
  low = c(p1_low, p2_low, p1-(pdiff-plow))
  high = c(p1_high, p2_high, p1+(phigh-pdiff))
  
  ylow = min(c(p1_low, p2_low, p1-(pdiff-plow), 0) )
  yhigh = max(c(p1_high, p2_high, p1+(phigh-pdiff), 1))
  
  
  res = plot(c(1,2,4), points, pch=c(19,19,17), xaxt="n", xlim=c(0.4, 0.4+4), ylim=c(ylow, yhigh), bty = "l", ylab=measure)
  arrows(c(1,2,4), high, c(1,2,4), low, angle=90,code=3,length = 0.0)   
  arrows(1,points[1], 4.4, points[1], angle=90, code=3, length=0, lty=2)
  arrows(2,points[2], 4.4, points[2], angle=90, code=3, length=0, lty=2)
  
  if (points[1] >= points[2]) {
    val <- axTicks(4) - axTicks(4)[min(which(axTicks(4) > median(axTicks(4))))]
    loc <- axTicks(4) - (axTicks(4)[min(which(axTicks(4) > median(axTicks(4))))] - points[2])
  }
  if (points[1] < points[2]) {
    val <- axTicks(4) - axTicks(4)[max(which(axTicks(4) < median(axTicks(4))))]
    loc <- axTicks(4) - (axTicks(4)[max(which(axTicks(4) < median(axTicks(4))))] - points[2])
  }
  
  axis(4, at=loc, labels=val)
  axis(1, at = c(1,2,4), labels= groupnames)
  res
}


estimate.diffproportions = function(name1 = "Group 1", r1, n1, name2 = "Group 2", r2, n2, measure = "Proportion", conf = 0.95, plot=TRUE, main, eventdefined = "events") {
  
  
  p1 = r1/n1
  p2 = r2/n2
  q1 = 1-p1
  q2 = 1-p2
  z = 1.96
  
  p1_res = estimate.oneproportion(r1, n1, conf)
  p2_res = estimate.oneproportion(r2, n2, conf)
  
  p1_low = p1_res[2]
  p1_high = p1_res[3]
  
  p2_low = p2_res[2]
  p2_high = p2_res[3]
  
  
  pdiff = p1 - p2
  
  plow = pdiff - sqrt( (p1-p1_low)^2 + (p2_high - p2)^2  )
  phigh = pdiff + sqrt( (p2 - p2_low)^2 + (p1_high - p1)^2 )
  
  label = paste(name2, " - ", name1)
  
  if(plot) {
    points = c(p1, p2, p1)
    low = c(p1_low, p2_low, p1-(pdiff-plow))
    high = c(p1_high, p2_high, p1+(phigh-pdiff))
    
    ylow = min(c(p1_low, p2_low, p1-(pdiff-plow), 0) )
    yhigh = max(c(p1_high, p2_high, p1+(phigh-pdiff), 1))
    
    
    plot(c(1,2,4), points, pch=c(19,19,17), xaxt="n", xlim=c(0.4, 0.4+4), ylim=c(ylow, yhigh), bty = "l", ylab=measure, main=main)
    arrows(c(1,2,4), high, c(1,2,4), low, angle=90,code=3,length = 0.0)   
    arrows(1,points[1], 4.4, points[1], angle=90, code=3, length=0, lty=2)
    arrows(2,points[2], 4.4, points[2], angle=90, code=3, length=0, lty=2)
    
    if (points[1] >= points[2]) {
      val <- axTicks(4) - axTicks(4)[min(which(axTicks(4) > median(axTicks(4))))]
      loc <- axTicks(4) - (axTicks(4)[min(which(axTicks(4) > median(axTicks(4))))] - points[2])
    }
    if (points[1] < points[2]) {
      val <- axTicks(4) - axTicks(4)[max(which(axTicks(4) < median(axTicks(4))))]
      loc <- axTicks(4) - (axTicks(4)[max(which(axTicks(4) < median(axTicks(4))))] - points[2])
    }
    
    axis(4, at=loc, labels=val)
    axis(1, at = c(1,2,4), labels= c("High Power", name1, "Difference"))
    
    
  }
  
  
  res = list(
    name1 = name1,
    r1 = r1,
    n1 = n1,
    p1 = p1_res[["P"]],
    p1_low = p1_res[["CI_low"]],
    p1_high = p1_res[["CI_high"]],
    name2 = name2,
    r2 = r2,
    n2 = n2,
    p2 = p2_res[["P"]],
    p2_low = p2_res[["CI_low"]],
    p2_high = p2_res[["CI_high"]],
    name3 = "Difference",
    pdiff = pdiff,
    plow = plow,
    phigh = phigh,
    conf = conf,
    measure = paste("Proportion ", eventdefined, " for ", measure, " with ", conf*100, "% CI")
  )

  res
}


estimate.diffproportions.rawdata = function(pdata, conditioncolumn, resultcolumn, eventdefined, conf=.95, plot=FALSE, main="") {
  
  groups = 1
  n = 0
  r = 0  
  names = 0
  
  eventdefined = levels(pdata[[resultcolumn]])[eventdefined]
  

  for (gname in levels(pdata[[conditioncolumn]])) {
    gdata = pdata[pdata[[conditioncolumn]] == gname, ]
    names[groups] = gname
    n[groups] = nrow(gdata)
    r[groups] = nrow(gdata[gdata[[resultcolumn]] == eventdefined, ])
    
    print(paste(gname, " r = ", r[groups], " of N = ", n[groups], " for a proportion of", r[groups]/n[groups]))
    groups = groups + 1
  }
  

  if(groups == 3) {
    res = estimate.diffproportions(name1 = names[1], r1=r[1], n1=n[1], name2=names[2], r2=r[2], n2=n[2], measure=resultcolumn, conf=conf, plot=plot, main=main, eventdefined = eventdefined)
  } else {
    res = c(n, r, "Too many groups")
  }
  
  
  res
  
}



