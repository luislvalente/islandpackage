## quantiles function
quantiles = function(probdist,probs)
{
  result = NULL
  cdf = cumsum(probdist[2,])
  for(i in 1:length(probs))
  {
    n = max(which(cdf <= probs[i]))
    x = probdist[1,n]
    if(cdf[n] == probs[i])
    {c()
      result[i] = x
    } else
      if(n < length(cdf))
      {
        result[i] = ((x + 1) * (probs[i] - cdf[n]) + x * (cdf[n + 1] - probs[i]))/(cdf[n + 1] - cdf[n])
      } else
      {
        result[i] = x
      }
  }
  names(result) = probs
  return(result)
}