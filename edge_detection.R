edge_detection = function(X, k)
{

  ## 'pads' zeroes to n-by-dim(pad.X)[2] matrix
  pad.X = matrix(0, dim(X)[1]+2*k, dim(X)[2]+2*k)
  pad.X[(k+1):(dim(X)[1]+k), (k+1):(dim(X)[2]+k)] = X

  ## assign a temporary pad.X matrix
  temp_pad = pad.X

  ## for-loop to conduct mean_filter
  # loops through rows starting k+1 and ending dim(pad.X)[1]-k
  for (i in (k+1):(dim(pad.X)[1] - k))
  {
    # loops through columns starting k+1 and ending dim(pad.X)[1]-k
    for (j in (k+1):(dim(pad.X)[2] - k))
    {
      # initializing sum
      val = c()
      # loops through values -k, ..., 0, ..., k
      for (x in -k:k)
      {
        # loops through values -k, ..., 0, ..., k
        for (y in -k:k)
        {
          # sums all adjacent values from pivot [i,j]
          val = append(val, temp_pad[i+x, j+y])
        }
      }
      # averages all values from sum [much faster than mean()]
      pad.X[i,j] = sd(val)
    }
  }

  ## removes 'pad' from dim(pad.X)[1]-by-dim(pad.X)[2] matrix
  sd.X = pad.X[-c(1:k, dim(pad.X)[1]:(dim(pad.X)[1]-k+1)), ][, -c(1:k, dim(pad.X)[2]:(dim(pad.X)[2]-k+1))]

  # Set a threshold for edge detection based on the top 10% of standard deviation values
  threshold <- quantile(sd.X, 0.9)

  # Set index with sd > threshold = 1, and rest = 0
  for (i in 1:nrow(sd.X))
  {
    for (j in 1:ncol(sd.X))
    {
      if (sd.X[i,j] >= threshold)
      {
        sd.X[i,j] = 1
      }
      else
      {
          sd.X[i,j] = 0
        }
    }
  }

  return(sd.X)
}
