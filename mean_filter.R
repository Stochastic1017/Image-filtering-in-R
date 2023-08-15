mean_filter = function(X, k)
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
      sum = 0
      # loops through values -k, ..., 0, ..., k
      for (x in -k:k)
      {
        # loops through values -k, ..., 0, ..., k
        for (y in -k:k)
        {
          # sums all adjacent values from pivot [i,j]
          sum = sum + temp_pad[i+x, j+y]
        }
      }
      # averages all values from sum [much faster than mean()]
      pad.X[i,j] = sum/((2*k + 1)*(2*k + 1))
    }
  }

  ## removes 'pad' from dim(pad.X)[1]-by-dim(pad.X)[2] matrix
  mean.X = pad.X[-c(1:k, dim(pad.X)[1]:(dim(pad.X)[1]-k+1)), ][, -c(1:k, dim(pad.X)[2]:(dim(pad.X)[2]-k+1))]

  return(mean.X)
}
