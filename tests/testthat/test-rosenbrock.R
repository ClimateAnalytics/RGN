test_that('rosenbrock', {

  simFunc_rosenbrock=function(x){
    r=rep(0,2)
    r[1] = 1.0-x[1]
    r[2]=10.0*(x[2]-x[1]**2)
    return(r)
  }

  tmp = rgn(simFunc=simFunc_rosenbrock,
            x0=c(-1.0,  0.0), xLo=c(-1.5, -1.0), xHi=c( 1.5,  3.0),
            simTarget=c(0,0),
            cnvSettings=list(dump=0))
  error=tmp$error;message=tmp$message;x=tmp$x;info=tmp$info

  # expected output based on F90 RGN Rosenbrock example
  expect_equal(x,c(1,1))
  expect_equal(info$f,0)
  expect_equal(info$nEval,116)
  expect_equal(info$nIter,16)
  expect_equal(info$termFlag,2)

})


