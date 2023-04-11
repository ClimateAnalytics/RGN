test_that('hymod', {

  data("BassRiver")

  tmp = rgn(simFunc=simFunc_hymod,
            x0=c(400.,0.5,0.1,0.2,0.1),
            xLo=c(1.,0.1,0.05,0.000001,0.000001),
            xHi=c(1000.,2.,0.95,0.99999,0.99999),
            simTarget=BassRiverData$Runoff.mm.day[365:length(BassRiverData$Date)],
            stateVal=c(100.0,30.0,27.0,25.0,30.0,0.0,0.0,0.0),
            nWarmUp=365,
            rain=BassRiverData$Rain.mm,
            pet=BassRiverData$ET.mm)
  error=tmp$error;message=tmp$message;x=tmp$x;info=tmp$info

  # expected output based on F90 RGN hymod example
  expect_equal(signif(x,digits=7),c(146.7564,0.3635988,0.1895957,0.99999,0.7430698))
  expect_equal(signif(info$f,digits=7),6840.165)
  expect_equal(info$nEval,367)
  expect_equal(info$nIter,29)
  expect_equal(info$termFlag,2)

})
