
# mlrMBO_mP

Usage:

# start from ps and constrain power to one value
cs <- modifyParam(ps, id="power", lower = 444, upper = 444)
prop = suppressWarnings({proposePoints(opt.state, cs)})

# start from cs and constrain gas to Air
cs2 <- modifyParam(ps, cs, id="gas", lower = "Air")
prop = suppressWarnings({proposePoints(opt.state, cs2)})

# start from ps again and constrain pressure to a range
cs <- modifyParam(ps, id="pressure", lower = 55, upper = 88)
prop = suppressWarnings({proposePoints(opt.state, cs)})

