require(RUnit)

checkTrue(1 < 2)
# checkTrue(1 == 2)

v <- 1:3
checkEquals(v, 1:3)

checkException(stop("Raise an exception"))
