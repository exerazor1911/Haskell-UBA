parteEntera :: Float -> Integer
parteEntera 0 = 0
parteEntera n = 1 + parteEntera (n - 1) 