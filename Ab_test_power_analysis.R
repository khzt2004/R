# https://geoffruddock.com/run-ab-test-with-unequal-sample-size/

library(pwr)

# 50:50 split gives us 93% power
n1 = 25000
n2 = 25000
p1 = 0.15
p2 = 0.16
h = abs(2*asin(sqrt(p1))-2*asin(sqrt(p2)))
pwr.2p2n.test(h, n1=n1, n2=n2, sig.level=0.10)


# 10:90 split gives us 58% power
n1 = 5000
n2 = 45000
pwr.2p2n.test(h, n1=n1, n2=n2, sig.level=0.10)


# 10:10 with 80% hold out gives us 40% power 
n1 = 5000
n2 = 5000
pwr.2p2n.test(h, n1=n1, n2=n2, sig.level=0.10)
