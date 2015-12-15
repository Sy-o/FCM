import Help

a = [1,2,3,4,3,2,1]
b = [-1,-2,-3,-5,-6,-7,-9]
c = []
d = [1.3,1.2,1.1,2.5,3.4,5.8,4.8]

assert a b name = if a == b then  "Test " ++ name ++ " passed" 
                            else  "Test"  ++ name ++ " failed"

main = do 
    print (assert (euclideanDist a b::Float) (19.974984355438178)   "Negative numer test")
    print (assert (euclideanDist a a::Float) (0)    "Same lists test")
    print (assert (euclideanDist c b::Float) (0)   "Differance length test")
    print (assert (euclideanDist a d::Float) (5.969087032369355)  "Float arr test")