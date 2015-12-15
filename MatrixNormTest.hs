import Help

a = [[1,3,4],[5,6,7]]
b = [[1,2,3],[5,6,-7]]
c = [[-1,-2,-3],[-5,-6,-7]]
d = [[1.3,1.2,1.1],[1.2,1.3,1.5]]

assert a b name = if a == b then  "Test " ++ name ++ " passed" 
                            else  "Test"  ++ name ++ " failed"

main = do 
   print (assert (matrixNorm a b::Float) (14)   "Negative numer test")
   print (assert (matrixNorm a a::Float) (0)    "Same lists test")
   print (assert (matrixNorm c b::Float) (12)   "Negative arr test")
   print (assert (matrixNorm a d::Float) (5.5)  "Float arr test")