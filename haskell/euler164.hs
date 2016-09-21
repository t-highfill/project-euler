
maxlen :: Integer
maxlen = 20

numbers len [] = sum [numbers (len+1) [i] | i<-[1..9]]
numbers len [x] = sum [numbers (len+1) [x,i] | i<-[0..9], i+x <= 9]
numbers len (x:xs) = if len == maxlen then 1
                     else sum [numbers (len+1) (xs++[i])| i<-[0..9],
                               x+i+(sum xs) <= 9]

result = numbers 0 []

main = print result
