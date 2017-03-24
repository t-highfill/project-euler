
count len = sum [helper (len-1) [False | _<-[0..9]] i | i<-[1..9]] where
  helper 0 has _ = if all id has then 1 else 0
  helper n has last = sum [helper (n-1) [h || i==nxt | (h,i)<-zip has [0..9]] nxt | nxt<-[last-1,last+1], nxt>=0, nxt<=9]

main = print $ count 40
