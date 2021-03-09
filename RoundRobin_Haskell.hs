module Main
        where
main = do
  let bt = [20,17,16,14,11,10,5,3,1]
  let wt = [0,0,0,0,0,0,0,0,0]
  let q = 3
  putStrLn "Round Robin Algorithm"
  putStr "Quantum: "
  print q
  putStr "Burst times : "
  print bt
  putStr "Waiting times : "
  print $ roundRobin bt wt q

listOlsun [] =[]
listOlsun x = x

roundRobin [] [] 0 = []
roundRobin bt wt q = do
  let remBt = listOlsun bt
  let t = 0
  dongu1 remBt wt t q (1) bt
-- flag 1

dongu1 remBt wt t q flag bt = do 
  if flag == 1
    then do
          let flag1 = f remBt
          dongu1 (btDegistirme remBt wt t q bt 0) (wtDegistirme remBt wt t q bt 0) (timeDegistirme remBt wt t q bt 0) q flag1 bt
    else wt

timeDegistirme remBt wt t q bt i = do
  if i<9--sayı almayı başarınca (length wt) olacak
    then if remBt !! i > 0
            then do 
                    if remBt !! i > q
                      then timeDegistirme (updateBt remBt i q) wt (t+q) q bt (i+1)
                      else timeDegistirme (updateBt2 remBt i) (updateWt wt bt i t) (t+remBt !! i ) q bt (i+1)
            else timeDegistirme (updateBt2 remBt i) (updateWt wt bt i (t)) (t) q bt (i+1)
    else t 

f list =
  if list !! 0 == 0 && list !! 1 == 0 && list !! 2 == 0 && list !! 3 == 0 && list !! 4 == 0 && list !! 5 == 0 && list !! 6 == 0 && list !! 7 == 0 && list !! 8 == 0
    then 0
    else 1

wtDegistirme remBt wt t q bt i  = do
  if i<9--sayı almayı başarınca (length wt) olacak
    then if remBt !! i > 0
            then do 
                    if remBt !! i > q
                      then wtDegistirme (updateBt remBt i q) wt (t+q) q bt (i+1) 
                      else wtDegistirme (updateBt2 remBt i) (updateWt wt bt i (t+ remBt !! i)) (t+ remBt !! i ) q bt (i+1)
            else wt--Degistirme (updateBt2 remBt i) (updateWt wt bt i t) (t+remBt !! i ) q bt (i+1)
    else wt  

btDegistirme remBt wt t q bt i= do
  if i<9--sayı almayı başarınca (length wt) olacak
    then if remBt !! i > 0
            then do 
                    if remBt !! i > q
                      then btDegistirme (updateBt remBt i q) wt (t+q) q bt (i+1)
                      else btDegistirme (updateBt2 remBt i) (updateWt wt bt i t) (t+remBt !! i ) q bt (i+1)
            else btDegistirme (updateBt2 remBt i) (updateWt wt bt i t) (t + remBt !! i) q bt (i+1)
    else remBt 

flagDegistirme remBt wt t q flag bt i= do
  if i<9--sayı almayı başarınca (length wt) olacak
    then if remBt !! i > 0
            then do 
                    let flag = 0
                    if remBt !! i > q
                      then flagDegistirme (updateBt remBt i q) wt (t+q) q flag bt (i+1)
                      else flagDegistirme (updateBt2 remBt i) (updateWt wt bt i t) (t+remBt !! i ) q flag bt (i+1)
            else flagDegistirme (updateBt2 remBt i) (updateWt wt bt i t) (t+remBt !! i ) q flag bt (i+1)
    else flag

--girilen indis 0 olur
updateBt2 remBt index  = do
      let a0 = remBt !! 0
      let a1 = remBt !! 1
      let a2 = remBt !! 2
      let a3 = remBt !! 3
      let a4 = remBt !! 4
      let a5 = remBt !! 5
      let a6 = remBt !! 6
      let a7 = remBt !! 7
      let a8 = remBt !! 8
      f10 a0 a1 a2 a3 a4 a5 a6 a7 a8 index 
f10 a0 a1 a2 a3 a4 a5 a6 a7 a8 i = 
  if i==0
        then [0,a1,a2,a3,a4,a5,a6,a7,a8]
        else if i==1
                then [a0,0,a2,a3,a4,a5,a6,a7,a8]
                else if i==2
                        then [a0,a1,0,a3,a4,a5,a6,a7,a8]
                        else if i==3
                                then [a0,a1,a2,0,a4,a5,a6,a7,a8]
                                else if i==4
                                        then [a0,a1,a2,a3,0,a5,a6,a7,a8]
                                        else if i==5
                                                then [a0,a1,a2,a3,a4,0,a6,a7,a8]
                                                else if i==6 
                                                        then [a0,a1,a2,a3,a4,a5,0,a7,a8]
                                                        else if i==7
                                                                then [a0,a1,a2,a3,a4,a5,a6,0,a8]
                                                                else if i==8
                                                                        then [a0,a1,a2,a3,a4,a5,a6,a7,0]
        else [a0,a1,a2,a3,a4,a5,a6,a7,a8]
-- index girilen dizinin değerini azaltıp gönderen fonk rem_bt[i] -= quantum;
updateBt remBt index q = do
      let a0 = remBt !! 0
      let a1 = remBt !! 1
      let a2 = remBt !! 2
      let a3 = remBt !! 3
      let a4 = remBt !! 4
      let a5 = remBt !! 5
      let a6 = remBt !! 6
      let a7 = remBt !! 7
      let a8 = remBt !! 8
      f11 a0 a1 a2 a3 a4 a5 a6 a7 a8 index q 
--f11::Int->Int->Int->Int->Int->Int->Int->Int->Int->Int->Int->[Int]
f11 a0 a1 a2 a3 a4 a5 a6 a7 a8 i q =
      if i==0
        then [(a0-q),a1,a2,a3,a4,a5,a6,a7,a8]
        else if i==1
                then [a0,(a1-q),a2,a3,a4,a5,a6,a7,a8]
                else if i==2
                        then [a0,a1,(a2-q),a3,a4,a5,a6,a7,a8]
                        else if i==3
                                then [a0,a1,a2,(a3-q),a4,a5,a6,a7,a8]
                                else if i==4
                                        then [a0,a1,a2,a3,(a4-q),a5,a6,a7,a8]
                                        else if i==5
                                                then [a0,a1,a2,a3,a4,(a5-q),a6,a7,a8]
                                                else if i==6 
                                                        then [a0,a1,a2,a3,a4,a5,(a6-q),a7,a8]
                                                        else if i==7
                                                                then [a0,a1,a2,a3,a4,a5,a6,(a7-q),a8]
                                                                else if i==8
                                                                        then [a0,a1,a2,a3,a4,a5,a6,a7,(a8-q)]
        else [a0,a1,a2,a3,a4,a5,a6,a7,a8]
 --wt[i] = t - bt[i];       
updateWt wt bt index time = do
      let a0 = wt !! 0
      let a1 = wt !! 1
      let a2 = wt !! 2
      let a3 = wt !! 3
      let a4 = wt !! 4
      let a5 = wt !! 5
      let a6 = wt !! 6
      let a7 = wt !! 7
      let a8 = wt !! 8
      f12 a0 a1 a2 a3 a4 a5 a6 a7 a8 bt index time 

f12 a0 a1 a2 a3 a4 a5 a6 a7 a8 remBt i t =
      if i==0
        then [(t-remBt !! 0),a1,a2,a3,a4,a5,a6,a7,a8]
        else if i==1
                then [a0,(t-remBt !! 1),a2,a3,a4,a5,a6,a7,a8]
                else if i==2
                        then [a0,a1,(t-remBt !! 2),a3,a4,a5,a6,a7,a8]
                        else if i==3
                                then [a0,a1,a2,(t-remBt !! 3),a4,a5,a6,a7,a8]
                                else if i==4
                                        then [a0,a1,a2,a3,(t-remBt !! 4),a5,a6,a7,a8]
                                        else if i==5
                                                then [a0,a1,a2,a3,a4,(t-remBt !! 5),a6,a7,a8]
                                                else if i==6 
                                                        then [a0,a1,a2,a3,a4,a5,(t-remBt !! 6),a7,a8]
                                                        else if i==7
                                                                then [a0,a1,a2,a3,a4,a5,a6,(t-remBt !! 7),a8]
                                                                else if i==8
                                                                        then [a0,a1,a2,a3,a4,a5,a6,a7,(t-remBt !! 8)]
        else [a0,a1,a2,a3,a4,a5,a6,a7,a8]