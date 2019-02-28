recaSeq :: Int -> [Int]
recaSeq 0 = [0]
recaSeq x = 
 let prevList = recaSeq (x - 1)
     prevTerm = last prevList
  in 
    if (prevTerm - x) > 0 && (notElem (prevTerm-x) prevList) then 
     prevList ++ [prevTerm - x]
    else 
     prevList ++ [prevTerm + x]

recaMan :: Int -> Int
recaMan x = last (recaSeq x)

recaList :: [Int] -> [Int]
recaList xs = map recaMan xs