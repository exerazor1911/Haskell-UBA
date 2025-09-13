module TripletUtils(
    getFirstElement,
    getSecondElement,
    getThirdElement
) where
--Helper func
getFirstElement :: (Integer, Integer, Integer) -> Integer
getFirstElement (x,_,_) = x

getSecondElement :: (Integer, Integer, Integer) -> Integer
getSecondElement (_,x,_) = x

getThirdElement :: (Integer, Integer, Integer) -> Integer
getThirdElement (_,_,x) = x
