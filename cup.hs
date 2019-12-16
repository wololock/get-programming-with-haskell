
cup flOz = \message -> message flOz

coffeCup = cup 12

getOz aCup = aCup id

drink aCup ozDrank = if ozDiff >= 0
  then cup ozDiff
  else cup 0
  where
    flOz = getOz aCup
    ozDiff = flOz - ozDrank

isEmpty aCup = getOz aCup == 0

afterManySips = foldl drink coffeCup [1,1,1,1,1]
