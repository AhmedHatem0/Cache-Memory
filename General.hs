--GeneralMethods
data Item a = It Tag (Data a) Bool Int | NotPresent deriving (Show, Eq)

data Tag = T Int deriving (Show, Eq)

data Data a = D a deriving (Show, Eq)

--

convertBinToDec :: Integral i => i -> i

convertBinToDec 0 = 0
convertBinToDec i = 2 * convertBinToDec (div i 10) + (mod i 10)


replaceIthItem :: t -> [t] -> Int -> [t]
 
replaceIthItem _ [] _ = [] 
replaceIthItem t (x:xs) 0 = t:xs
replaceIthItem t (x:xs) n = if n < 0 then (x:xs) else x : (replaceIthItem t xs (n-1)) 


splitEvery :: Int -> [a] -> [[a]]

splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest) where (first,rest) = splitAt n list


logBase2 x = logBase 2 x

fillZeros ::  [Char] -> Int -> [Char]

fillZeros [] n = [] 
fillZeros l 0 = l 
fillZeros l n | n>0 = concat [['0'],(fillZeros l (n-1))]




getNumBits sets type1 cache 
							| type1 == "fullyAssoc" = 0
							| type1 == "setAssoc" = ceiling (logBase2 sets)
							| otherwise = ceiling (logBase2 (sets))
							
--- Fully Associative 
data Output a = Out (a, Int) | NoOutput deriving (Show, Eq)

getDataFromCache stringAddress (h:t) "fullyAssoc" bitsNum
	= getDataFromCacheHelper stringAddress (h:t) "fullyAssoc" bitsNum 0			
				

getDataFromCacheHelper _ [] "fullyAssoc" _ _ = NoOutput				
getDataFromCacheHelper stringAddress ((It (T t) (D d) valid order):r) "fullyAssoc" bitsNum hopsNum			
				|(valid && (read stringAddress) == t) = Out (d, hopsNum)
				|otherwise = getDataFromCacheHelper stringAddress r "fullyAssoc" bitsNum (hopsNum+1)


convertAddress binAddress bitsNum "fullyAssoc" = (binAddress,0)


replaceInCache tag idx memory oldCache "fullyAssoc" bitsNum = (data1, (getNewCache tag data1 (incValids oldCache)))
	where	data1 = (memory !! (convertBinToDec tag))

			
getNewCache tag data1 oldCache = if(hasFalse oldCache) then (getNewCacheF tag data1 oldCache) else (getNewCacheT tag data1 oldCache)
			
getNewCacheF tag data1 ((It _ (D d) False x):rest) = ((It (T tag) (D data1) True 0):rest)
getNewCacheF tag data1 (h:rest) = (h: (getNewCacheF tag data1 rest))


getNewCacheT tag data1 ((It y (D d) True x):rest) = if(x == getMaxOrder (((It y (D d) True x):rest))) then ((It (T tag) (D data1) True 0):rest) else ((It y (D d) True x): getNewCacheT tag data1 rest) 
	
getMaxOrder [] = -1
getMaxOrder ((It _ _ _ x):t) = if(x> (getMaxOrder t)) then x else getMaxOrder t

incValids [] = []
incValids ((It y (D d) True x):t) = ((It y (D d) True (x+1)):(incValids t))
incValids (h:t) = (h:(incValids t))

hasFalse [] = False
hasFalse ((It _ (D d) False x):rest) = True
hasFalse (h:rest) = hasFalse rest

getData stringAddress cache memory cacheType bitsNum
	| x == NoOutput = replaceInCache tag index memory cache cacheType bitsNum
	| otherwise = (getX x, cache)
	where
	x = getDataFromCache stringAddress cache cacheType bitsNum
	address = read stringAddress:: Int
	(tag, index) = convertAddress address bitsNum cacheType
	getX (Out (d, _)) = d


---runProgram :: (Eq t, Floating a, RealFrac a) => [String] -> [Item t] -> [t] -> [Char] -> a -> ([t], [Item t])
runProgram [] cache _ _ _ = ([], cache)
runProgram (addr: xs) cache memory cacheType numSets =
	((d:prevData), finalCache)
	where
	bitsNum = round(logBase2 numSets)
	(d, updatedCache) = getData addr cache memory cacheType bitsNum
	(prevData, finalCache) = runProgram xs updatedCache memory cacheType numSets
