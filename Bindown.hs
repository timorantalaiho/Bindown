module Bindown where

type Volume = Integer
type Weight = Integer

data Range t = Range {
    start :: t
  , end   :: t
  } deriving (Show, Eq, Ord)


data Lump = Lump {
    id     :: String
  , volume :: Volume
  , weight :: Weight
  } deriving (Show, Eq)

data Container = Container {
    volumeLimits          :: Range Volume
  , weightLimits          :: Range Weight
  , contents              :: [Lump]
  , name                  :: String
  } deriving (Show, Eq)



total :: (Lump -> Integer) -> Integer -> [Lump] -> Integer
total f z ls = foldl step z ls
     where step acc l = acc + f l

totalVolume :: [Lump] -> Volume
totalVolume = total volume 0 

totalWeight :: [Lump] -> Weight
totalWeight = total weight 0 

containerVolume :: Container -> Volume
containerVolume = totalVolume . contents

containerWeight :: Container -> Weight
containerWeight = totalWeight . contents

maxVolume :: Container -> Volume
maxVolume = end . volumeLimits

maxWeight :: Container -> Weight
maxWeight = end . weightLimits

freeCapacity :: Container -> (Volume, Weight)
freeCapacity c = ( ((maxVolume c) - containerVolume c), 
                   ((maxWeight c) - containerWeight c)
                 )

fitsIn :: Volume -> Weight -> Container -> Bool
fitsIn v w c = let (freeVol, freeWeight) = freeCapacity c                       
               in freeVol >= v && freeWeight >= w                               


lumpFitsIn :: Container -> Lump -> Bool
lumpFitsIn c l = fitsIn (volume l) (weight l) c

isFullEnough :: Container -> Bool
isFullEnough c = (((containerVolume c >= start (volumeLimits c)) && 
                   (containerWeight c >= start (weightLimits c))))

isSmallEnough :: Container -> Bool
isSmallEnough c = (((containerVolume c <= end (volumeLimits c)) && 
                   (containerWeight c <= end (weightLimits c))))

isWithinLimits :: Container -> Bool
isWithinLimits c = isFullEnough c && isSmallEnough c

add :: Container -> Lump -> Container
add c l = Container (volumeLimits c) (weightLimits c) (l : contents c) (name c)

addAll :: Container -> [Lump] -> Container
addAll c ls = Container (volumeLimits c) (weightLimits c) (ls ++ contents c) (name c)

-- The two container types that we want to use
bin :: [Lump] -> Container
bin ls = Container (Range 450 900) (Range 160 210) ls "bin"

bowl :: [Lump] -> Container
bowl ls = Container (Range 500 800) (Range 720 900) ls "bowl"

createOneEmpty :: ([Lump] -> Container) -> Container
createOneEmpty x = x []

createEmpty :: Int -> ([Lump] -> Container) -> [Container]
createEmpty n cc = take n $ repeat $ createOneEmpty cc


-- A solution is a function that takes in the list of available Containers and
-- the list of Lumps to allocate and returns the list of the produced
-- Containers and the possible overflow Lumps.

type Solution = ([Container], [Lump]) -> ([Container], [Lump])

-- Everything is overflow here 
dummyPacking :: Solution
dummyPacking (cs, ls) = ([bin []], ls)

-- This is a stupid algorithm that can only fill the first container
greedyPacking :: Solution
greedyPacking (cs, []) = (cs, [])
greedyPacking (cs, ls) = let firstLump = head ls
                             firstContainer = head cs
			 in if not $ isFullEnough (addAll firstContainer ls)
                         then (cs, ls)
                         else if (lumpFitsIn (firstContainer) firstLump)
			 then greedyPacking ( (add firstContainer firstLump) : (tail cs), tail ls)
			 else (cs, ls)


-- Some test data
bin1 = bin [(Lump "l1" 10 29), (Lump "l2" 20 30)]
bin2 = bin [(Lump "l3" 44 55), (Lump "l4" 77 66)]

bowl1 = bowl [(Lump "l5" 90 99), (Lump "l6" 90 90)]
bowl2 = bowl [(Lump "l7" 94 95), (Lump "l8" 97 96)]

hugeLump = Lump "hugeLump" 9999 9999
smallLump = Lump "smallLump" 1 1
thousandSmallLumps = take 1000 (repeat smallLump)

-- Poor man's "unit tests" :) 
w1 = "container weight on bin1: 59 == " ++ show (containerWeight bin1)
v1 = "container volume on bowl2: 191 == " ++ show (containerVolume bowl2)

v2 = "addition on bowl1: 680 == " ++ show (containerVolume (add (bowl1) (Lump "newLump" 500 600)))

f1 = "does huge lump fit to bin1: False == " ++ show (lumpFitsIn bin1 (hugeLump))
f2 = "does small lump fit to bin1: True == " ++ show (lumpFitsIn bin1 (smallLump))

c1 = "creating new bins with enum: 97 == " ++ show (length (createEmpty 97 bin))

of1 = "Everything stays in overflow when trying to pack hugeLump: 3 == " ++ (show $ length $snd (greedyPacking (createEmpty 10 bin, (hugeLump : contents bin1))))
of2 = "Everything stays in overflow if minimum is not satisified: 2 == " ++ show (length (snd (greedyPacking (createEmpty 10 bin, (contents bin1)))))
of3 = "Individual 1-sized lumps fit to bin: 1000-210 = 790 == " ++ show (length (snd (greedyPacking (createEmpty 10 bin, thousandSmallLumps))))
of4 = "Individual 1-sized lumps fit to bowl: 1000-800 = 200 == " ++ show (length (snd (greedyPacking (createEmpty 10 bowl, thousandSmallLumps))))

testOutput = foldl step "" [w1, v1, v2, f1, f2, c1, of1, of2, of3, of4]
  where step acc s = acc ++ "\n" ++ s ++ "\n"

myTests = putStr testOutput
