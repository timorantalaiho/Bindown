
type Volume = Integer
type Weight = Integer

data Range t = Range {
    start :: t
  , end   :: t
  } deriving (Show, Eq, Ord)

class Ord a => Range2 a where
    contains :: Range a -> a -> Bool

instance Range2 Integer where
    contains r x = x >= start r && x <= end r

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

total :: [Lump] -> (Lump -> Integer) -> Integer -> Integer
total ls f z = foldl step z ls
    where step acc l = acc + f l

totalVolume :: [Lump] -> Volume
totalVolume ls = total ls volume 0 

totalWeight :: [Lump] -> Weight
totalWeight ls = total ls weight 0 

containerVolume :: Container -> Volume
containerVolume c = totalVolume (contents c)

containerWeight :: Container -> Weight
containerWeight c = totalWeight (contents c)

maxVolume :: Container -> Volume
maxVolume c = end (volumeLimits c)

maxWeight :: Container -> Weight
maxWeight c = end (weightLimits c)

freeCapacity :: Container -> (Volume, Weight)
freeCapacity c = ( ((maxVolume c) - containerVolume c), 
                   ((maxWeight c) - containerWeight c)
                 )

fitsIn :: Volume -> Weight -> Container -> Bool
fitsIn v w c = let free = freeCapacity c
               in if (fst free >= v && snd free >= w) 
               then True
               else False

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

-- The two container types that we want to use
bin :: [Lump] -> Container
bin ls = Container (Range 450 900) (Range 160 210) ls "bin"

bowl :: [Lump] -> Container
bowl ls = Container (Range 500 800) (Range 720 900) ls "bowl"


-- So I'd like to be able to say that a _solution_ has a type [Lump] -> ([Container], [Lump]) 
-- the idea being that a solution is a function that takes in the list of Lumps and 
-- returns the list of the produced Containers and the possible overflow Lumps.
-- But so far I was only express to express something faintly like this in 
-- this kludge:
class BinDownSolution l where
    solution :: [l] -> ([Container], [l]) 

-- Everything is overflow here 
instance BinDownSolution Lump where
    solution ls = ([bin []], ls)


-- Some test data
bin1 = bin [(Lump "l1" 10 29), (Lump "l2" 20 30)]
bin2 = bin [(Lump "l3" 44 55), (Lump "l4" 77 66)]

bowl1 = bowl [(Lump "l5" 90 99), (Lump "l6" 90 90)]
bowl2 = bowl [(Lump "l7" 94 95), (Lump "l8" 97 96)]

-- Poor man's "unit tests" :) 
w1 = "container weight on bin1: 59 == " ++ show (containerWeight bin1)
v1 = "container volume on bowl2: 191 == " ++ show (containerVolume bowl2)

v2 = "addition on bowl1: 680 == " ++ show (containerVolume (add (bowl1) (Lump "newLump" 500 600)))

testOutput = foldl step "" [w1, v1, v2]
  where step acc s = acc ++ "\n" ++ s ++ "\n"

myTests = putStr testOutput
