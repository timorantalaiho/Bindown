import Bindown
import Control.Monad ( liftM2 )
import Data.List ( intersperse )
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Property
import Test.QuickCheck.Test


makePositive :: Integer -> Integer
makePositive x = abs(x) + 1

instance Arbitrary Lump where
    arbitrary = do 
      id <- arbitrary
      vol <- arbitrary
      w <- arbitrary
      return (Lump ("id " ++ id) (makePositive vol) (makePositive w))

prop_lump_has_id :: Lump -> Bool
prop_lump_has_id l = 
  (length ((Bindown.id) l)) > 0

prop_lump_has_positive_dimensions :: Lump -> Bool
prop_lump_has_positive_dimensions l = 
  ((weight l) > 0) && ((volume l) > 0)

combine_props :: [Lump -> Bool] -> Lump -> Bool
combine_props ps l = and $ map ($ l) ps

prop_lump_is_ok :: Lump -> Bool
prop_lump_is_ok l = combine_props [prop_lump_has_id, prop_lump_has_positive_dimensions] l 


-- Test data
l1 = Lump "1" 91 1
l2 = Lump "" 19 22
l3 = Lump "l3" 19 0

