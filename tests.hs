import Bindown
import Control.Monad ( liftM )
import Data.List ( intersperse )
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Property
import Test.QuickCheck.Test

-- Lump :: String -> Volume -> Weight -> Lump

-- not-so-interesting generator, to get started.
lumps :: Gen Lump
lumps = do
  let id = "1"
  let vol = 2
  let w = 3
  return $ Lump id vol w 

-- not-so-interesting property, to get started
prop_lumps_have_ids :: Property
prop_lumps_have_ids = 
  forAll lumps $ \l ->
    length (Bindown.id l) > 0


