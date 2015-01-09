import Prelude hiding (Maybe (..))

data Maybe a= Nothing| Just a
      deriving (Show)

instance (Eq m)=> Eq (Maybe m) where
  Just x==Just y= x==y
  Nothing== Nothing= True
  _==_=False

test1= (Just 5) == (Just 5)
test2= (Just 5) == (Just 10)

-- -----------------------------------------------------------------------------
data TrafficLight=Red|Yellow|Green
      deriving(Eq,Show,Ord)

test3= Red<Yellow
test4= Red==Yellow
-- -----------------------------------------------------------------------------------------------
--Functor for Maybe
class MapFunctor f where
  mapFunctor::(a->b)->f a->f b

instance MapFunctor Maybe where
  mapFunctor f (Just m)=Just(f m)
  mapFunctor f Nothing=Nothing

testMapFunctor=mapFunctor (+5) (Just 6)