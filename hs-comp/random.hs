type Seed = Integer

newtype Gen a = Gen {runGen :: Seed -> (a, Seed) }

rand :: Gen Integer
rand = Gen $ \s -> (s, s+1)

mkSeed :: Integer -> Seed
mkSeed x = x

fiveRands :: [Integer]
fiveRands = getRands (mkSeed 1) 5

getRands :: Seed -> Integer -> [Integer]
getRands _ 0 = []
getRands s n =
  let (f, s') = (runGen rand) s
  in (s : getRands s' (n - 1))

-- generalA :: (a -> b) -> Gen a -> Gen b
-- generalA f g = Gen $ \s ->
--   let (v, s') = g s
--   in (f v, s')

-- randLetter :: Gen Char
-- randLetter s = generalA toLetter rand
--
randEven :: Gen Integer
randEven = (*2) <$> rand

instance Functor Gen where
  fmap f g = Gen $ \s ->
    let (v, s') = runGen g s
    in (f v, s')

generalPair:: Gen a -> Gen b -> Gen (a,b)
generalPair ga gb= Gen $ \s ->
  let (a, s') = runGen ga s
      (b, _)  = runGen gb s
  in ((a, b), s')


generalB:: (a -> b-> c) -> Gen a -> Gen b -> Gen c
generalB f ga gb= Gen $ \s ->
  let (a, s') = runGen ga s
      (b, _)  = runGen gb s
  in (f a b, s')

generalPair2:: Gen a -> Gen b -> Gen (a,b)
generalPair2 = generalB (,)

repRandom :: [Gen a] -> Gen [a]
repRandom gas =
  case gas of
    [] -> Gen $ \s -> ([], s)
    (x:xs) -> Gen $ \s ->
      let (a, s')  = runGen x s
          (as, s'') = runGen (repRandom xs) s'
      in (a:as, s'')

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo ga fab = Gen $ \s ->
  let (a, s') = runGen ga s
  in runGen (fab a) s'

mkGen :: a -> Gen a
mkGen a = Gen $ \s -> (a, s)
