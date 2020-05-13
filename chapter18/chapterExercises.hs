-- stack --resolver lts-12.18 script --package QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

main :: IO ()
main = do
  quickBatch $ functor $ (NopeDotJpg :: Nope (Int, String, Int))
  quickBatch $ applicative $ (NopeDotJpg :: Nope (Int, String, Int))
  quickBatch $ monad $ (NopeDotJpg :: Nope (Int, String, Int))

  quickBatch $ functor $ (Left "a" :: E String (Int, Int, Int))
  quickBatch $ applicative $ (Left "a" :: E String (Int, Int, Int))
  quickBatch $ monad $ (Left "a" :: E String (Int, Int, Int))

  quickBatch $ functor $ (Identity (1, 2, 3) :: Identity (Int, Int, Int))
  quickBatch $ applicative $ (Identity (1, 2, 3) :: Identity (Int, Int, Int))
  quickBatch $ monad $ (Identity (1, 2, 3) :: Identity (Int, Int, Int))

  quickBatch $ functor $ Cons ('a', 'b', 'c') Nil
  quickBatch $ applicative $ Cons ('a', 'b', 'c') Nil
  quickBatch $ monad $ Cons ('a', 'b', 'c') Nil


data Nope a =
    NopeDotJpg deriving (Eq, Show)

instance Arbitrary (Nope a) where
  arbitrary = pure NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

instance Functor NopeDotJpg where
  fmap _ NopeDotJpg= NopeDotJpg

instance Applicative NopeDotJpg where
  pure NopeDotJpg = const NopeDotJpg
  NopeDotJpg <*> NopeDotJpg = NopeDotJpg

instance Monad NopeDotJpg where
  NopeDotJpg >>= _ = NopeDotJpg

data E b a =
    Left a
  | Right b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary e) => Arbitrary (E e a) where
  arbitrary = oneof [Left <$> arbitrary, Right <$> arbitrary]

instance (Eq a, Eq e) => EqProp (E e a) where
  (=-=) = eq

instance Functor (E a) where
  fmap _ (Left a) = Left a
  fmap f (Right a) = Right (f a)

instance Applicative (E a) where
  pure a = Right a
  (Right f) <*> (Right b) = Right (f b)
  (Left f) <*> _ = Left f
  _ <*> (Left b) = Left b

instance Monad (E a) where
  (Left a) >>= _ = Left a
  (Right a) >>= f = Right (f a)

newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

instance Functor (Identity a) where
  fmap f (Identity a) = Identity (f a)

instance Applicative (Identity a) where
  pure a = Identity a
  (Identity f) <*> (Identity a) = Idenity (f b)

instance Monad (Identity a) where
  return = pure
  (Identity a) >>= f = f a

data List a = Nil | Cons a (List a)
    deriving (Eq, Show)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = oneof [Cons <$> arbitrary <*> arbitrary, return Nil]

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons f x $ (fmap f xs)

instance Applicative List where
  pure  = flip Cons Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f fs) <*> (Cons x xs) = (Cons f x) $ (Cons f xs) $ (Cons fs x) $ (Cons fs xs)

instance Monad List where
  (Nil) >>= _ = Nil
  (Cons x xs) >>= f = f x $ f xs
