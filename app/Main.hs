{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}

module Main where

import qualified Data.Unpackable.Maybe as Unpack
import GHC.Exts
    ( unsafeCoerce#, Int(I#), (+#), sizeofArray#, unpackClosure# )
import Foreign ( Storable(sizeOf) )

data Test = Test {
    x1 :: {-# UNPACK #-} !(Maybe Int)
}

data TestUnpack = TestUnpack {
    x2 :: {-# UNPACK #-} !(Unpack.Maybe Int)
}


unsafeSizeof :: a -> Int
unsafeSizeof a =
  case unpackClosure# a of
    (# x, ptrs, nptrs #) ->
      sizeOf (undefined::Int) + -- one word for the header
        I# (sizeofArray# (unsafeCoerce# ptrs)
             +# sizeofArray# nptrs)


main :: IO ()
main = do
    putStrLn $ "Test { Nothing }        -> " <> show (unsafeSizeof (Test Nothing))
    putStrLn $ "Test { Just }           -> " <> show (unsafeSizeof (Test (Just 42)))
    putStrLn $ "Test { Unpack.Nothing } -> " <> show (unsafeSizeof (TestUnpack Unpack.Nothing))
    putStrLn $ "Test { Unpack.Just }    -> " <> show (unsafeSizeof (TestUnpack (Unpack.Just 42)))
