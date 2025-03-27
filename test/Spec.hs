{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}

import qualified Data.Maybe.Unboxed as U
import GHC.Exts
    ( unsafeCoerce#, Int(I#), (+#), sizeofArray#, unpackClosure# )
import Foreign ( Storable(sizeOf) )

data Test = Test {
    x1 :: !(U.Maybe Int)
}

data TestUnboxed = TestUnboxed {
    x2 :: {-# UNPACK #-} !(U.Maybe Int)
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
    putStrLn $ "Test        { U.Nothing } -> " <> show (unsafeSizeof (Test U.Nothing))
    putStrLn $ "Test        { U.Just }    -> " <> show (unsafeSizeof (Test (U.Just 42)))
    putStrLn $ "TestUnboxed { U.Nothing } -> " <> show (unsafeSizeof (TestUnboxed U.Nothing))
    putStrLn $ "TestUnboxed { U.Just }    -> " <> show (unsafeSizeof (TestUnboxed (U.Just 42)))
