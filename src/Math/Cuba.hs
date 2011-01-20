{-# LANGUAGE ForeignFunctionInterface #-}

module Math.Cuba where

import Foreign
import Foreign.C.Types
import Control.Monad

seed :: Int
seed = 0

type UserData = (Double)
type Integrand = [Double] -> UserData -> [Double]

type C_integrand_t = Ptr CInt -> Ptr CDouble
    -> Ptr CInt -> Ptr CDouble
    -> Ptr UserData 
    -> IO ()

data Result = Result
    { value :: Double
    , error :: Double
    , prob  :: Double
    } deriving (Show)

foreign import ccall "cuba.h Suave"
    c_suave :: CInt -> CInt
        -> FunPtr C_integrand_t -> Ptr UserData
        -> CDouble -> CDouble
        -> CInt -> CInt -> CInt -> CInt -> CInt
        -> CDouble
        -> Ptr CInt -> Ptr CInt -> Ptr CInt
        -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble
        -> IO ()

suave :: Int -> Int -> Integrand -> UserData -> Double -> Double -> [Result]
suave ndim ncomp integrand userData epsrel epsabs = unsafePerformIO $
    allocaBytes (8*ncomp) $ \p_value ->
    allocaBytes (8*ncomp) $ \p_error ->
    allocaBytes (8*ncomp) $ \p_prob  ->
    alloca $ \p_nregions         ->
    alloca $ \p_neval            ->
    alloca $ \p_fail             -> do
        p_userData <- new userData
        p_c_integrand <- return $ integrandConverter integrand
        c_suave (fromIntegral ndim) (fromIntegral ncomp)
            p_c_integrand p_userData
            (realToFrac epsrel) (realToFrac epsabs)
            2 (fromIntegral seed) 0 50000000 50000 0.1
            p_nregions p_neval p_fail
            p_value p_error p_prob
        values <- (liftM $ map realToFrac) $ peekArray ncomp p_value
        errors <- (liftM $ map realToFrac) $ peekArray ncomp p_error
        probs  <- (liftM $ map realToFrac) $ peekArray ncomp p_prob
        freeHaskellFunPtr p_c_integrand
        return $ seq (values, errors, probs) $ zipWith3 Result values errors probs

foreign import ccall "cuba.h Vegas"
    c_vegas :: CInt -> CInt
        -> FunPtr C_integrand_t -> Ptr UserData
        -> CDouble -> CDouble
        -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CChar
        -> Ptr CInt -> Ptr CInt
        -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble
        -> IO ()

vegas :: Int -> Int -> Integrand -> UserData -> Double -> Double -> [Result]
vegas ndim ncomp integrand userData epsrel epsabs = unsafePerformIO $
    allocaBytes (8*ncomp) $ \p_value ->
    allocaBytes (8*ncomp) $ \p_error ->
    allocaBytes (8*ncomp) $ \p_prob  ->
    alloca $ \p_neval            ->
    alloca $ \p_fail             -> do
        p_userData <- new userData
        p_c_integrand <- return $ integrandConverter integrand
        c_vegas (fromIntegral ndim) (fromIntegral ncomp)
            p_c_integrand p_userData
            (realToFrac epsrel) (realToFrac epsabs)
            2 (fromIntegral seed) 0 50000000 20000 10000 1000 0 nullPtr
            p_neval p_fail
            p_value p_error p_prob
        values <- (liftM $ map realToFrac) $ peekArray ncomp p_value
        errors <- (liftM $ map realToFrac) $ peekArray ncomp p_error
        probs  <- (liftM $ map realToFrac) $ peekArray ncomp p_prob
        freeHaskellFunPtr p_c_integrand
        return $ seq (values, errors, probs) $ zipWith3 Result values errors probs

foreign import ccall "cuba.h Cuhre"
    c_cuhre :: CInt -> CInt
        -> FunPtr C_integrand_t -> Ptr UserData
        -> CDouble -> CDouble
        -> CInt -> CInt -> CInt -> CInt
        -> Ptr CInt -> Ptr CInt -> Ptr CInt
        -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble
        -> IO ()

cuhre :: Int -> Int -> Integrand -> UserData -> Double -> Double -> [Result]
cuhre ndim ncomp integrand userData epsrel epsabs = unsafePerformIO $
    allocaBytes (8*ncomp) $ \p_value ->
    allocaBytes (8*ncomp) $ \p_error ->
    allocaBytes (8*ncomp) $ \p_prob  ->
    alloca $ \p_nregions         ->
    alloca $ \p_neval            ->
    alloca $ \p_fail             -> do
        p_userData <- new userData
        p_c_integrand <- return $ integrandConverter integrand
        c_cuhre (fromIntegral ndim) (fromIntegral ncomp)
            p_c_integrand p_userData
            (realToFrac epsrel) (realToFrac epsabs)
            6 0 10000 0
            p_nregions p_neval p_fail
            p_value p_error p_prob
        values <- (liftM $ map realToFrac) $ peekArray ncomp p_value
        errors <- (liftM $ map realToFrac) $ peekArray ncomp p_error
        probs  <- (liftM $ map realToFrac) $ peekArray ncomp p_prob
        freeHaskellFunPtr p_c_integrand
        return $ seq (values, errors, probs) $ zipWith3 Result values errors probs

foreign import ccall "wrapper"
    mkFunPtrC_integrand :: C_integrand_t -> IO (FunPtr C_integrand_t)

integrandConverter :: Integrand -> FunPtr C_integrand_t
integrandConverter integrand = unsafePerformIO $ do
    mkFunPtrC_integrand c_integrand where
        c_integrand p_ndim p_xs p_ncomp p_fs p_userData = do
            ndim <- peek p_ndim
            ncomp <- peek p_ncomp
            userData <- peek p_userData
            xs <- liftM (liftM  realToFrac) $ peekArray (fromIntegral ndim) p_xs
            fs <- return $ integrand xs userData
            if (length fs /= fromIntegral ncomp)
                then fail "Ncomp is not right"
                else return ()
            pokeArray p_fs $ map realToFrac fs 
            return ()


