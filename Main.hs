module Main where

import Cuba

myIntegrand :: Integrand
myIntegrand xs d | length xs /= 3 = undefined
                 | otherwise =
    [x, (sin x) * (cos y) * (exp z), x, x**2, x**6 * y * z, s] where
        [x, y, z] = xs
        (s) = d

main :: IO()
main = do
    let results = vegas 3 6 myIntegrand (1) 0.001 0
    print results


