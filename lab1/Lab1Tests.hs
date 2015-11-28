module Main
where

import qualified Lab1 as L
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

emptyMatrix :: [L.Object]
emptyMatrix = [[]]

nullMatrix :: [L.Object]
nullMatrix = [[0,0,0],[0,0,0],[0,0,0]]

onesMatrix :: [L.Object]
onesMatrix = [[1,1,1],[1,1,1],[1,1,1]]

multiMatrix :: [L.Object]
multiMatrix = [[6,6,6],[6,6,6],[6,6,6]]

emptyVector :: L.Object
emptyVector = []

onesVector :: L.Object
onesVector = [1,1,1,1,1,1,1,1,1]

-- ==================================================
testEmptyDistance :: L.DistanceType -> Test.HUnit.Test
testEmptyDistance d = TestCase (assertEqual "Distance of empty vectors: " 0.0 (L.distance d emptyVector emptyVector))

testSameDistance :: L.DistanceType -> Test.HUnit.Test
testSameDistance d = TestCase (assertEqual "Distance of same vectors: " 0.0 (L.distance d onesVector onesVector))

testSomeDistance :: L.DistanceType -> Test.HUnit.Test
testSomeDistance d 
                   | d == L.Euclidean = TestCase (assertEqual "Dist of vectors ([1,2,3] [2,4,6]): " (sqrt 14) (L.distance d [1,2,3] [2,4,6]))
                   | d == L.Hamming   = TestCase (assertEqual "Dist of vectors: " 3 (L.distance d [1,0,0,1,1] [0,0,1,0,1]))

testEmptyNorma :: Test.HUnit.Test
testEmptyNorma = TestCase (assertEqual "Matrix norm of null matricies: " 0.0 (L.matrixNorma nullMatrix nullMatrix))

testOnesNorma :: Test.HUnit.Test
testOnesNorma = TestCase (assertEqual "Matrix norm of ones matricies: " 0.0 (L.matrixNorma onesMatrix onesMatrix))

testMultiNorma :: Test.HUnit.Test
testMultiNorma = TestCase (assertEqual "Matrix norm of ones matrix and 6 matrix: " 5.0 (L.matrixNorma multiMatrix onesMatrix))

-- ==================================================
testDistanceHamming :: [Test.HUnit.Test]
testDistanceHamming = [TestLabel "Test Hamming distance (empty)" $ testEmptyDistance L.Hamming,
                       TestLabel "Test Hamming distance (same vectors)" $ testSameDistance L.Hamming,
                       TestLabel "Test Hamming distance (some vectors)" $ testSomeDistance L.Hamming]

testDistanceEuclidean :: [Test.HUnit.Test]
testDistanceEuclidean = [TestLabel "Test Euclidean distance (empty)" $ testEmptyDistance L.Euclidean,
                         TestLabel "Test Euclidean distance (same vectors)" $ testSameDistance L.Euclidean,
                         TestLabel "Test Euclidean distance (some vectors)" $ testSomeDistance L.Euclidean]

testMatrixNorma :: [Test.HUnit.Test]
testMatrixNorma = [TestLabel "Test matrix norma (null)" testEmptyNorma,
                   TestLabel "Test matrix norma (ones)" testOnesNorma,
                   TestLabel "Test matrix norma (one vs multi)" testMultiNorma]

-- ==================================================
-- hUnitTestToTests: Adapt an existing HUnit test into a list of test-framework tests
tests :: [Test.Framework.Test]
tests = hUnitTestToTests $ TestList $ concat [testMatrixNorma,testDistanceEuclidean,testDistanceHamming]

main :: IO ()
main = defaultMain tests