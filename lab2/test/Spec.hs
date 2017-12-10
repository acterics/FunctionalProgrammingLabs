
import Test.Hspec
import Lib

simple_method :: Integer -> Integer -> Integer -> Integer
simple_method a b p = is_solved 0 where 
    is_solved x = do
        if mod ((a^x) - b) p == 0 then x
        else is_solved $ x + 1
    

main :: IO ()
main = hspec $ do 
    describe "should be" $ do
        it "simple method test" $ do 
            (simple_method 3 13 17) `shouldBe` 4
            (simple_method 3 14 17) `shouldBe` 9
            (simple_method 3 15 17) `shouldBe` 6
        it "pollard method test" $ do
            pollard_rho 3 13 17 `shouldBe` simple_method 3 13 17
            
            
    
    
