module Test1 where

-- Corresponds to test1.net

import BN.Common
import BN.BayesianNet

import qualified Data.Map as M

-----------------------------------------------------------------------------------------------
-- * Running a test

runTest :: BN Identity a -> Either BError a
runTest f = runIdentity $ runBN [("bool", ["T", "F"])] f

runTestIO :: BN IO a -> IO (Either BError a)
runTestIO = runBN [("bool", ["T", "F"])] 


{-
    Right [("v1",0.2,0.8),("v2",0.32000002,0.68),("v3",0.42400002,0.57600003),("v4",0.27112,0.72888),("v5",0.45,0.55)]
    
    coincides with SamIam priors for the same network! :)
-}
test1ok :: IO ()
test1ok
  = do
    let ps = runTest $ setupOk >> bnQuery (getPs ["v1", "v2", "v3", "v4", "v5"])
    putStrLn . show $ ps


{- 
    
    v2 = T
    Right [("v1",0.25,0.75),("v2",1.0,0.0),("v3",0.9,0.1),("v4",0.435,0.565),("v5",0.45,0.55)]
    
    v2 = T && v5 = F
    Right [("v1",0.25,0.75),("v2",1.0,0.0),("v3",0.9,0.1),("v4",0.3,0.7),("v5",0.0,1.0)]
    
    Same as SamIam too. :)
-}
test2ok :: IO ()
test2ok
  = do
    let (Right (p1, p2)) = runTest (do 
        setupOk
        bnPrepareQuery
        bnObserve "v2" "T"
        x <- bnQuery $ getPs ["v1", "v2", "v3", "v4", "v5"]
        bnObserve "v5" "F"
        y <- bnQuery $ getPs ["v1", "v2", "v3", "v4", "v5"]
        return (x, y))
    putStrLn . show $ p1
    putStrLn . show $ p2
    
test3ok :: IO (Either BError ())
test3ok = runTestIO (
    do
      setupOk
      bnPrepareQuery
      x <- bnRecursiveCond "v2"
      y <- bnRecursiveCond "v3"
      lift $ lift $ lift $ showp "v2" x
      lift $ lift $ lift $ showp "v3" y
      bnObserve "v2" "T"
      x' <- bnRecursiveCond "v2"
      y' <- bnRecursiveCond "v3"
      lift $ lift $ lift $ showp "v2" x'
      lift $ lift $ lift $ showp "v3" y'
      return ()
    )

showp :: Lbl -> (Val -> Prob) -> IO ()
showp l f 
  = do
      putStrLn l
      putStrLn ("    T: " ++ show (f "T"))
      putStrLn ("    F: " ++ show (f "F"))
          
loopyTest :: IO (Either BError ())
loopyTest = runTestIO (
  do
    loopySetup
    bnObserve "v3" "F"
    g <- bnRecursiveCond "v1"
    f <- bnQuery $ 
            bnMarginalizeLCS (bnDataFusion "v4")
    
    lift $ lift $ lift $ showp "v1" g
    lift $ lift $ lift $ showp "v4" f
    return ()
  )
  
-----------------------------------------------------------------------------------------------
-- * Setup Testing

loopySetup :: (M m) => BN m ()
loopySetup
  = do
    mapM_ (flip bnAddNode "bool") ["v1", "v2", "v3", "v4", "v5"]
    mapM_ (uncurry bnAddArc) [("v1", "v2"), ("v1", "v3")
                             ,("v2", "v5"), ("v2", "v4")
                             ,("v3", "v4")
                             ]
                             
    bnSetGamma "v1" $ M.fromList [(("T", []), 0.8), (("F", []), 0.2)]
    bnSetGamma "v2" $ gammaAddNames ["v1"]
      [(("T", ["T"]), 0.9)
      ,(("F", ["T"]), 0.1)
      ,(("T", ["F"]), 0.3)
      ,(("F", ["F"]), 0.7)
      ]
      
    bnSetGamma "v5" $ gammaAddNames ["v2"]
      [(("T", ["T"]), 0.4)
      ,(("F", ["T"]), 0.6)
      ,(("T", ["F"]), 0.5)
      ,(("F", ["F"]), 0.5)
      ]
      
    bnSetGamma "v3" $ gammaAddNames ["v1"]
      [(("T", ["T"]), 0.2)
      ,(("F", ["T"]), 0.8)
      ,(("T", ["F"]), 0.6)
      ,(("F", ["F"]), 0.4)
      ]
      
    bnSetGamma "v4"
      $ gammaAddNames ["v2", "v3"]
        [ ( ("T", ["T", "T"]),  0.1)
        , ( ("F", ["T", "T"]), 0.9)
        , ( ("T", ["T", "F"]), 0.6)
        , ( ("F", ["T", "F"]), 0.4)
        , ( ("T", ["F", "T"]), 0.2)
        , ( ("F", ["F", "T"]), 0.8)
        , ( ("T", ["F", "F"]), 0.1)
        , ( ("F", ["F", "F"]), 0.9)
        ]  
        
    bnPrepareQuery

-- Sucessfull setup.
setupOk :: (M m) => BN m ()
setupOk
  = do
    mapM_ (flip bnAddNode "bool") ["v1", "v2", "v3", "v4", "v5"]
    mapM_ (uncurry bnAddArc) [("v1", "v2"), ("v2", "v3")
                             ,("v2", "v4"), ("v5", "v4")
                             ]
    -- Root Nodes
    bnSetGamma "v1" $ M.fromList [(("T", []), 0.2),  (("F", []), 0.8)]
    bnSetGamma "v5" $ M.fromList [(("T", []), 0.45), (("F", []), 0.55)]
    
    -- One Parent Nodes
    bnSetGamma "v2" 
      $ gammaAddNames ["v1"] 
         [(("T", ["T"]),   0.4)
         ,(("F", ["T"]),  0.6)
         ,(("T", ["F"]),  0.3)
         ,(("F", ["F"]), 0.7)
         ]
                                        
    bnSetGamma "v3"
      $ gammaAddNames ["v2"]
        [(("T", ["T"]),   0.9)
        ,(("F", ["T"]),  0.1)
        ,(("T", ["F"]),  0.2)
        ,(("F", ["F"]), 0.8)
        ]
    -- Two Parent nodes                               
    bnSetGamma "v4"
      $ gammaAddNames ["v2", "v5"]
        [ ( ("T", ["T", "T"]),  0.6)
        , ( ("F", ["T", "T"]), 0.4)
        , ( ("T", ["T", "F"]), 0.3)
        , ( ("F", ["T", "F"]), 0.7)
        , ( ("T", ["F", "T"]), 0.15)
        , ( ("F", ["F", "T"]), 0.85)
        , ( ("T", ["F", "F"]), 0.23)
        , ( ("F", ["F", "F"]), 0.77)
        ]
        
-- Erroneous setup, probabilities do not sum to 1.
setupErr1 :: (M m) => BN m ()
setupErr1
  = do
    mapM_ (flip bnAddNode "bool") ["v1", "v2", "v3", "v4", "v5"]
    mapM_ (uncurry bnAddArc) [("v1", "v2"), ("v2", "v3")
                             ,("v2", "v4"), ("v5", "v4")
                             ]
    -- Root Nodes
    bnSetGamma "v1" $ M.fromList [(("T", []), 0.2),  (("F", []), 0.8)]
    bnSetGamma "v5" $ M.fromList [(("T", []), 0.45), (("F", []), 0.55)]
    
    -- One Parent Nodes
    bnSetGamma "v2" 
      $ gammaAddNames ["v1"] 
         [(("T", ["T"]),   0.4)
         ,(("F", ["T"]),  0.6)
         ,(("T", ["F"]),  0.3)
         ,(("F", ["F"]), 0.7)
         ]
                                        
    bnSetGamma "v3"
      $ gammaAddNames ["v2"]
        [(("T", ["T"]),   0.2) -- 0.9
        ,(("F", ["T"]),  0.1)
        ,(("T", ["F"]),  0.2)
        ,(("F", ["F"]), 0.8)
        ]
    -- Two Parent nodes                               
    bnSetGamma "v4"
      $ gammaAddNames ["v2", "v5"]
        [ ( ("T", ["T", "T"]),  0.6)
        , ( ("F", ["T", "T"]), 0.4)
        , ( ("T", ["T", "F"]), 0.3)
        , ( ("F", ["T", "F"]), 0.7)
        , ( ("T", ["F", "T"]), 0.15)
        , ( ("F", ["F", "T"]), 0.85)
        , ( ("T", ["F", "F"]), 0.23)
        , ( ("F", ["F", "F"]), 0.77)
        ]
        
-- Erroneous setup, assessment tables incomplete.
setupErr2 :: (M m) => BN m ()
setupErr2
  = do
    mapM_ (flip bnAddNode "bool") ["v1", "v2", "v3", "v4", "v5"]
    mapM_ (uncurry bnAddArc) [("v1", "v2"), ("v2", "v3")
                             ,("v2", "v4"), ("v5", "v4")
                             ]
    -- Root Nodes
    bnSetGamma "v1" $ M.fromList [(("T", []), 0.2),  (("F", []), 0.8)]
    bnSetGamma "v5" $ M.fromList [(("T", []), 0.45), (("F", []), 0.55)]
    
    -- One Parent Nodes
    bnSetGamma "v2" 
      $ gammaAddNames ["v1"] 
         [(("T", ["T"]),   0.4)
         ,(("F", ["T"]),  0.6)
         ,(("T", ["F"]),  0.3)
         ,(("F", ["F"]), 0.7)
         ]
                                        
    bnSetGamma "v3"
      $ gammaAddNames ["v2"]
        [(("T", ["T"]),   0.9)
        ,(("F", ["T"]),  0.1)
        ,(("T", ["F"]),  0.2)
        ,(("F", ["F"]), 0.8)
        ]
    -- Two Parent nodes                               
    bnSetGamma "v4"
      $ gammaAddNames ["v2", "v5"]
        [ ( ("T", ["T", "T"]),  0.6)
        , ( ("F", ["T", "T"]), 0.4)
        , ( ("T", ["T", "F"]), 0.3)
        -- , ( ("F", ["T", "F"]), 0.7)
        , ( ("T", ["F", "T"]), 0.15)
        , ( ("F", ["F", "T"]), 0.85)
        , ( ("T", ["F", "F"]), 0.23)
        , ( ("F", ["F", "F"]), 0.77)
        ]

-----------------------------------------------------------------------------------------------
-- * Reading all Priors

getPs :: (M m) => [Lbl] -> QueryBN m [(Lbl, Prob, Prob)]
getPs ls
  = do
    pvs <- mapM bnDataFusion ls
    return $ map (\(l, p) -> (l, p "T", p "F")) (zip ls pvs)
