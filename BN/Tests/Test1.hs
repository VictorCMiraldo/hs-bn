module Test1 where

-- Corresponds to test1.net

import BN.Common
import BN.BayesianNet

import qualified Data.Map as M

-----------------------------------------------------------------------------------------------
-- * Running a test

runTest :: BN Identity a -> Either BError a
runTest f = runIdentity $ runBN [("bool", ["T", "F"])] f

runTestBayesT :: BayesT Identity a -> Either BError a
runTestBayesT f = runIdentity $ runBayesT [("bool", ["T", "F"])] f

{-
    Right [("v1",0.2,0.8),("v2",0.32000002,0.68),("v3",0.42400002,0.57600003),("v4",0.27112,0.72888),("v5",0.45,0.55)]
    
    coincides with SamIam priors for the same network! :)
-}
test1ok :: IO ()
test1ok
  = do
    let ps = runTestBayesT (setupOk >> getPs ["v1", "v2", "v3", "v4", "v5"])
    putStrLn . show $ ps


{- 
    Right [("v1",0.25,0.75),("v2",1.0,0.0),("v3",0.9,0.1),("v4",0.435,0.565),("v5",0.45,0.55)]
    
    Same as SamIam too. :)
-}
test2ok :: IO ()
test2ok
  = do
    let ps = runTestBayesT (do 
        setupOk
        bnObserve "v2" "T"
        getPs ["v1", "v2", "v3", "v4", "v5"])
    putStrLn . show $ ps

-----------------------------------------------------------------------------------------------
-- * Setup Testing

-- Sucessfull setup.
setupOk :: (M m) => BayesT m ()
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
setupErr1 :: (M m) => BayesT m ()
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
setupErr2 :: (M m) => BayesT m ()
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

getPs :: (M m) => [Lbl] -> BayesT m [(Lbl, Prob, Prob)]
getPs ls
  = do
    pvs <- mapM bnDataFusion ls
    return $ map (\(l, p) -> (l, p "T", p "F")) (zip ls pvs)
