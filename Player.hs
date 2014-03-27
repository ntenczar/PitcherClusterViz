module Player
where

import Data.Trees.KdTree

data Player = Player { name     :: String
                     , teamName :: String
                     , fip      :: Double
                     , xFip     :: Double
                     , wFAc     :: Double
                     , wFTc     :: Double
                     , wFCc     :: Double
                     , wFSc     :: Double
                     , wFOc     :: Double
                     , wSIc     :: Double
                     , wSLc     :: Double
                     , wCUc     :: Double
                     , wKCc     :: Double
                     , wEPc     :: Double
                     , wCHc     :: Double
                     , wSCc     :: Double
                     , wKNc     :: Double
                     , faPct    :: Double
                     , ftPct    :: Double
                     , fcPct    :: Double
                     , fsPct    :: Double
                     , foPct    :: Double
                     , siPct    :: Double
                     , slPct    :: Double
                     , cuPct    :: Double
                     , kcPct    :: Double
                     , epPct    :: Double
                     , chPct    :: Double
                     , scPct    :: Double
                     , knPct    :: Double
                     , swStrPct :: Double
                     , pId      :: Int
                     , kNN      :: [(Int, Double)]
                     , color    :: String
                     } deriving (Show, Eq, Read)

instance Point Player where
    -- dimension :: Player -> Int
    dimension p = 1
    -- coord :: Int -> Player -> Double
    coord 0 p = faPct p
    -- dist2 :: Player -> Player -> Double
    dist2 p1 p2 = foldr (\m s -> s + (m p1 p2)) 0 compList

compList = [swStrComp, faComp, ftComp, fcComp, fsComp, foComp, siComp
           , slComp, cuComp, kcComp, epComp, chComp, scComp, knComp
           , fipComp]

assignColor :: Player -> [(String, String, String)] -> Player
assignColor p []   = p { color = "ffffff" }
assignColor p ((tName,_,c):ls) = if tName == teamName p
                                 then p { color = c }
                                 else assignColor p ls

teams = [("Angels"        , "ANA"     , "cc3300")
        , ("Diamondbacks" , "ARI"     , "c4122c")
        , ("Braves"       , "ATL"     , "0c5284")
        , ("Orioles"      , "BAL"     , "f3782c")
        , ("Red Sox"      , "BOS"     , "e2333e")
        , ("White Sox"    , "CHA"     , "231f20")
        , ("Cubs"         , "CHN"     , "45694")
        , ("Reds"         , "CIN"     , "ec164c")
        , ("Indians"      , "CLE"     , "ed174c")
        , ("Rockies"      , "COL"     , "241e1c")
        , ("Tigers"       , "DET"     , "023d69")
        , ("Astros"       , "HOU"     , "ffa500")
        , ("Royals"       , "KCA"     , "998040")
        , ("Dodgers"      , "LAN"     , "045a9c")
        , ("Brewers"      , "MIL"     , "04225c")
        , ("Twins"        , "MIN"     , "42462")
        , ("Yankees"      , "NYA"     , "0")
        , ("Mets"         , "NYN"     , "f47e34")
        , ("Athletics"    , "OAK"     , "339900")
        , ("Phillies"     , "PHI"     , "d41244")
        , ("Pirates"      , "PIT"     , "d79500")
        , ("Padres"       , "SDN"     , "6496c4")
        , ("Mariners"     , "SEA"     , "519d97")
        , ("Giants"       , "SFN"     , "f47a3c")
        , ("Cardinals"    , "SLN"     , "d47b12")
        , ("Rays"         , "TBA"     , "6699cc")
        , ("Rangers"      , "TEX"     , "4482")
        , ("Blue Jays"    , "TOR"     , "3300ff")
        , ("Nationals"    , "WAS"     , "04265c")
        , ("Marlins"      , "FLO"     , "04a2b4")]

fipComp :: Player -> Player -> Double
fipComp p1 p2 = sq $ (fip p1) - (fip p2)

swStrComp :: Player -> Player -> Double
swStrComp p1 p2 = sq $ (swStrPct p1) - (swStrPct p2)

faComp :: Player -> Player -> Double
faComp p1 p2 = sq $ (fa p1) - (fa p2)

ftComp :: Player -> Player -> Double
ftComp p1 p2 = sq $ (ft p1) - (ft p2)

fcComp :: Player -> Player -> Double
fcComp p1 p2 = sq $ (fc p1) - (fc p2)

fsComp :: Player -> Player -> Double
fsComp p1 p2 = sq $ (fs p1) - (fs p2)

foComp :: Player -> Player -> Double
foComp p1 p2 = sq $ (fo p1) - (fo p2)

siComp :: Player -> Player -> Double
siComp p1 p2 = sq $ (si p1) - (si p2)

slComp :: Player -> Player -> Double
slComp p1 p2 = sq $ (sl p1) - (sl p2)

cuComp :: Player -> Player -> Double
cuComp p1 p2 = sq $ (cu p1) - (cu p2)

kcComp :: Player -> Player -> Double
kcComp p1 p2 = sq $ (kc p1) - (kc p2)

epComp :: Player -> Player -> Double
epComp p1 p2 = sq $ (ep p1) - (ep p2)

chComp :: Player -> Player -> Double
chComp p1 p2 = sq $ (ch p1) - (ch p2)

scComp :: Player -> Player -> Double
scComp p1 p2 = sq $ (sc p1) - (sc p2)

knComp :: Player -> Player -> Double
knComp p1 p2 = sq $ (kn p1) - (kn p2)

fa :: Player -> Double
fa p = (faPct p) * (wFAc p)

ft :: Player -> Double
ft p = (ftPct p) * (wFTc p)

fc :: Player -> Double
fc p = (fcPct p) * (wFCc p)

fs :: Player -> Double
fs p = (fsPct p) * (wFSc p)

fo :: Player -> Double
fo p = (foPct p) * (wFOc p)

si :: Player -> Double
si p = (siPct p) * (wSIc p)

sl :: Player -> Double
sl p = (slPct p) * (wSLc p)

cu :: Player -> Double
cu p = (cuPct p) * (wCUc p)

kc :: Player -> Double
kc p = (kcPct p) * (wKCc p)

ep :: Player -> Double
ep p = (epPct p) * (wEPc p)

ch :: Player -> Double
ch p = (chPct p) * (wCHc p)

sc :: Player -> Double
sc p = (scPct p) * (wSCc p)

kn :: Player -> Double
kn p = (knPct p) * (wKNc p)

sq :: Double -> Double
sq x = x * x

{-
teams = [("Los Angeles Angels"    , "ANA"    , "cc3300")
        ,("Arizona Diamondbacks"  , "ARI"    , "c4122c")
        ,("Atlanta Braves"        , "ATL"    , "0c5284")
        ,("Baltimore Orioles"     , "BAL"    , "f3782c")
        ,("Boston Red Sox"        , "BOS"    , "e2333e")
        ,("Chicago White Sox"     , "CHA"    , "231f20")
        ,("Chicago Cubs"          , "CHN"    , "45694")
        ,("Cincinnati Reds"       , "CIN"    , "ec164c")
        ,("Cleveland Indians"     , "CLE"    , "ed174c")
        ,("Colorado Rockies"      , "COL"    , "241e1c")
        ,("Detroit Tigers"        , "DET"    , "023d69")
        ,("Houston Astros"        , "HOU"    , "ffa500")
        ,("Kansas City Royals"    , "KCA"    , "998040")
        ,("Los Angeles Dodgers"   , "LAN"    , "045a9c")
        ,("Miami Marlins"         , "MIA"    , "04a2b4")
        ,("Milwaukee Brewers"     , "MIL"    , "04225c")
        ,("Minnesota Twins"       , "MIN"    , "42462")
        ,("New York Yankees"      , "NYA"    , "0")
        ,("New York Mets"         , "NYN"    , "f47e34")
        ,("Oakland Athletics"     , "OAK"    , "339900")
        ,("Philadelphia Phillies" , "PHI"    , "d41244")
        ,("Pittsburgh Pirates"    , "PIT"    , "d79500")
        ,("San Diego Padres"      , "SDN"    , "6496c4")
        ,("Seattle Mariners"      , "SEA"    , "519d97")
        ,("San Francisco Giants"  , "SFN"    , "f47a3c")
        ,("St. Louis Cardinals"   , "SLN"    , "d47b12")
        ,("Tampa Bay Rays"        , "TBA"    , "6699cc")
        ,("Texas Rangers"         , "TEX"    , "4482")
        ,("Toronto Blue Jays"     , "TOR"    , "3300ff")
        ,("Washington Nationals"  , "WAS"    , "04265c")
        ,("Miami Marlins"         , "FLO"    , "04a2b4")]
        -}
