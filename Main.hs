module Main
where

import Parse
import Player
import Data.Trees.KdTree
import Data.Aeson
import GHC.Generics
import System.Environment

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS

import Debug.Trace

main :: IO ()
main = getArgs >>= parseCmdArgs

parseCmdArgs [minDist] = run (read minDist)
parseCmdArgs _ = error "wrong number of arguments"

run :: Double -> IO ()
run minDist = do
  plist <- parsePitchingFile
  let plist'    = map (\(p, i) -> p {pId = i}) $ zip plist [0..(length plist)]
      plist''   = map (\p -> assignColor p teams) plist'
      tree      = treeify plist''
      plist'''  = map (\p -> nnToComp p $ drop 1 $ kNearestNeighbors tree 6 p)
                    plist''
      links     = trimLinks minDist $ allLinks plist'''
      nodes     = allNodes plist'''
      fileData  = lazyToStrictBS $ encode $ PitcherData nodes links
  BS.writeFile "data.json" fileData
  return ()

trimLinks :: Double -> [Link] -> [Link]
trimLinks minDist links = foldr (\(Link s t d) total -> if d < minDist
                                                          then (Link s t d):total
                                                          else total) [] links

lazyToStrictBS :: LBS.ByteString -> BS.ByteString
lazyToStrictBS x = BS.concat $ LBS.toChunks x

data PitcherData = PitcherData { nodes :: [Node]
                               , links :: [Link]
                               } deriving (Show, Generic)

instance ToJSON PitcherData

data Node = Node { name  :: String
                 , group :: Int
                 , color :: String
                 } deriving (Show, Generic)

instance ToJSON Node

data Link = Link { source :: Int
                 , target :: Int
                 , distance  :: Double
                 } deriving (Show, Generic)

instance ToJSON Link

allLinks :: [Player] -> [Link]
allLinks players = foldr (\p s -> s ++ (map (\(l, d) -> Link (pId p) l d)
                     (kNN p))) [] players

allNodes :: [Player] -> [Node]
allNodes = map (\p -> Node (Player.name p) (pId p) (Player.color p))

nnToComp :: Player -> [Player] -> Player
nnToComp p pList =
    let comps = map (\p' -> (pId p', dist2 p' p)) pList
    in p { kNN = comps}

treeify :: [Player] -> KdTree Player
treeify = fromList
