module Parse
where

import Text.CSV
import Player

import Debug.Trace

pitchingFile :: String
pitchingFile = "pitchers2013Adv.csv"

parsePitchingFile :: IO ([Player])
parsePitchingFile = do
    result <- parseCSVFromFile pitchingFile
    case result of
      Left _         -> return ([])
      Right contents -> return (csvToPlayers contents)

csvToPlayers :: CSV -> [Player]
csvToPlayers csv = map recordToPlayer $ prune csv where
  prune csv = reverse $ drop 1 $ reverse csv

recordToPlayer [name, teamName , fip , xFip , war,  wFAc , wFTc , wFCc
               , wFSc , wFOc , wSIc , wSLc , wCUc , wKCc , wEPc
               , wCHc , wSCc , wKNc , faPct , ftPct , fcPct , fsPct
               , foPct , siPct , slPct , cuPct , kcPct , epPct , chPct
               , scPct , knPct , swStrPct , pid] = Player name teamName
                                                    (read fip) (read xFip)
                                                    (read wFAc)
                                                    (read wFTc) (read wFCc)
                                                    (read wFSc) (read wFOc)
                                                    (read wSIc) (read wSLc)
                                                    (read wCUc)
                                                    (read wKCc) (read wEPc)
                                                    (read wCHc) (read wSCc)
                                                    (read wKNc)
                                                    (read faPct) (read ftPct)
                                                    (read fcPct) (read fsPct)
                                                    (read foPct) (read siPct)
                                                    (read slPct) (read cuPct)
                                                    (read kcPct)
                                                    (read epPct)
                                                    (read chPct) (read scPct)
                                                    (read knPct)
                                                    (read swStrPct)
                                                    (read pid) [] ""
recordToPlayer rec = error $ "Error parsing: " ++ show rec
