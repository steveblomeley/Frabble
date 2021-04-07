module Frabble.Scoring where

import Frabble.Types
import Frabble.Useful

-- Score a word
wordBonus :: Position -> LiveBonuses -> Int
wordBonus p bs = case b of
                     Just (Word n) -> n
                     _             -> 1
                 where b = tryFind p bs

letterBonus :: Position -> LiveBonuses -> Int
letterBonus p bs = case b of
                       Just (Letter n) -> n
                       _               -> 1
                   where b = tryFind p bs
                 
wordBonuses :: LiveWord -> LiveBonuses -> Int
wordBonuses [] _ = 1
wordBonuses (x:xs) bs = (wordBonus p bs) * (wordBonuses xs bs)
                        where (p,_) = x 

letterScores :: LiveWord -> LiveBonuses -> Int
letterScores [] _ = 0
letterScores (x:xs) bonuses = (score * bonus) + (letterScores xs bonuses)
                              where (pos,letter) = x
                                    score = Frabble.Useful.find letter scores 
                                    bonus = letterBonus pos bonuses        

wordScore :: LiveWord -> LiveBonuses -> Int
wordScore word bonuses = (letterScores word bonuses) * (wordBonuses word bonuses)
