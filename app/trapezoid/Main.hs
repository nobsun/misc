{-# LANGUAGE MultiWayIf #-}
module Main where

import Control.Parallel
import Control.Parallel.Strategies
import Data.Maybe
import Numeric.Natural

main :: IO ()
main = putStr $ unlines $ map (show . trapezoids) [10]

parTrapezoids :: Int -> (Int, Int, [(Integer, Integer)])
parTrapezoids d = (d, len, cs)
    where
        cs = catMaybes $ parMap rpar phi [10^(d-1) .. 10^d-1]
        len = length cs
        phi b = case divMod (b*(b-2)) (2*10^d-b) of
            (a,0) | a < 10^d && a >= 10^(d-1) -> Just (a, b)
            _                                 -> Nothing

trapezoids :: Int -> (Int, Int, [(Integer, Integer)])
trapezoids d = (d, len, cs)
    where
        cs = [(a,b) | b <- [10^(d-1) .. 10^d - 1]
                    , let (a,r) = divMod (b*(b-2)) (2*10^d-b)
                    , r == 0
                    , a < 10^d
                    , a >= 10^(d-1)]
        len = length cs

trapezoids' :: Int -> (Int, Int, [(Integer, Integer)])
trapezoids' d = if
    | d < 7 -> trapezoids d
    | otherwise -> (d, len, sols)
    where
        len = length sols
        sols = concat $ parMap rpar (solve d) bss
        w = 10^3
        bss = splitEvery w [10^(d-1) .. 10^d - 1]

solve :: Int -> [Integer] -> [(Integer,Integer)]
solve d bs = cs
    where
        cs = catMaybes $ parMap rpar phi bs
        len = length cs
        phi b = case divMod (b*(b-2)) (2*10^d-b) of
            (a,0) | a < 10^d && a >= 10^(d-1) -> Just (a, b)
            _                                 -> Nothing

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = case splitAt n xs of
    (ys, zs) -> case zs of
        [] -> [ys]
        _  -> ys : splitEvery n zs


{- ^
>>> length trapezoids
149
>>> trapezoids
[(100902,401600),(107302,412700),(109252,416000),(114682,425000),(116232,427520)
,(122257,437120),(124102,440000),(128002,446000),(132733,453125),(134402,455600)
,(137402,460000),(137952,460800),(138462,461540),(140427,464375),(147052,473750)
,(148810,476192),(155152,484850),(157502,488000),(164247,496875),(165502,498500)
,(166666,500000),(173092,508160),(178602,515000),(181482,518520),(182702,520000)
,(184027,521600),(195202,534800),(197542,537500),(202477,543125),(203252,544000)
,(204546,545456),(214312,556250),(215002,557000),(216442,558560),(217777,560000)
,(227202,570000),(228572,571430),(232502,575500),(235061,578125),(250344,593408)
,(253002,596000),(257142,600000),(258602,601400),(270382,612500),(272002,614000)
,(272782,614720),(284090,625000),(285627,626375),(286552,627200),(292522,632480)
,(295402,635000),(307623,645504),(311352,648650),(312962,650000),(316802,653200)
,(324307,659375),(333334,666668),(335002,668000),(336812,669440),(346752,677250)
,(350302,680000),(360118,687500),(362902,689600),(373652,697600),(376922,700000)
,(377777,700625),(378702,701300),(383802,705000),(386680,707072),(395002,713000)
,(402052,717950),(403157,718720),(406847,721280),(416837,728125),(419842,730160)
,(430627,737375),(434602,740000),(448052,748750),(451952,751250),(453127,752000)
,(460702,756800),(469822,762500),(473022,764480),(478752,768000),(480770,769232)
,(484502,771500),(497002,779000),(500800,781250),(512122,787880),(516477,790400)
,(527799,796875),(528902,797500),(531202,798800),(533332,800000),(550477,809525)
,(555002,812000),(560186,814816),(562377,816000),(567492,818750),(584662,827840)
,(588802,830000),(597502,834500),(615708,843750),(618202,845000),(619402,845600)
,(621904,846848),(640502,856000),(642858,857144),(649602,860400),(653982,862500)
,(678127,873875),(680554,875000),(684452,876800),(691427,880000),(693882,881120)
,(711112,888890),(713602,890000),(716302,891200),(717601,891776),(736362,900000)
,(738902,901100),(743597,903125),(750268,905984),(755002,908000),(778252,917750)
,(781082,918920),(783702,920000),(789942,922560),(802102,927500),(812377,931625)
,(819352,934400),(838202,941800),(843877,944000),(859522,950000),(863947,951680)
,(884467,959375),(886152,960000),(887502,960500),(888962,961040),(897002,964000)
,(910036,968750),(914602,970400),(925642,974360),(930952,976250),(948702,982500)
,(953374,984128),(970102,989900),(976252,992000),(997002,999000)
-}
{-
 1:  2
 2:  5
 3: 10
 4: 17
 5: 20
 6:149
 7: 24
 8:132
 9: 47
10:151
11: 30
-}
{-
(10,151,[(1000090002,4000160000),(1069558156,4121093750),(1083357392,4144531250),(1092525252,4160000000),(1136180402,4232569600),(1150407442,4255842560),(1212725502,4355712000),(1224900252,4374843750),(1225099752,4375156250),(1234756098,4390243904),(1234856252,4390400000),(1327335858,4531250000),(1375481252,4602118750),(1391153752,4624846250),(1391260162,4625000000),(1401559852,4639846400),(1459705002,4722375000),(1484012402,4756250000),(1551515152,4848484850),(1562514436,4863266816),(1666550002,4999850000),(1666666666,5000000000),(1738240002,5090760000),(1741632002,5095000000),(1841966002,5218034000),(1860147602,5239852400),(1860271002,5240000000),(1939520002,5333480000),(2045325002,5454400000),(2045454546,5454545456),(2148186252,5568000000),(2164740006,5585937500),(2177644002,5599856000),(2177777777,5600000000),(2259745402,5687129600),(2263625002,5691200000),(2347361252,5777920000),(2364405902,5795312500),(2378187362,5809312640),(2475124722,5906250000),(2489234202,5920140800),(2506800627,5937359375),(2595012502,6022587500),(2599178302,6026562500),(2609322877,6036224000),(2689038002,6111250000),(2703463754,6124661248),(2722093127,6141906875),(2840909090,6250000000),(2841062502,6250137500),(2851683711,6259644416),(2856057377,6263552000),(2969540002,6363500000),(3070135502,6449864500),(3070295202,6450000000),(3093932002,6470000000),(3228316002,6581684000),(3232980002,6585500000),(3333333334,6666666668),(3333500002,6666800000),(3486356995,6787109375),(3503030302,6800000000),(3607631202,6879868800),(3607804877,6880000000),(3646290002,6908960000),(3741010327,6979296875),(3758130082,6991869920),(3758307502,6992000000),(3784562502,7011200000),(3866792930,7070707072),(4031828127,7187371875),(4032012194,7187500000),(4049815502,7199872000),(4050184502,7200128000),(4072794752,7215781250),(4191439882,7296875000),(4219235802,7315625000),(4306262627,7373737375),(4324881822,7386055680),(4353138454,7404674048),(4475977277,7484569600),(4499805002,7499875000),(4500195002,7500125000),(4698232322,7625000000),(4790305002,7681695000),(4820123002,7699877000),(4820325202,7700000000),(4949700002,7777900000),(4995121952,7804878050),(4995328002,7805000000),(5121212122,7878787880),(5333120002,7999880000),(5333333332,8000000000),(5463642002,8072608000),(5469790002,8076000000),(5650572802,8174427200),(5683118082,8191881920),(5683338752,8192000000),(5824466002,8266784000),(5988963752,8352156250),(5995420212,8355468750),(6011136252,8363520000),(6219596882,8468750000),(6241990202,8479884800),(6367658752,8541781250),(6383890072,8549703680),(6390571252,8552960000),(6563398002,8636250000),(6586924890,8647450112),(6751718402,8725000000),(6775606112,8736112640),(6805312502,8749887500),(6805555554,8750000000),(6953930002,8818070000),(6960926002,8821250000),(7111111112,8888888890),(7111360002,8889000000),(7166474502,8913525500),(7363636362,9000000000),(7363890002,9000110000),(7575500002,9090800000),(7740108402,9159891600),(7740369002,9160000000),(7778915002,9176000000),(7997152802,9265347200),(8004700002,9268400000),(8166935002,9333440000),(8199806827,9346484375),(8371915358,9414062500),(8412145462,9429687500),(8438787877,9440000000),(8605479962,9503895040),(8639859377,9516940625),(8647732527,9519921875),(8666894502,9527168000),(8816946502,9583437500),(8844004066,9593495936),(8844284377,9593600000),(8885743127,9608960000),(9100378786,9687500000),(9274102502,9749897500),(9274390242,9750000000),(9302211777,9759897600),(9302788227,9760102400),(9338095002,9772625000),(9522601627,9837398375),(9522893602,9837500000),(9566076002,9852500000),(9701010102,9898989900),(9729827333,9908844544),(9999700002,9999900000)])
5,080,396,969,928 bytes allocated in the heap
  93,765,381,080 bytes copied during GC
     382,848,032 bytes maximum residency (201 sample(s))
     306,934,752 bytes maximum slop
            1362 MiB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0     439086 colls, 439086 par   313.476s  70.523s     0.0002s    0.0084s
  Gen  1       201 colls,   200 par   55.042s  15.851s     0.0789s    0.5673s

  Parallel GC work balance: 24.63% (serial 0%, perfect 100%)

  TASKS: 34 (1 bound, 33 peak workers (33 total), using -N16)

  SPARKS: 19155191384 (11968574541 converted, 949761 overflowed, 74146 dud, 230454351 GC'd, 6954147353 fizzled)

  INIT    time    0.007s  (  0.050s elapsed)
  MUT     time  15074.907s  (1005.391s elapsed)
  GC      time  368.517s  ( 86.375s elapsed)
  EXIT    time    0.058s  (  0.005s elapsed)
  Total   time  15443.489s  (1091.820s elapsed)

  Alloc rate    337,010,177 bytes per MUT second

  Productivity  97.6% of total user, 92.1% of total elapsed

stack exec -- trapezoid +RTS -N16 -s  15256.17s user 187.60s system 1414% cpu 18:12.10 total
-}