import Data.Sort (sort, sortBy)
import Data.List (group, partition, permutations, foldl', scanl', delete)
import Control.Monad (guard)

type Apfel = Float
type Apfelkiste = [Apfel]
data Packung = Packung Apfel Apfel Apfel Apfel
                  deriving Show

dirksKiste = [286,295,203,278,239,207,262,298,281,208,
              222,233,256,293,215,242,261,222,229,289,
              205,207,221,271,229,283,226,203,294,204,
              202,290,200,228,282,217,273,217,283,235,
              211,273,212,200,269,218,230,274,297,242,
              281,217,211,296,241,266,248,222,238,290,
              267,289,280,213,263,290,242,297,284,261,
              223,262,208,286,226,265,285,208,289,285,
              250,243,288,209,219,286,206,292,217,254,
              219,210,283,291,238,207,289,273,227,287] :: Apfelkiste

durchschnittsGewichtApfel = 250.0         

zweiAusKiste :: Apfelkiste -> [(Apfel, Apfel)]
zweiAusKiste kiste@(a1:a2:_) = let kombi = do
                                      a1 <- sort kiste
                                      a2 <- sort kiste
                                      guard (a1 < a2)
                                      return (a1, a2)
                             in if null kombi then [(a1,a2)] else kombi

bestPaarFit :: Apfel -> Apfel -> [(Apfel, Apfel)] -> (Apfel, Apfel, Apfel, Apfel)
bestPaarFit a1 a2 as = let (a3, a4, diff) = foldr iter (-1.0, -1.0, 5000) as
                           iter (a1', a2') (a3', a4', diff') | d' < diff' = (a1', a2', d')
                                                             | otherwise = (a3', a4', diff')
                                                             where d' = abs (4*durchschnittsGewichtApfel - a1 - a2 - a1' - a2')
                       in (a1, a2, a3, a4)

apfelKisteEinpacken :: Apfelkiste -> [Packung]
apfelKisteEinpacken (a1:a2:a3:a4:as) = Packung a1 a2 a3 a4 : apfelKisteEinpacken as
apfelKisteEinpacken _                = []

sortNachHäufigkeitLeichtZuerst :: Apfelkiste -> Apfelkiste
sortNachHäufigkeitLeichtZuerst ak = concat (sortBy (\l1 l2 -> compare (length l2) (length l1)) (group $ sort ak))

sortNachHäufigkeitSchwerZuerst :: Apfelkiste -> Apfelkiste
sortNachHäufigkeitSchwerZuerst ak = concat (sortBy (\l1 l2 -> compare (length l2) (length l1)) (group $ reverse $ sort ak))

sortNachGeeignesterApfel :: Float -> Apfelkiste -> Apfelkiste
sortNachGeeignesterApfel gewicht = sortBy (\gewicht1 gewicht2 -> compare ( abs (gewicht1 - gewicht))( abs (gewicht2 - gewicht)))

nehmeÄhnlichenJeNachGewichtLeichterOderSchwerer :: Float -> Apfelkiste -> Apfelkiste
nehmeÄhnlichenJeNachGewichtLeichterOderSchwerer gewicht kiste | gewicht > durchschnittsGewichtApfel = let (as1, as2) = partition (>durchschnittsGewichtApfel) kiste
                                                                                                      in reverse as2 ++ as1
                                                              | otherwise                           = let (as1, as2) = partition (<=durchschnittsGewichtApfel) kiste
                                                                                                      in reverse as2 ++ as1

packe :: Apfelkiste -> [Packung]
packe äpfel@(_:_: _: _: _) = let (kleinsterApfel:restÄpfel) = sort äpfel
                                 (größterApfel:restÄpfel') = reverse restÄpfel
                                 (häufigsterApfel:restÄpfel'') = sortNachHäufigkeitSchwerZuerst restÄpfel'
                                 fehlendesGewicht = 4 * durchschnittsGewichtApfel - kleinsterApfel - größterApfel - häufigsterApfel
                                 (besterApfel:restÄpfel''') = sortNachGeeignesterApfel fehlendesGewicht restÄpfel''
                           in  Packung kleinsterApfel größterApfel häufigsterApfel besterApfel : packe restÄpfel'''                                                                       
packe _                    = []

packe2 :: Apfelkiste -> [Packung]
packe2 äpfel@(_:_: _: _: _) = let (häufigsterLeichterApfel:restÄpfel) = sortNachHäufigkeitLeichtZuerst äpfel
                                  (häufigsterSchwersterApfel:restÄpfel') = sortNachHäufigkeitSchwerZuerst restÄpfel
                                  fehlendesGewicht = 4 * durchschnittsGewichtApfel - häufigsterLeichterApfel - häufigsterSchwersterApfel
                                  (besterApfel1:restÄpfel'') = sortNachGeeignesterApfel (fehlendesGewicht/2) restÄpfel'
                                  fehlendesGewicht' = fehlendesGewicht - besterApfel1
                                  (besterApfel2:restÄpfel''') = sortNachGeeignesterApfel fehlendesGewicht' restÄpfel''
                           in  Packung häufigsterLeichterApfel häufigsterSchwersterApfel besterApfel1 besterApfel2 : packe2 restÄpfel'''                                                                       
packe2 _                    = []

packe3 :: Apfelkiste -> [Packung]
packe3 äpfel@(_:_: _: _: _) = let (a1:a2:restÄpfel) = sortNachGeeignesterApfel durchschnittsGewichtApfel äpfel
                                  (a3:restÄpfel')   = nehmeÄhnlichenJeNachGewichtLeichterOderSchwerer a2 restÄpfel
                                  fehlendesGewicht  = 4 * durchschnittsGewichtApfel - a1 - a2 - a3
                                  (a4:restÄpfel'')  = sortNachGeeignesterApfel fehlendesGewicht restÄpfel'
                              in  Packung a1 a2 a3 a4 : packe3 restÄpfel''                                                                       
packe3 _                    = []

packe4 :: Apfelkiste -> [Packung]
packe4 äpfel@(_:_: _: _: _) = let (a1:restÄpfel) = sortNachGeeignesterApfel durchschnittsGewichtApfel äpfel
                                  (a2:restÄpfel')   = nehmeÄhnlichenJeNachGewichtLeichterOderSchwerer a1 restÄpfel
                                  (a3:restÄpfel'')   = nehmeÄhnlichenJeNachGewichtLeichterOderSchwerer a2 restÄpfel'
                                  fehlendesGewicht  = 4 * durchschnittsGewichtApfel - a1 - a2 - a3
                                  (a4:restÄpfel''')  = sortNachGeeignesterApfel fehlendesGewicht restÄpfel''
                              in  Packung a1 a2 a3 a4 : packe4 restÄpfel'''                                                                       
packe4 _                    = []

packe5 :: Apfelkiste -> [Packung]
packe5 äpfel@(_:_: _: _: _) = let (kleinsterApfel:restÄpfel) = sort äpfel
                                  (größterApfel:restÄpfel') = reverse restÄpfel
                                  (a3:restÄpfel'')   = sortNachGeeignesterApfel durchschnittsGewichtApfel restÄpfel'
                                  fehlendesGewicht = 4 * durchschnittsGewichtApfel - kleinsterApfel - größterApfel - a3
                                  (besterApfel:restÄpfel''') = sortNachGeeignesterApfel fehlendesGewicht restÄpfel''
                              in  Packung kleinsterApfel größterApfel a3 besterApfel : packe5 restÄpfel'''                                                                       
packe5 _                    = []

--packeBF :: Apfelkiste -> (Float,[Packung])
packeBF = foldl' iter (50, []) . permutations
                 where iter (w,packung) p | w' < w = (w', apfelKisteEinpacken p)
                                          | otherwise = (w,packung)
                                            where w' = durchschnittlicheAbweichung $ apfelKisteEinpacken p

packe6 :: Apfelkiste -> [Packung]
packe6 äpfel@(_:_: _: _: _) = let (kleinsterApfel:restÄpfel) = sort äpfel
                                  (größterApfel:restÄpfel') = reverse restÄpfel
                                  (_, _, a3, a4) = bestPaarFit kleinsterApfel größterApfel $ zweiAusKiste restÄpfel'
                                  restÄpfel'' = delete a3 $ delete a4 restÄpfel'
                              in  Packung kleinsterApfel größterApfel a3 a4 : packe6 restÄpfel''                                                                       
packe6 _                    = []


summeAbweichung :: [Packung] -> Float
summeAbweichung (Packung a1 a2 a3 a4:rest) = abs (a1 + a2 + a3 + a4 - 4 * durchschnittsGewichtApfel) + summeAbweichung rest
summeAbweichung _ = 0

durchschnittlicheAbweichung :: [Packung] -> Float
durchschnittlicheAbweichung packungen = summeAbweichung packungen / fromIntegral (length packungen)