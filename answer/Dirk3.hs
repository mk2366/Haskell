import Data.Sort (sort, sortBy)
import Data.List (group)

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

sortNachHäufigkeitLeichtZuerst :: Apfelkiste -> Apfelkiste
sortNachHäufigkeitLeichtZuerst ak = concat (sortBy (\l1 l2 -> compare (length l2) (length l1)) (group $ sort ak))

sortNachHäufigkeitSchwerZuerst :: Apfelkiste -> Apfelkiste
sortNachHäufigkeitSchwerZuerst ak = concat (sortBy (\l1 l2 -> compare (length l2) (length l1)) (group $ reverse $ sort ak))

sortNachGeeignesterApfel :: Float -> Apfelkiste -> Apfelkiste
sortNachGeeignesterApfel gewicht = sortBy (\gewicht1 gewicht2 -> compare (abs (gewicht1 - gewicht))(abs (gewicht2 - gewicht)))

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
                           in  Packung häufigsterLeichterApfel häufigsterSchwersterApfel besterApfel1 besterApfel2 : packe restÄpfel'''                                                                       
packe2 _                    = []


summeAbweichung :: [Packung] -> Float
summeAbweichung (Packung a1 a2 a3 a4:rest) = abs (a1 + a2 + a3 + a4 - 4 * durchschnittsGewichtApfel) + summeAbweichung rest
summeAbweichung _ = 0

durchschnittlicheAbweichung :: [Packung] -> Float
durchschnittlicheAbweichung packungen = summeAbweichung packungen / fromIntegral (length packungen)