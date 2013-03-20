import Control.Applicative ((<*>), (<$>))
import Data.List (intersect, union)
import Data.List.Utils (addToAL)
import Data.Maybe (maybe)

data State = {frontier :: }

type Coordinate = (Int, Int)
type Street = (String, [Coordinate])

parseStreet :: [String] -> Street
parseStreet (x1:y1:street:x2:y2:_) = (street, [(read x1, read y1),(read x2, read y2)])
parseStreet x = error ("Wrong data format: " ++ show x)

unify :: [Street] -> [Street] -> [Street]
unify table []     = table
unify table (x@((street,corners)):xs) = unify newTable xs where
    newTable = maybe (x:table) expand $ lookup street table
    expand oldCorners = addToAL table street (union corners oldCorners)

corner :: [Street] -> String -> String -> Maybe Coordinate
corner [] _ _ = Nothing
corner table a b = fmap head $ intersect <$> aCorners <*> bCorners where
    aCorners = lookup a table
    bCorners = lookup b table

distance :: Coordinate -> Coordinate -> Float
distance (x1, y1) (x2, y2) = sqrt . fromIntegral $ (x1 - x2)^2 + (y1 - y2)^2

main = do
    stuff <- getContents
    table <- return . unify [] . map (parseStreet . words) . filter (not . null) $ lines stuff
    putStrLn . unlines $ map show table
    putStrLn . show $ corner table "Noerregade" "Vestergade"