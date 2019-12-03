import Prelude hiding (Right, Left)
import Data.List

inputLeft = 
    [Right 75,Down 30,Right 83,Up 83,Left 12,Down 49,Right 71,Up 7,Left 72]
inputRight  =     
    [Up 62,Right 66,Up 55,Right 34,Down 71,Right 55,Down 58, Right 83]

data Direction = Up Int | Down Int | Right Int | Left Int

type Point = (Int,Int)

type Segment = (Point, Point)

type Wire = [Segment]

intersectSegment :: Segment -> Segment -> [Point]
intersectSegment ((lx1,ly1), (rx1,ry1)) ((lx2,ly2), (rx2,ry2)) = 
    let 
        pointsLeft = [(x,y) | x<-[lx1..rx1], y<-[ly1..ry1]]
        pointsRight = [(x,y) | x<-[lx2..rx2], y<-[ly2..ry2]]
    in 
        pointsLeft `intersect` pointsRight

intersectWire :: Wire -> Wire -> [Point]
intersectWire wireLeft wireRight = 
    concat [intersectSegment sl sr|sl<-wireLeft, sr<-wireRight]

buildWire :: [Direction] -> Wire
buildWire (dir:dirs) = 
    let 
        createSegment :: Direction -> Point -> Segment
        createSegment (Up amount) (x,y) = ((x,y), (x,(y+amount)))
        createSegment (Down amount) (x,y) = ((x,y), (x,(y-amount)))
        createSegment (Left amount) (x,y) = ((x,y), ((x-amount),y))
        createSegment (Right amount) (x,y) = ((x,y), ((x+amount),y))


        buildSegments :: [Direction] -> [Segment] -> [Segment]
        buildSegments [] pts = reverse pts
        buildSegments (dir:dirs) segments@((pointLeft, pointRight):rest) = buildSegments dirs ((createSegment dir pointRight):segments)
    in
        buildSegments dirs [createSegment dir (0,0)]
