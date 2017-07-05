||| An Enumerated Type to represent Direction
data Direction = North | East | South | West

turnClockwise : Direction -> Direction
turnClockwise North = East
turnClockwise East = South
turnClockwise South = West
turnClockwise West = North


||| A Union Type to represent shapes
data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

%name Shape shape, shape1, shape2

area : Shape -> Double
area (Triangle base height) = base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

||| A Recursive Type to define a Picture
data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture

%name Picture pic, pic1, pic2


rectangle : Picture
rectangle = Primitive $ Rectangle 20 10

circle : Picture
circle = Primitive $ Circle 5

triangle : Picture
triangle = Primitive $ Triangle 10 10

triangle1 : Picture
triangle1 = Primitive $ Triangle 20 20

testPicture : Picture
testPicture = Combine (Translate 5 5 rectangle) $ Combine (Translate 35 5 circle) (Translate 15 25 triangle)

pictureArea : Picture -> Double
pictureArea (Primitive shape) = area shape
pictureArea (Combine pic pic1) = pictureArea pic + pictureArea pic1
pictureArea (Rotate x pic) = pictureArea pic
pictureArea (Translate x y pic) = pictureArea pic

||| A generic type to define the biggest triangle in a picture
data Biggest = NoTriangle | Size Double

biggerTriangle : Biggest -> Biggest -> Biggest
biggerTriangle NoTriangle NoTriangle = NoTriangle
biggerTriangle NoTriangle (Size y) = Size y
biggerTriangle (Size x) NoTriangle = Size x
biggerTriangle (Size x) (Size y) =
  case x > y of
    True => Size x
    False => Size y

biggestTriangle : Picture -> Biggest
biggestTriangle (Primitive (Triangle base height)) = Size $ area (Triangle base height)
biggestTriangle (Primitive (Rectangle _ _)) = NoTriangle
biggestTriangle (Primitive (Circle _)) = NoTriangle
biggestTriangle (Combine pic pic1) =
  let
    biggest0 = biggestTriangle pic
    biggest1 = biggestTriangle pic1
  in
    biggerTriangle biggest0 biggest1
biggestTriangle (Rotate x pic) = biggestTriangle pic
biggestTriangle (Translate x y pic) = biggestTriangle pic

testTriangles : Picture
testTriangles =
  Combine (Translate 5 5 triangle) (Translate 10 5 triangle1)
