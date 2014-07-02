module Algebra where

class Vector a where
	(+.) :: a -> a -> a
	(-.) :: a -> a -> a
	(|.) :: a -> a -> Float
	(*.) :: Float -> a -> a
	(/.) :: a -> Float -> a
	neutral :: a

newtype Vector3 = Vector3 (Float, Float, Float) deriving(Show)
newtype Matrix3 = Matrix3 (Vector3, Vector3, Vector3) deriving(Show)

getOp :: (Float -> Float -> Float) -> (Vector3 -> Vector3 -> Vector3)
getOp op (Vector3 (x,y,z)) (Vector3 (u,v,w)) = Vector3 (op x u, op y v, op z w)

norm2 :: Vector3 -> Float
norm2 v = v |. v

norm :: Vector3 -> Float
norm v = sqrt $ norm2 v

normalize :: Vector3 -> Vector3
normalize v = v /. (norm v)

minabs :: Float -> Float -> Float
minabs x y = if abs(x) > abs(y) then y else x

minpos :: Float -> Float -> Maybe Float
minpos x y = 
	if x >= 0 && y >= 0 then Just $ min x y else 
		if x < 0 && y < 0 then Nothing else 
			if x >= 0 && y < 0 then Just x else 
				Just y

mult :: Matrix3 -> Vector3 -> Vector3
mult (Matrix3 (a, b, c)) (Vector3 (x,y,z)) = (x *. a) +. (y *. b) +. (z *. c)

instance Vector Vector3 where
		(+.) a b = getOp (+) a b
		(-.) a b = getOp (-) a b
		(|.) (Vector3 (x,y,z)) (Vector3 (u,v,w)) = x*u+y*v+z*w
		(*.) a (Vector3 (x,y,z)) = Vector3 (a*x, a*y, a*z)
		(/.) (Vector3 (x,y,z)) a = Vector3 (x/a, y/a, z/a)
		neutral = Vector3 (0,0,0)

(^.) :: Vector3 -> Vector3 -> Vector3
(^.) (Vector3 (xa, ya, za)) (Vector3 (xb, yb, zb)) =
	Vector3 (ya*zb-za*yb, xb*za-xa*zb, xa*yb-xb*ya)