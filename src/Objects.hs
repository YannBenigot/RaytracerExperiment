module Objects where

import Algebra

class (Show a) => ObjectClass a where
	intersects :: a -> Vector3 -> Vector3 -> Maybe Vector3
	normal :: a -> Vector3 -> Vector3
	material :: a -> Material

data Object = forall a. ObjectClass a => Object a

instance Show Object where
	show (Object a) = show a

instance ObjectClass Object where
	intersects (Object a) o d = intersects a o d
	normal (Object a) p = normal a p
	material (Object a) = material a

newtype Color = Color (Float, Float, Float) deriving(Show)
data Material = Material {reflection :: Float, transmission :: Float, diffusion :: Float, specular :: Float, color :: Color} deriving(Show)
data Sphere = Sphere {sphereCenter :: Vector3, sphereRadius :: Float, sphereMaterial :: Material} deriving(Show)

instance ObjectClass Sphere where
	intersects s origin direction =
		let k = direction |. (((sphereCenter s)) -. origin) in
		if k >= 0
		then
			let distance2 = norm2 (origin +. (k *. direction) -. (sphereCenter s)) in
			if distance2 < (sphereRadius s) * (sphereRadius s)
			then
				let nearestToInterDist = sqrt ((sphereRadius s) ** 2 - distance2) in
				let l1 = k - nearestToInterDist; l2 = k - nearestToInterDist in
				Just (origin +. ((minabs l1 l2) *. direction))
			else
				Nothing
		else
			Nothing
	normal s point = normalize (point -. (sphereCenter s))
	material s = sphereMaterial s