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
data Triangle = Triangle {triangleOrigin :: Vector3, triangleUa :: Vector3, triangleUb :: Vector3, triangleUc :: Vector3, triangleA :: Float, triangleB :: Float, triangleC :: Float, triangleNormal :: Vector3, triangleMaterial :: Material} deriving(Show)
data Plane = Plane {planeOrigin :: Vector3, planeNormal :: Vector3, planeMaterial :: Material} deriving(Show)

makeTriangle :: Vector3 -> Vector3 -> Vector3 -> Material -> Triangle
makeTriangle a b c m =
	Triangle a (normalize (b-.a)) (normalize (c-.b)) (normalize (a-.c)) (norm (b-.a)) (norm (c-.b)) (norm (a-.c)) (normalize ((b-.a) ^. (c-.a))) m

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

instance ObjectClass Triangle where
	intersects t point direction =
		let o = triangleOrigin t in
		let n = triangleNormal t in
		let den = (direction |. n) in
		if den == 0.0 then Nothing else
		let distFromPoint = ((o -. point) |. n) / den in
		if distFromPoint < 0 then Nothing else
		let rayProj = point +. (distFromPoint *. direction) in
		let checkEdge orig ua ub = -- Checks if rayProj is "inside" the angle ua <- Orig -> -ub
			let uaOrtho = (ua ^. ub) ^. ua in
			let up = normalize (rayProj -. orig) in
			(uaOrtho |. up) * (-(uaOrtho |. ub)) >= 0 && -(ua |. ub) <= (ua |. up)
		in
		if (checkEdge o (triangleUa t) (triangleUc t)) && (checkEdge (o +. ((triangleB t) *. (triangleUa t))) (triangleUb t) (triangleUa t))
		then
			Just rayProj
		else
			Nothing
	normal t point = triangleNormal t
	material t = triangleMaterial t

instance ObjectClass Plane where
	intersects p point direction =
		let o = planeOrigin p in
		let n = planeNormal p in
		let den = (direction |. n) in
		if den == 0.0 then Nothing else
		let distFromPoint = ((o -. point) |. n) / den in
		if distFromPoint < 0 then Nothing else
		let rayProj = point +. (distFromPoint *. direction) in
		let Vector3 (x,y,z) = rayProj in
		Just rayProj
	normal t point = planeNormal t
	material t = planeMaterial t	