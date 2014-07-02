module Raytracer where
import System.IO
import Algebra
import Objects

data Camera = Camera {cameraPosition :: Vector3, cameraMatrix :: Matrix3, cameraFovX :: Float, cameraFovY :: Float, cameraDist :: Float} deriving(Show)

data Light = Light {lightPosition :: Vector3, lightLuminosity :: Color} deriving(Show)

data Scene = Scene {objects :: [Object], sceneLight :: Light, ambientLight :: Color} deriving(Show)

render :: Scene -> Camera -> Int -> Int -> IO ()
cameraCoordToAbsolute :: Camera -> Vector3 -> Vector3
rayColor :: Scene -> Vector3 -> Vector3 -> Color
rayObjectColor :: Scene -> Object -> Vector3 -> Vector3 -> [Float] -> Int -> Color
nearestIntersection :: Scene -> Vector3 -> Vector3 -> Maybe (Object, Vector3)
lightAttenuation :: Float -> Float
bitFurther :: Vector3 -> Vector3 -> Vector3

epsilon = 0.01
bitFurther point direction = point +. (epsilon *. direction)

nearestIntersection scene point ray =
	let Vector3(x,y,z) = point in
	let getNearest bestInter newInter = case (bestInter, newInter) of
		((Just (o1, inter1)), (o2, Just inter2)) -> 
			let n1 = norm2(point-.inter1); n2 = norm2(point-.inter2) in
			if n1 > n2 then Just (o2, inter2) else Just (o1, inter1)
		(Nothing, (o, Just inter)) -> Just (o,inter)
		((Just (o, inter)), (_, Nothing)) -> Just (o, inter)
		(Nothing, (o, Nothing)) -> Nothing
	in
	foldl
		getNearest
		Nothing
		(map (\o -> (o, intersects o point ray)) (objects scene))

lightAttenuation dist = 1.0 -- dist * dist / 30.0 -- Not used by reflection

clamp :: Float -> Float
clamp x = max (min x 1.0) 0.0
clampColor :: Color -> Color
clampColor (Color (r,g,b)) = Color (clamp r, clamp g, clamp b)

rayObjectColor scene object point incidentRay refractiveIndices recursionLevel =
	if recursionLevel == 6 then Color (0,0,0) else
	let m = material object in
	let n = normal object point in
	-- Reflection
	let reflectionColor =
		if reflection m == 0.0 then Color (0,0,0) else
			let normalComponent = (incidentRay |. n) *. n in
			let tangentComponent = incidentRay -. normalComponent in
			let newRay = tangentComponent -. normalComponent in
			let intersection = nearestIntersection scene (bitFurther point newRay) newRay in
			case intersection of
				Just (newObject, newPoint) -> rayObjectColor scene newObject newPoint newRay refractiveIndices (recursionLevel+1)
				Nothing -> Color (0,0,0)
	in
	-- Diffusion
	let diffusionColor light =
		if diffusion m == 0.0 then Color (0,0,0) else
		let toLightVec = (lightPosition light) -. point in
		let toLight = normalize toLightVec in
		let factor = max ((toLight |. n)) 0 in
		let dist = norm toLightVec in
		let intersection = nearestIntersection scene (bitFurther point toLight) toLight in
		let Vector3(x,y,z) = point in
		let resColor = 
			let Color (r, g, b) = lightLuminosity light in
			let f = factor / (lightAttenuation dist) in
			Color (r*f, g*f, b*f) in
		case intersection of
			Just (_, p) -> if norm(p-.point) < dist then Color(0,0,0) else resColor
			Nothing -> resColor
				
	in
	-- Specular
	let specularColor light =
		if specular m == 0.0 then Color (0,0,0) else
		let toLightVec = (lightPosition light) -. point in
		let toLight = normalize toLightVec in
		let normalComponent = (toLight |. n) in
		let reflectedLight = ((2 * normalComponent) *. n) -. toLight in
		let factor = (max (-(reflectedLight |. incidentRay)) 0) ** 6 in
		let dist = norm toLightVec in
		let intersection = nearestIntersection scene (bitFurther point toLight) toLight in
		case intersection of -- TODO: fix same as diffusion
			Just (_, _) -> Color (0,0,0)
			Nothing -> 
				let Color (r, g, b) = lightLuminosity light in
				let f = factor / (lightAttenuation dist) in
				Color (r*f, g*f, b*f)
	in
	-- Transmission
	let transmittedColor =
		if transmission m == 0.0 then Color (0,0,0) else
		let tangentVec = normalize $ (incidentRay ^. n) ^. n in
		let tangentComponent = (incidentRay |. tangentVec) in
		let normalComponent = (incidentRay |. n) in
		let (ri1, ri2, newRefractiveIndices) =
			if normalComponent <= 0 then -- We are entering the object
				let ri2 = refractiveIndex m in
				case refractiveIndices of
					(n:ns) -> (n, ri2, ri2:refractiveIndices)
					[] -> (1.0, ri2, ri2:refractiveIndices)
			else
				case refractiveIndices of
					(n1:n2:ns) -> (n1, n2, ns)
					(n1:[]) -> (n1, 1.0, [])
					[] -> (1.0, 1.0, [])
		in
		let newTangentComponent = (ri1 / ri2) * tangentComponent in
		if abs newTangentComponent > 1 then Color(0,0,0) else
		let newIncidentRay = ((sqrt(1 - newTangentComponent ** 2) * (if normalComponent > 0 then 1 else -1)) *. n) +. (newTangentComponent *. tangentVec) in
		let intersection = nearestIntersection scene (bitFurther point newIncidentRay) newIncidentRay in
		case intersection of
			Just (newObject, newPoint) -> rayObjectColor scene newObject newPoint newIncidentRay newRefractiveIndices (recursionLevel+1)
			Nothing -> Color (0,0,0)
		in
	let dc = diffusionColor $ sceneLight scene in
	let sc = specularColor $ sceneLight scene in
	let tc = transmittedColor in
	let r = reflection m; d = diffusion m; s = specular m; t = transmission m; a = ambient m; Color (red, green, blue) = color m in
	let Color (rr, rg, rb) = reflectionColor in
	let Color (dr, dg, db) = dc in
	let Color (sr, sg, sb) = sc in
	let Color (tr, tg, tb) = tc in
	let Color (ar, ag, ab) = ambientLight scene in
	clampColor $ Color (red * (t * tr + s * sr + r * rr + d * dr + a * ar), green * (t * tg +s * sg + r * rg + d * dg + a * ag), blue * (t * tb + s * sb + r * rb + d * db + a * ab))


rayColor scene point incidentRay =
	case (nearestIntersection scene point incidentRay) of
		Just (object, newPoint) -> rayObjectColor scene object newPoint incidentRay [] 0
		Nothing -> Color (0,0,0)

cameraCoordToAbsolute camera v = (mult (cameraMatrix camera) v) +. (cameraPosition camera)

i2f :: Int -> Float
i2f i = fromIntegral i

render scene camera width height =
	let projRectWidth = 2 * (cameraDist camera) * (tan ((cameraFovX camera)/2)) in
	let projRectHeight = 2 * (cameraDist camera) * (tan ((cameraFovY camera)/2)) in
	let fwidth = i2f width; fheight = i2f height in
	let cPoints = [Vector3 (projRectWidth * ((i2f x) / fwidth - 0.5), projRectHeight * ((i2f y) / fheight - 0.5), cameraDist camera) | y <- (reverse [0..(height-1)]), x <- (reverse [0..(width-1)])] in
	let rayOfCPoint cPoint = normalize ((cameraCoordToAbsolute camera cPoint) -. (cameraPosition camera)) in
	let pointOfCPoint cPoint = cameraCoordToAbsolute camera cPoint in
	let stringOfColor (Color (r,g,b)) =
		(show $ round $ 255 * r) ++ " " ++ (show $ round $ 255 * g) ++ " " ++ (show $ round $ 255 * b) ++ " "
	in
	let stringOfCPoint = stringOfColor . (\cPoint -> rayColor scene (pointOfCPoint cPoint) (rayOfCPoint cPoint)) in
	do
		file <- openFile "manger3.ppm" WriteMode
		result <- hPutStr file ("P3 " ++ (show width) ++ " " ++ (show height) ++ " 255 ")
		mapM_ (\p -> hPutStr file $ stringOfCPoint p) cPoints
		hClose file
		return ()
