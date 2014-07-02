import System.IO
import Raytracer
import Objects
import Algebra

s1 = Sphere (Vector3 (-2.5,0.0,0.0)) 2.0 (Material 1.0 0.3 0.0 0.6 0.05 1.0 (Color (1.0, 1.0, 0.2)))
s2 = Sphere (Vector3 (2.5,0.0,0.0)) 2.0 (Material 1.0 0.3 0.0 0.6 0.8 1.0 (Color (0.2, 1.0, 1.0)))
s3 = Sphere (Vector3 (1.0,3.0,0.0)) 0.5 (Material 1.0 0.3 0.0 0.9 0.05 1.0 (Color (1.0, 1.0, 1.0)))
s4 = Sphere (Vector3 (2.5,0.0,-3.0)) 0.5 (Material 0.0 0.0 1.0 0.0 0.0 1.1 (Color (1.0, 1.0, 1.0)))
s5 = Sphere (Vector3 (2.5,-4.0,0.0)) 2.0 (Material 1.0 0.3 0.0 0.6 0.8 1.0 (Color (0.2, 1.0, 1.0)))
s6 = Sphere (Vector3 (0.0,0.0,-4.0)) 1.0 (Material 0.0 0.0 1.0 0.0 0.0 1.1 (Color (1.0, 1.0, 1.0)))
t = makeTriangle (Vector3 (3.0, -4.0, -1.0)) (Vector3 (-3.0, -4.0, -1.0)) (Vector3 (0.0, -4.0, 6.0)) (Material 1.0 0.3 0.0 0.2 0.4 1.0 (Color (0.3, 1.0, 0.3))) 
p = Plane (Vector3 (0.0, 0.0, 10.0)) (Vector3 (0.0, 0.0, -1.0)) (Material 1.0 0.4 0.0 0.5 0.0 1.0 (Color (1.0, 1.0, 1.0))) 
--p2 = Plane (Vector3 (0.0, -1.0, 0.0)) (Vector3 (0.0, 1.0, 0.0)) (Material 0.0 0.0 1.0 0.0 0.0 1.5 (Color (1.0, 1.0, 1.0))) 
l = Light (Vector3 (0.0, 5.0, -3.0)) (Color (1.0, 1.0, 1.0))
cam = Camera (Vector3 (0.0, 0.0, -7.0)) (Matrix3 (Vector3 (1.0, 0.0, 0.0), Vector3 (0.0, 1.0, 0.0), Vector3(0.0, 0.0, 1.0))) (pi/2) (pi/2) 2.0
scene = Scene [Object s1, Object s2, Object s3, Object t, Object p, Object s4, Object s5, Object s6] l (Color (0.1, 0.1, 0.1))

main :: IO ()
main = do render scene cam 600 600