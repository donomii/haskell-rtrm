import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Graphics.UI.GLUT.Initialization
import Data.SG
import Control.Parallel
import System.Exit
import GHC.Exts (inline)
-- import List
import Data.Time
import Data.Time.Clock.POSIX
import Data.Time.Format
-- import System.Posix.Unistd
-- import Locale
import Text.Printf
--import Control.Monad.Parallel
import Control.Parallel.Strategies
--import Bindings
import Data.IORef
type Scalar = GLfloat
type Point3 = Point3' GLfloat
type Rel2 = Rel2' GLfloat
type Rel3 = Rel3' GLfloat
type Matrix33 = Matrix33' GLfloat
type Vec2 = Pair GLfloat
type Vec3 = Triple GLfloat
type Line2 = LinePair GLfloat
type Line3 = LineTriple GLfloat

radpoints = 32::GLfloat
step = 1
zeroVec = makeRel3 (0,0,0)
eps = 0.1
clipDistance = 5.0
sinTable :: [GLfloat]
cosTable :: [GLfloat]
sinTable = map (\x -> sin (x*pi/180)) $! [0..720]
cosTable = map (\x -> cos ((x-90)*pi/180)) $! [0..720]

data MyShape =  MySphere GLfloat GLfloat GLfloat GLfloat |
		MyCube  GLfloat GLfloat GLfloat GLfloat  |
		MyTerrain  GLfloat GLfloat GLfloat GLfloat  |
		MyOddCube  GLfloat GLfloat GLfloat GLfloat
data IntersectionT = Intersection Point3 Rel3 GLfloat Rel3


negPIon4 :: GLfloat
negPIon4 = (-pi/4.0::GLfloat)

cubeTest :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> Rel3 -> Bool
cubeTest c x' y' z' rad aPoint  =  cubeTest' (rotateYXc (aPoint - makeRel3 (x',y',z')) negPIon4 (6*c*pi/180::GLfloat)) rad

cubeTest' :: (GLfloat, GLfloat, GLfloat) -> GLfloat  -> Bool
cubeTest' (x, y, z) rad = (4*x*x<rad) && (4*y*y<rad) && (4*z*z<rad)

sphereTest :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> Rel3 -> Bool
sphereTest c x' y' z' rad aPoint =  sphereTest' ((rotateYX aPoint  0 (6*c*pi/180::GLfloat) ) - makeRel3 (x',y',z')) rad


sphereTest' :: Rel3 -> GLfloat ->  Bool
sphereTest' (Rel3 (x,y,z) d) rad  =   x*x + y*y + z*z < rad



objects :: [MyShape]
objects =  map ( \x -> (MyCube (2*x) 0.0 0.0 1)) [-5 .. 5]  ++  map ( \x -> (MySphere 0.0 x 0.0 1)) [-5 .. 5] ++ map ( \x -> (MySphere 0.0 0.0 x 1)) [-5 .. 5] -- ++ map ( \x -> (MySphere (-2) (-2) (-2)  1)) [1..1]

scenecheck :: GLfloat -> Rel3 -> Rel3 -> Bool
scenecheck c aVec dir =
   cubeTest   c (-2) 0.0 0.0 2 aVec ||
     cubeTest   c (-2) (-2.0) 0.0 2 aVec ||
       cubeTest   c 2 0.0 0.0 2 aVec ||
	 cubeTest   c 2 (-2.0) 0.0 2 aVec ||
	   sphereTest c 0.0   2   0.0 0.5 aVec ||
	     sphereTest c 0.0   1   0.0 0.5 aVec ||
	       sphereTest c 0.0   3   0.0 0.5 aVec




--myPoints :: [(GLfloat,GLfloat,GLfloat)]
myRow  :: GLfloat -> [(GLfloat,GLfloat,GLfloat)]
--myPoints = map (\k -> (k/100,k/100,-5.0)) [-100..100]
myRow j = map (\k -> (k/radpoints,j/radpoints,-5.0)) [-radpoints..radpoints]
myCol  =  map (\k -> myRow k) [-radpoints..radpoints]
allPoints = concat myCol

throwRay:: GLfloat -> Rel3->Rel3->Point3 -> Rel3 -> IntersectionT
throwRay c up right origin direction  =  marchRay c up right  origin direction  False 0

col2arr (Color3 r g b) = [r,g,b]
calculate_colour :: GLfloat -> GLfloat ->Rel3->Rel3->Rel3 ->GLfloat -> GLfloat ->GLfloat  ->Point3-> GLfloat -> (GLfloat, GLfloat, GLfloat, GLfloat, GLfloat, GLfloat, GLfloat)
calculate_colour c ang up right position displayx displayy displayz aLight travelled =
  let intersec = marchRay c up right  (relToPoint position)   (normalise (createRay ang displayx  displayy  displayz)) True 0
      in let (r,g,b) = determine_colour intersec aLight
  in ( r, g, b, displayx, displayy, displayz , (getTravelledFromIntersect intersec))


pointToRel (Point3 (x,y,z)) = makeRel3 (x, y, z)
relToPoint (Rel3 (x, y, z) d) = Point3 (x,y,z)

determine_colour (Intersection point dir trav normal)  aLight =
  	    if trav>clipDistance
	      then (0,0,0)
	      else
	      let lightDir = normalise ((pointToRel aLight) - (pointToRel point))
		  in if dotProduct normal lightDir>0
			    then  let incident = (dotProduct normal lightDir)
			      in (incident, incident, incident)
			    else (0, 0, 0)
	      --else normalColour  (normalise normal)
	      --else colour_by_distance aLight point
--calculate_colour x y z aLight = marchRay 0 (Point3 (x, y, z)) (makeRel3 (0.0,0.0,1.0))  aLight
--if x*x + y*y < 0.5*0.5
	--		    then Color3 1 1 1
	--		    else Color3 x y z

--dotProd (Rel3 (x, y, z) d) (Rel3 (x', y', z') d') = makeRel3 (x*x', y*y', z*z')
cross   (Rel3 (x, y, z) d) (Rel3 (x', y', z') d') = makeRel3 (y*z-z*y', z*x'-x*z', x*y'-y*x')
absVec (Rel3 (x, y, z) d) = makeRel3 ((abs x), abs(y), abs(z))
normalise (Rel3 (x,y,z) d) = let mag = sqrt(x*x + y*y +z*z) in makeRel3(x/mag, y/mag, z/mag)

interpolate_normal c up right (Point3 (x,y,z)) direction =
  let hitpoint = makeRel3 (x,y,z)
      qIntersect = throwRay c up right (relToPoint ((hitpoint + scaleRel eps up) - direction))  (scaleRel 0.5 direction)
      rIntersect = throwRay c up right (relToPoint ((hitpoint + scaleRel eps right) - direction)) (scaleRel 0.5 direction)
      q = getPointFromIntersect qIntersect
      r = getPointFromIntersect rIntersect
      xvec = makeRel3 ( x - (getX q), y - (getY q), z - (getZ q))
      zvec = makeRel3 ( x - (getX r), y - (getY r), z - (getZ r))
	in normalise (cross xvec zvec)

getPointFromIntersect (Intersection p3 _ _ _) = p3
getTravelledFromIntersect (Intersection _ _ travelled _) = travelled

rotateY (Rel3 (x, y, z) d) ang =  rY x y z (sin ang) (cos ang)

rY x y z s c = makeRel3 (z*s + x*c, y, z*c - x*s)

rotateVY x y z ang =  (z*sin(ang) + x*cos(ang), y, z*cos(ang) - x*sin(ang))
rotateVXY aVec angx angy =  rotateY (rotateX aVec angx) angy



rotateX (Rel3 (x, y, z) d) ang = rX x y z (sin ang) (cos ang)
rX x y z s c = makeRel3 (x, y*c - z*s, y*s + z*c)

rotateYX :: Rel3 -> GLfloat -> GLfloat ->Rel3
rotateYX (Rel3 (x, y, z) d) angY angX = rYX  x y z (mFastSin angY) (mFastCos angY) (mFastSin angX) (mFastCos angX)
rotateYXc :: Rel3 -> GLfloat -> GLfloat ->(GLfloat, GLfloat, GLfloat)
rotateYXc (Rel3 (x, y, z) d) angY angX = rYXc  x y z (mFastSin angY) (mFastCos angY) (mFastSin angX) (mFastCos angX)
rYX :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat ->Rel3
rYX x y z sY cY sX cX = makeRel3 (z*sY + x*cY, y*cX - (z*cY - x*sY)*sX, y*sX + (z*cY - x*sY)*cX)
rYXc :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat ->(GLfloat,GLfloat,GLfloat)
rYXc x y z sY cY sX cX = (z*sY + x*cY, y*cX -z*cY*sX + x*sY*sX, y*sX + z*cY*cX - x*sY*cX)

b = (4.0::GLfloat)/pi
c = (-4::GLfloat)/pi/pi
ninetyDeg = pi/(2::GLfloat)
oneEighty = pi
twoSeventy = oneEighty+ninetyDeg
threeSixty = 2*pi

modularIndex :: GLfloat -> Int
modularIndex x = 360+ (rem (round (x*180/pi)) 360)
mFastCos x = mFastSin (x-ninetyDeg)
mFastSin :: GLfloat -> GLfloat
mFastSin x = if x>threeSixty
		     then mFastSin (x-threeSixty)
		     else if x>twoSeventy
				    then (-mFastSin (x-oneEighty))
				    else if x>oneEighty
						    then (-mFastSin (x-oneEighty))
						    else if x>ninetyDeg
								      then fastSin (oneEighty-x)
								      else fastSin x
tableSin x = sinTable !! (modularIndex x)
tableCos x = cosTable !! (modularIndex x)
fastSin x =b * x + c * x * (abs x)
fastCos x = fastSin (x-ninetyDeg)

createOrigin x y z position   = plusDir (Point3 (x, y, z)) position
--createOrigin x y z position   = plusDir (Point3 (x, y, z)) position
createRay ang x y z = rotateX (rotateY (fromPt (Point3 (x,y,0)) (Point3 (0,0,-1))) ang) 0



normalColour (Rel3 (x, y, z) d) = Color3 x y z
skyColour = Color3 0 0 0



colour_by_distance aLight aPoint = let m = 10/(magSq (fromPt aPoint  aLight))
    in Color3 m m m


findHalfWay :: Rel3 -> Rel3 -> Rel3
findHalfWay a b =  a + (scaleRel 0.5  (b - a))

bisection :: GLfloat -> Rel3 -> Rel3 -> Int -> [MyShape] -> Rel3
bisection c outside inside count candidates =
  --chatter("bifurcation("+a+", "+b+", "+count+");");
      if count>0
		then let h = findHalfWay outside inside
			in if scenecheck c h (inside - outside)--isHit c h (inside - outside) candidates
			  then bisection c outside h (count-1) candidates
			  else bisection c h inside (count-1) candidates
		else findHalfWay outside inside



marchRay :: GLfloat->Rel3  ->Rel3   -> Point3 -> Rel3  -> Bool ->GLfloat -> IntersectionT
marchRay c up right  origin direction recurse travelled =
  --let point = plusDir origin (scaleRel travelled direction)
  let point = plusDir origin $! (scaleRel travelled direction)
      in if travelled > clipDistance
		  then Intersection point direction travelled zeroVec
		  else if scenecheck c (pointToRel point) direction --isHit c (pointToRel point) direction objects
			      then if recurse
						then let accurateHit = (bisection  c ((pointToRel point) - (scaleRel step direction)) (pointToRel point) 10 objects)
						  in Intersection (relToPoint accurateHit) direction travelled (interpolate_normal c up right (relToPoint accurateHit) direction)
						else Intersection (relToPoint (bisection  c ((pointToRel point) - (scaleRel step direction)) (pointToRel point) 10 objects)) direction travelled (makeRel3 (0,0,0))
			      else marchRay  c up right  origin direction recurse $! (travelled+step)



--xxx (MyShape x y z rad) (MyShape x' y' z' rad')  = if (x*x + y*y +z*z)<(x'*x' + y'*y' + z'*z') then  LT else GT





--renderPoint ang position xx yy zz displayx displayy displayz = do
 -- color $ (calculate_colour ang position displayx displayy displayz (Point3 (xx,yy,zz)))
  --vertex$ Vertex3 displayx displayy 0

renderACol r g b displayx displayy displayz = do
  color  $ Color3 r g b
  vertex $ Vertex3 displayx displayy 0
  vertex $ Vertex3 (displayx + (1/radpoints)) displayy 0
  vertex $ Vertex3 (displayx + (1/radpoints)) (displayy + (1/radpoints)) 0
  vertex $ Vertex3 displayx (displayy + (1/radpoints)) 0

  --vertex $ Vertex3 (displayx + ( 1/ (2*radpoints))) displayy 0
calculateCol c ang up right xx yy zz displayx displayy displayz travelled =
  (calculate_colour c ang up right (fromPt (Point3 (0,0,0)) (Point3 (xx,yy,zz))) displayx displayy displayz (Point3 (-2,0,-2)) travelled)

calculateCol2 c ang up right xx yy zz (displayx, displayy, displayz) travelled =
  (calculate_colour c ang up right (fromPt (Point3 (0,0,0)) (Point3 (xx,yy,zz))) displayx displayy displayz (Point3 (-2,0,-2)) travelled)

calculateRow c ang up right xx yy zz y z =
  (parMap rwhnf)(\(displayx, displayy, displayz)->(calculateCol c ang up right xx yy zz displayx displayy displayz 0) ) $! (myRow y)




renderAllPoints c ang up right xx yy zz =
  --let allCols = (parMap rwhnf) (\(displayx, displayy, displayz)->(calculateCol ang up right xx yy zz displayx displayy displayz) ) allPoints
  let allCols = concat $! (parMap rwhnf) (\displayy->(calculateRow c ang up right xx yy zz displayy (-5.0::GLfloat)) ) [-radpoints .. radpoints]
      in do renderPrimitive Quads $ mapM_ ( \(r,g,b, displayx, displayy, displayz, trav)->renderACol  r g b displayx displayy displayz )  allCols




reshape s@(Size w h) = do  viewport $= (Position 0 0, s)







tripleToRel x y z = makeRel3 (x,y,z)

display clock angle position up right = do
  clear [ColorBuffer]
  loadIdentity
  (xx,yy,zz) <- get position
  --translate $ Vector3 x y 0
  preservingMatrix $ do
    a <- get angle
    c<- get clock
    clock $= c+1
    (ux, uy, uz) <- get up
    (rx, ry, rz) <- get right
    (xpos,ypos,zpos) <- get position
    --rotate a $ Vector3 0 0 (1::GLfloat)
    scale 0.7 0.7 (0.7::GLfloat)
    --renderPrimitive Points $ mapM_ (\(displayx, displayy, displayz)->renderPoint a (makeRel3 (xpos,ypos,zpos))  xx yy zz displayx displayy displayz ) allPoints
    --pointSize $= 5
    --pointSmooth$= Enabled
    start <- getCurrentTime
    renderAllPoints c a (makeRel3 (ux,uy,uz)) (makeRel3 (rx,ry,rz))  xx yy zz
    finish <- getCurrentTime
    let elapsed = diffUTCTime finish start
	in putStrLn $ printf "%f" ((fromRational (toRational elapsed))::Double)
    swapBuffers

idle angle delta = do
  --a <- get angle
  --d <- get delta
  --angle $= a+d
  postRedisplay Nothing



main = do
  (progname,_) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  createWindow "Hello World"
  reshapeCallback $= Just reshape
  angle <- newIORef (0.1::GLfloat)
  delta <- newIORef (0.1::GLfloat)
  position <- newIORef (1.0::GLfloat, 1.0, 2.5)
  direction <- newIORef (0.0::GLfloat, 0.0, 0.20::GLfloat)
  right <- newIORef (0.20::GLfloat, 0.0, 0.0)
  up <- newIORef (0.0::GLfloat, 0.20::GLfloat, 0.0)
  clock <- newIORef (1.0::GLfloat)
  idleCallback $= Just (idle angle delta)
  displayCallback $= (display clock angle position up right)
  mainLoop
