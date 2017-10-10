module Main where
--| hiding add function because Mathbox.Mathbox because has add fuction conflict erasies
import Data.Maybe (Maybe(..))
import Mathbox.Field (Field(..))
import Mathbox.Mathbox (MathboxPrimitive(..), add, applyOnThree, colorWhite, mkMathbox, orbitControls, setThreeClearColor, toJs)

import Data.Foreign.Class (encode)
import Data.List (fromFoldable)

import Mathbox.Classes as C

import Mathbox.Types as T
import Prelude hiding (add)
import Prim as P

-- | Write is in Data.Foreign.Class
matrixData = encode [
  [
    [-1, -1,  -1],
    [-1, -1,  -1],
    [ 1, -1,  -1],
    [ 1, -1,  -1]],
  [
    [-1, -1,  -1],
    [-1, -1,  1],
    [ 1, -1,  1],
    [ 1, -1,  -1]]
]

mathbox :: MathboxPrimitive
mathbox =
  cartesian [
    cam,
    matrix,
    cube
  ]

cartesian :: Array MathboxPrimitive -> MathboxPrimitive
cartesian nested =
    (Cartesian $ C.mkCartesian {
      range = Val [T.mkVec2 (-2) 2, T.mkVec2 (-2) 2, T.mkVec2 (-2) 2],
      scale = Val (T.mkVec3 1 1 1)
    }) ( fromFoldable nested )

cam :: MathboxPrimitive
cam = Camera $ C.mkCamera { proxy = Val true, position = Just $ Val $ T.mkVec3 2 3 3 }

matrix :: MathboxPrimitive
matrix = Matrix $ C.mkMatrix { data = Just $ Val matrixData, channels = Val 3 }

--| side function takes 2 params i.e (Color => String , Opacity => Number)
side :: String -> Number -> MathboxPrimitive
--| Surface is a Constructor mkSurface has color,opacity variables
side color opacity = Surface $ C.mkSurface { color = Val $ T.unsafeMkColor color }

transform :: T.Vec3 -> Array MathboxPrimitive -> MathboxPrimitive
transform v3 nested = (Transform3 $ C.mkTransform3 { position = Val $ v3 }) ( fromFoldable nested )

dataset :: Array MathboxPrimitive -> MathboxPrimitive
dataset nested = (Group $ C.mkGroup { active = Val true }) ( fromFoldable nested )

swizzle :: Array Int -> MathboxPrimitive
swizzle s = Swizzle $ C.mkSwizzle { order = Val $ T.mkSwizzle1 s }
setside :: MathboxPrimitive
setside = side "yellow" 0.80
cube ::  MathboxPrimitive
cube  =
        dataset [
                setside ,
                transform (T.mkVec3 0 2 0)
                [
                  setside,
                  transform (T.mkVec3 0 (-2) 0)
                  [
                    swizzle [2, 3, 1],
                    setside,
                    transform (T.mkVec3 2 0 0)
                    [
                      setside,
                      transform (T.mkVec3 (-2) 0 0)
                      [
                        swizzle [3, 2, 1],
                        setside,
                        transform (T.mkVec3 0 0 2)
                        [
                          setside
                        ]
                      ]
                    ]
                  ]
                ]
              ]




main = do
  mkMathbox { plugins: ["core", "controls"]
            , controls: { klass: orbitControls }
            } >>=
              applyOnThree (setThreeClearColor colorWhite 1.0) >>=

  add (toJs mathbox)
