module SmartHouse.Algorithm.Ray where

import Math.Line (Line, line)
import Three.Math.Vector (Vector3)

type Ray = Line Vector3

ray :: Vector3 -> Vector3 -> Ray
ray = line
