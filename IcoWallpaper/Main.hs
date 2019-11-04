module Main where

import Graphics.Gloss

main :: IO ()
main = display FullScreen white scene

scene :: Picture
scene = Circle 300

-- Icosahedron points

phi :: Float
phi = (1 + sqrt 5) / 2

icosahedron :: Shape
icosahedron = []

-- Code for camera

data Camera = Camera Int Int Int
data Point = Point Int Int Int
data Edge = Edge Point Point
data Shape = Shape [Edge]
data Scene = [Geometry]

cameraRender :: Camera -> Scene -> Picture
cameraRender = undefined
