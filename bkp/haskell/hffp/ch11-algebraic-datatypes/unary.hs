{-# LANGUAGE NoMonomorphismRestriction #-}
{-# ANN module "Hlint: ignore Use newtype instead of data" #-}

data Thing = Foo | Bar deriving Show

tf :: Thing
tf = Foo

tb :: Thing
tb = Bar


