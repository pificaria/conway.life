{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
module Main where

import Data.List (foldl')
import qualified Data.Vector.Unboxed as V
import GHC.Exts (Constraint)
import System.Posix.Unistd (usleep)
import System.Random (randomRs, getStdGen)
    
-- | Given we want to work with Unboxed vectors, we have to restrict ourselves
-- to objects 'a' of Hask where @Unbox a@ is obeyed. Therefore we'll define
-- subcategories of Hask by means of a constraint
--  @type SubCat f a :: Constraint@ 
--
-- Note that 'ExoComonad' doesn't really need to be a comonad, given it doesn't
-- need to be an endofunctor of our subcategory. But we can still require its
-- compliance to the Comonad laws for extract and extend:
--  @extend extract      = id 
--   extract . extend f  = f  
--   extend f . extend g = extend (f . extend g)@
--
-- See that we can't require @duplicate = extend id@, for we don't know if 'w' is
-- defined at 'w a'.
class ExoFunctor f where
    type SubCat f a :: Constraint
    emap :: (SubCat f a, SubCat f b) => (a -> b) -> f a -> f b

class ExoFunctor w => ExoComonad w where
    extract :: SubCat w a => w a -> a
    extend  :: (SubCat w a, SubCat w b) => (w a -> b) -> w a -> w b

class ExoComonad w => Store s w | w -> s where
    pos  :: SubCat w a => w a -> s
    peek :: SubCat w a => s -> w a -> a
    experiment :: (SubCat w a, Functor f) => (s -> f s) -> w a -> f a
    experiment f w = fmap (`peek` w) (f (pos w))
              
-- | The static information of a cellular automata is given by a type of states
-- 's', a space 'a', and a morphism (the general state) @a -> s@. That is, we
-- assign from each point 'a' a state from 's'. Furthermore there is the rule
-- @(a -> s) -> (a -> s)@ sending a general state to another one.
--
-- Now, we want our cellular automata to need only local information to compute
-- the state at a given point of 'a'. Thinking of @f :: a -> s@ as a generalized
-- element of 's' with stage 'a', we want to compute the new state of @s_a = f
-- a@ by means only of 'f' and 'a'. That is, given @((a -> s), a) -> s@, we want
-- to extend it to @((a -> s), a) -> ((a -> s), a)@, and this is given by the
-- comonadic structure of the 'Store' comonad: @Store a s ~ ((a -> s), a)@.
--
-- Therefore we generalize the 'Store' comonad to a typeclass (constrained to be
-- a comonad) whose methods give exactly what we want from it: the possibility
-- of getting the state 's' only from an index 'a', and the position of a chosen
-- state.
data P a = P {-# UNPACK #-} !Int
             {-# UNPACK #-} !(V.Vector a)

instance ExoFunctor V.Vector where
    type SubCat V.Vector a = V.Unbox a
    emap = V.map

instance ExoFunctor P where
    type SubCat P a = V.Unbox a
    emap f (P i v) = P i (emap f v)

instance ExoComonad P where
    extract (P i a)  = a V.! i
    extend f (P i a) = P i $ V.imap (\i _ -> f (P i a)) a

instance Store Int P where
    pos (P i _)    = i
    peek i (P _ a) = a V.! i
    experiment f (P i a) = fmap (a V.!) (f i)

-- | Right action of a type 'm' on a type 'a'.
type Action m a = a -> m -> a

-- | Given a type 's' of states and 'm' a group of translations, our rule is a
-- map @(m -> s) -> s@. That is, it'll receive a way to traverse 's' by means of
-- translations by 'm', and calculate the new state.
type Rule m s = (m -> s) -> s

-- | Now, the core element. Given an index type 'i', we'll quotient it by means
-- of a group of translations 'm' acting on 'i'. Now, given our Store with index
-- and state types 'i' and 's', respectively, and with a position @p :: i@ and
-- indexer @phi :: i -> s@, we can get a reparametrization @m -> s@ giving us
-- the states of translations by 'p'. So, if we have a rule @(m -> s) -> s@ we
-- can compute the new state indexed by 'i'.
step :: (SubCat w s, Store i w) => Action m i -> Rule m s -> w s -> s
step a r = r . experiment a

-- | For Conway's Game of Life, we define two different quotients of the integer
-- plane: the torus and the klein bottle.
type Plane = (Int, Int)

fromPlane :: Int -> Plane -> Int
fromPlane n (i, j) = i*n + j
{-# INLINE fromPlane #-}
                     
torus :: Int -> Int -> Action Plane Int
torus m n a (i, j) = let (p, q)  = divMod a n
                         p'      = (mod (i + p) m, mod (j + q) n) in fromPlane n p'

range :: Plane -> Plane -> [Plane]
range (i, j) (m, n) = [(p, q) | p <- [i .. m - 1], q <- [j .. n - 1]]
{-# INLINE range #-}
                      
-- | The rule of Game of Life is: a cell will stay or become alive in the next
-- state if and only if either it is alive and has two or three alive
-- neighbours, or it is dead and has exactly three alive neighbours.
life :: Rule Plane Bool
life phi = let nhood = [(i, j) | i <- [-1..1], j <- [-1..1], (i, j) /= (0, 0)]
               s = phi (0, 0)
               n = foldl' ((. (fromEnum . phi)) . (+)) 0 nhood in
           (s && (n == 2 || n == 3)) || (not s && n == 3)

-- | The machinery to generate a random grid and print the current state, along
-- with our main loop.
randomGrid :: Int -> IO (V.Vector Bool)
randomGrid r = fmap (V.fromListN r . map toEnum . randomRs (0, 1)) getStdGen

showPlane :: (SubCat w Bool, Store Int w) => Plane -> w Bool -> String
showPlane r@(_, n) w = let phi b        = if b then "· " else "  "
                           psi p@(_, j) = let i = fromPlane n p in
                                          if j == 0 then '\n' : phi (peek i w) else phi (peek i w) in
                       psi =<< range (0, 0) r

main :: IO ()
main = randomGrid (m * n) >>= mapM_ psi . timeline . P 0 where
    b@(m, n)    = (50, 80)
    gen n       = show n ++ "-th generation\n"
    phi         = step (torus m n) life
    clear       = "\x1b[2J\x1b[0;0f\x1b[0;0f"
    psi (i, !s) = putStr (clear ++ gen i ++ showPlane b s) >> usleep 300000
    timeline    = zip [0..] . iterate (extend phi)
