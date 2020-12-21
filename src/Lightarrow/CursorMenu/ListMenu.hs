module Lightarrow.CursorMenu.ListMenu where

import Control.Applicative
import FRP.BearRiver
import Lightarrow

import Lightarrow.CursorMenu.MenuSign

-- | A menu with a list of options.  The cursor moves up and down the list
-- by way of a selector activity that returns the next cursor position.
-- When the menu receives a 'menuConfirm' cue, it returns the currently
-- selected option.
listMenu :: (MenuSign a, Monad m) =>
                [c]                                 -- ^ options
                    -> ((Int, Int)
                            -> Int
                            -> Task a Int m Int)    -- ^ selector
                    -> SF m Int b                   -- ^ output source
                    -> Int                          -- ^ initial cursor position
                    -> Task a b m c                 -- ^ menu activity
listMenu options select output n0
                = mapTask (>>> left output)
                    (onlyUntil (arr confirm >>> notYet)
                        (loop n0))
    where   confirm (a, n)  = reactMenuConfirm (options !! n) a
            loop n          = select (0, length options) n >>= loop

-- | A menu selector that returns a new cursor position when it receives a
-- 'menuDown' or 'menuUp' cue.  When the cursor position is equal to the
-- lower bound, 'menuUp' is ignored; when it is equal to the upper bound,
-- 'menuDown' is ignored.
selectListBounded :: (Monad m, MenuSign a) =>
                        (Int, Int)                  -- ^ bounds
                            -> Int                  -- ^ cursor position
                            -> Task a Int m Int     -- ^ selecting activity
selectListBounded (nMin, nMax) n
                | n <= nMin     = onlyUntil moveDown
                                    (always (constant nMin))
                | n >= nMax     = onlyUntil moveUp
                                    (always (constant nMax))
                | otherwise     = onlyUntil move
                                    (always (constant n))
    where   moveDown    = arr (reactMenuDown (nMin + 1) . fst) >>> notYet
            moveUp      = arr (reactMenuUp (nMax - 1) . fst) >>> notYet
            move        = arr (reactWith [  reactMenuDown (n + 1),
                                            reactMenuUp (n - 1)     ] . fst)
                                >>> notYet

-- | A menu selector that returns a new cursor position when it receives a
-- 'menuDown' or 'menuUp' cue.  The cursor wraps around to the lower bound
-- when it would otherwise increase past the upper bound, and vice-versa.
selectListWrap :: (Monad m, MenuSign a) =>
                    (Int, Int)                      -- ^ bounds
                        -> Int                      -- ^ cursor position
                        -> Task a Int m Int         -- ^ selecting activity
selectListWrap (nMin, nMax) n = onlyUntil move
                                    (always (constant n))
    where   move        = arr (reactWith reactions . fst) >>> notYet
            reactions   = [ reactMenuDown (nMin + ((n + 1 - nMin) `mod` diff)),
                            reactMenuUp (nMin + ((n - 1 - nMin) `mod` diff))    ]
            diff        = nMax - nMin