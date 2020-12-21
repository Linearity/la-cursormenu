module Main where

import Control.Monad.Identity
import Data.Binary
import FRP.BearRiver hiding (first)
import Lightarrow
import Optics
import Test.Hspec

import Lightarrow.CursorMenu
import Data.List

main :: IO ()
main = hspec specListMenu
            
specListMenu :: Spec
specListMenu
        = describe "listMenu"
            (it "returns third option after three downs, one up, and one confirm"
                (result `shouldBe` [-1, -1, -1, -1, -1, -1, -1, -1, -1, 3]))
    where   result      = take 10 (runIdentity embedding)
            embedding   = embedSF sf (zip dts commands)
            sf          = runTask menu constant
            menu        = listMenu [1, 2, 3] selectListBounded output 0
            output      = constant (-1)
            dts         = repeat 0.016
            commands :: [Event Message]
            commands    = [ NoEvent,
                            menuDown,
                            NoEvent,
                            menuDown,
                            NoEvent,
                            menuDown,
                            NoEvent,
                            menuUp,
                            NoEvent,
                            menuConfirm ] ++ repeat NoEvent

data Message = Confirm | Down | Up
instance MenuSign Message where
    menuConfirm                 = Confirm
    reactMenuConfirm b Confirm  = Event b
    reactMenuConfirm _b _       = NoEvent
    menuDown                    = Down
    reactMenuDown b Down        = Event b
    reactMenuDown _b _          = NoEvent
    menuUp                      = Up
    reactMenuUp b Up            = Event b
    reactMenuUp _b _            = NoEvent