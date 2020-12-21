module Lightarrow.CursorMenu.MenuSign where

import FRP.BearRiver

-- | Types that can act as cues for a menu
class MenuSign a where
    -- | Cursor moving down
    menuDown            :: a
    reactMenuDown       :: b -> a -> Event b
    -- | Cursor moving left
    menuLeft            :: a
    reactMenuLeft       :: b -> a -> Event b
    -- | Cursor moving right
    menuRight           :: a
    reactMenuRight      :: b -> a -> Event b
    -- | Cursor moving up
    menuUp              :: a
    reactMenuUp         :: b -> a -> Event b
    -- | Confirmation of a selection
    menuConfirm         :: a
    reactMenuConfirm    :: b -> a -> Event b

instance MenuSign a => MenuSign (Event a) where
    menuDown                        = Event menuDown
    reactMenuDown b (Event x)       = reactMenuDown b x
    reactMenuDown _b _              = NoEvent
    menuLeft                        = Event menuLeft
    reactMenuLeft b (Event x)       = reactMenuLeft b x
    reactMenuLeft _b _              = NoEvent
    menuRight                       = Event menuRight
    reactMenuRight b (Event x)      = reactMenuRight b x
    reactMenuRight _b _             = NoEvent
    menuUp                          = Event menuUp
    reactMenuUp b (Event x)         = reactMenuUp b x
    reactMenuUp _b _                = NoEvent
    menuConfirm                     = Event menuConfirm
    reactMenuConfirm b (Event x)    = reactMenuConfirm b x
    reactMenuConfirm _b _           = NoEvent