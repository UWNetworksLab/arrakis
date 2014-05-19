{- 
   Attr: Mackerel attribute semantics
   
  Part of Mackerel: a strawman device definition DSL for Barrelfish
   
  Copyright (c) 2007, 2008, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
-}  

module Attr where

data Attr =   RO      -- Read Only
            | WO      -- Write Only
            | RC      -- Read Only, Read Clears
            | ROS     -- Read Only Sticky            
            | RW      -- Read Write
            | RWC     -- Read Write (1 to) Clear
            | RWZC    -- Read Write Zero to Clear
            | RWO     -- Write once 
            | RWCS    -- R/W clear sticky
            | RWS     -- Read Write Sticky
            | RWL     -- Read Write locked
            | MBZ     -- Must be Zero
            | MB1     -- Must be one 
            | RSVD    -- Reserved
            | NOATTR  -- Default No attribute
              deriving (Show, Eq)


-- User can reasonably read from this register
attr_user_can_read :: Attr -> Bool
attr_user_can_read WO = False
attr_user_can_read MBZ = False
attr_user_can_read MB1 = False
attr_user_can_read RSVD = False
attr_user_can_read _ = True

-- User can reasonably write to this register
attr_user_can_write :: Attr -> Bool
attr_user_can_write RO = False
attr_user_can_write RC = False
attr_user_can_write ROS = False
attr_user_can_write MBZ = False
attr_user_can_write MB1 = False
attr_user_can_write RSVD = False
attr_user_can_write _ = True

attr_is_writeable :: Attr -> Bool
attr_is_writeable RO = False
attr_is_writeable WO = True
attr_is_writeable RC = False
attr_is_writeable ROS = False
attr_is_writeable RW = True
attr_is_writeable RWC = True
attr_is_writeable RWZC = True
attr_is_writeable RWO = True
attr_is_writeable RWCS= True
attr_is_writeable RWS = True
attr_is_writeable RWL = True
attr_is_writeable MBZ = False
attr_is_writeable MB1 = False
attr_is_writeable RSVD = False
attr_is_writeable _  = False

attr_is_readable :: Attr -> Bool
attr_is_readable RO = True
attr_is_readable WO = False
attr_is_readable RC = True
attr_is_readable ROS = True
attr_is_readable RW = True
attr_is_readable RWC = True
attr_is_readable RWZC = True
attr_is_readable RWO = True
attr_is_readable RWCS= True
attr_is_readable RWS = True
attr_is_readable RWL = True
attr_is_readable MBZ = False
attr_is_readable MB1 = False
attr_is_readable RSVD = False
attr_is_readable _  = False

attr_is_writeonly :: Attr -> Bool
attr_is_writeonly RO = False
attr_is_writeonly WO = True
attr_is_writeonly RC = False
attr_is_writeonly ROS = False
attr_is_writeonly RW = False
attr_is_writeonly RWC = False
attr_is_writeonly RWZC = False
attr_is_writeonly RWO = False
attr_is_writeonly RWCS= False
attr_is_writeonly RWS = False
attr_is_writeonly RWL = False
attr_is_writeonly MBZ = False
attr_is_writeonly MB1 = False
attr_is_writeonly RSVD = False
attr_is_writeonly _  = False

-- Field must always be written with a value read from the register.
attr_preserve_on_write :: Attr -> Bool
attr_preserve_on_write RSVD = True
attr_preserve_on_write _ = False

-- Field can be preserved by reading from the register
attr_can_init_from_reg :: Attr -> Bool
attr_can_init_from_reg RW = True
attr_can_init_from_reg RSVD = True
attr_can_init_from_reg RWS = True
attr_can_init_from_reg RWL = True 
attr_can_init_from_reg _ = False

-- Field must always be written as zero
attr_zero_before_write :: Attr -> Bool
attr_zero_before_write MBZ = True
attr_zero_before_write _ = False

-- Field must always be written as one
attr_set_before_write :: Attr -> Bool
attr_set_before_write MB1 = True
attr_set_before_write _ = False
