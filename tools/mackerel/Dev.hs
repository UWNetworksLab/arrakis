{- 
  Dev: representation of a device
   
  Part of Mackerel: a strawman device definition DSL for Barrelfish
   
  Copyright (c) 2007, 2008, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
-}  

module Dev where

import MackerelParser


import qualified TypeTable as TT
import qualified RegisterTable as RT
import qualified Space


{--------------------------------------------------------------------

--------------------------------------------------------------------}

data Rec = Rec { name :: String,
                 desc :: String,
                 args :: [ AST ],
                 types ::[ TT.Rec ],
                 all_types ::[ TT.Rec ],
                 registers ::[ RT.Rec ],
                 spaces :: [ Space.Rec ],
                 imports :: [ String ]
               }

-- Create a device record from a list of DeviceFile ASTs from the Parser.  The first is the actual device file, while the rest are imports
make_dev :: DeviceFile -> [DeviceFile] -> Rec
make_dev df@(DeviceFile (Device n bitorder al d decls) imps) dfl =
    let ttbl = TT.make_rtypetable df
        stbl = Space.builtins ++ [ rec | (SpaceDecl rec) <- decls ]
        rtbl = RT.make_table ttbl decls n bitorder stbl
    in
      Rec { name = n, 
            desc = d, 
            args = al, 
            types = ttbl,
            all_types = ttbl ++ ( concat $ map TT.make_rtypetable dfl ),
            registers = rtbl,
            spaces = stbl,
            imports = imps
        }

shdws :: Rec -> [ RT.Shadow ]
shdws dev = RT.get_shadows (registers dev)
