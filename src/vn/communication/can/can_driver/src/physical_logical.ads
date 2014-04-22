-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- This package contains functions for conversion between logical
-- and physical representations of CAN messages.

with CAN_Defs;
use CAN_Defs;

with Lowlevel;
with Interfaces;
use Interfaces;

package Physical_Logical is
   procedure PhysicalToLogical(msgIn : CAN_Defs.CAN_Message; msgOut : out Lowlevel.CAN_Message_Logical);

   procedure LogicalToPhysical(msgIn : Lowlevel.CAN_Message_Logical; msgOut : out CAN_Defs.CAN_Message);
end Physical_Logical;
