------------------------------------------------------------------------------
--  This file is part of VN-Lib.
--
--  VN-Lib is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  VN-Lib is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with VN-Lib.  If not, see <http://www.gnu.org/licenses/>.
--
--  Copyright 2014, Nils Brynedal Ignell (nils.brynedal@gmail.com)
------------------------------------------------------------------------------

-- Summary:
-- CAN_Address_Assignment will be activated by an SM-CAN if it is assigned as
-- master. CAN_Address_Assignment will remain unactivated if the SM-CAN
-- master negotiation process is lost.
-- CAN_Address_Assignment will assign CAN addresses to all other units on the
-- CAN network.

with VN.Communication.CAN.Logic;

package VN.Communication.CAN.Logic.CAN_Address_Assignment is

   type CAN_Assignment_Master is new VN.Communication.CAN.Logic.Duty with private;
   type CAN_Assignment_Master_ptr is access all CAN_Assignment_Master'Class;

   type Address_Entry is
      record
         unitUCID  : VN.Communication.CAN.UCID;
         isUsed	   : Boolean := false;
      end record;

   type Address_Table is array(VN.Communication.CAN.CAN_Address_Sender) of Address_Entry;

   overriding procedure Update(this : in out CAN_Assignment_Master; msgIn : VN.Communication.CAN.CAN_Message_Logical; bMsgReceived : boolean;
                               msgOut : out VN.Communication.CAN.CAN_Message_Logical; bWillSend : out boolean);

   procedure Activate(this : in out CAN_Assignment_Master; theUCID : VN.Communication.CAN.UCID);

private

   type CAN_Assignment_Master_State is (Unactivated, Started);

   type CAN_Assignment_Master is
     new VN.Communication.CAN.Logic.Duty with
      record
         currentState 	: CAN_Assignment_Master_State := Unactivated;
         addresses 	: Address_Table;
         numUnitsFound  : VN.Communication.CAN.CAN_Address_Sender := 0;
         myUCID 	: VN.Communication.CAN.UCID;
      end record;

   procedure AssignCANAddress(this : in out CAN_Assignment_Master; theUCID : VN.Communication.CAN.UCID;
                              address : out VN.Communication.CAN.CAN_Address_Sender);

end VN.Communication.CAN.Logic.CAN_Address_Assignment;

