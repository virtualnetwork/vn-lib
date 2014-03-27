-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- CAN_Address_Assignment will be activated by an SM-CAN if it is assigned as
-- master. CAN_Address_Assignment will remain unactivated if the SM-CAN
-- master negotiation process is lost.
-- CAN_Address_Assignment will assign CAN addresses to all other units on the
-- CAN network.

-- ToDo: Use the information in Address_Entry in the discovery process.

with VN.Communication.CAN.Logic;
package VN.Communication.CAN.Logic.CAN_Address_Assignment is

   type CAN_Assignment_Master is new VN.Communication.CAN.Logic.Duty with private;
   type CAN_Assignment_Master_ptr is access all CAN_Assignment_Master'Class;

   type Address_Entry is
      record
         unitUCID  : VN.Communication.CAN.UCID;
         isUsed	   : Boolean := false;
      end record;

   --ToDo: Address tables (routing tables) need to be revised
   type Address_Table is array(VN.Communication.CAN.CAN_Address_Sender) of Address_Entry;

   overriding procedure Update(this : in out CAN_Assignment_Master; msgIn : VN.Communication.CAN.CAN_Message_Logical; bMsgReceived : boolean;
                               msgOut : out VN.Communication.CAN.CAN_Message_Logical; bWillSend : out boolean);

   procedure Activate(this : in out CAN_Assignment_Master; theUCID : VN.Communication.CAN.UCID);

private

   type CAN_Assignment_Master_State is (Unactivated, Started);

   type CAN_Assignment_Master is new VN.Communication.CAN.Logic.Duty with
      record
         currentState 	: CAN_Assignment_Master_State := Unactivated;
         addresses 	: Address_Table;
         numUnitsFound  : VN.Communication.CAN.CAN_Address_Sender := 0;
         myUCID 	: VN.Communication.CAN.UCID;
      end record;

  function AssignCANAddress(this : in out CAN_Assignment_Master; theUCID : VN.Communication.CAN.UCID) return VN.Communication.CAN.CAN_Address_Sender;

end VN.Communication.CAN.Logic.CAN_Address_Assignment;

