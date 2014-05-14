-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- ComponentType_Handler handles the component discovery process on the CAN network.
-- ComponentType_Handler sends out the DiscoveryRequest message to which other units will respond.

-- ComponentType_Handler takes a procedure pointer HelloProc as input when initiated.
-- This procedure is called each time new unit is discovered.


pragma Profile (Ravenscar);

with Ada.Real_Time;

with VN.Communication.CAN.CAN_Filtering;
with VN.Communication.CAN.Logic;
with VN.Communication.CAN.Logic.Sender;

package VN.Communication.CAN.Logic.ComponentType_Handler is

   type ComponentType_Handler is
     new VN.Communication.CAN.Logic.Duty with private;

   type ComponentType_Handler_ptr is access all ComponentType_Handler'Class;

   overriding procedure Update(this : in out ComponentType_Handler; msgIn : VN.Communication.CAN.CAN_Message_Logical; bMsgReceived : boolean;
                               msgOut : out VN.Communication.CAN.CAN_Message_Logical; bWillSend : out boolean);

   procedure Activate(this : in out ComponentType_Handler; theCUUID : VN.VN_CUUID;
                      CANAddress : VN.Communication.CAN.CAN_Address_Sender;
                      sender : VN.Communication.CAN.Logic.Sender.Sender_Duty_ptr);
private

   procedure HelloProc(this 	  : in out ComponentType_Handler;
                       sender     : in VN.Communication.CAN.Logic.Sender.Sender_Duty_ptr;
                       CANAddress : VN.Communication.CAN.CAN_Address_Sender;
                       isSM_CAN   : Boolean);

   type Unit_Entry is
      record
         isSM_CAN  : Boolean;
         isComponentTypeSet   : boolean := false;
      end record;

   type Unit_Table is array(VN.Communication.CAN.CAN_Address_Sender) of Unit_Entry;
   type ComponentType_Handler_State is (Unactivated, Activated);

   --ToDO: Put this in a config file of some sort:
   DELAY_TIME : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds(500);

   type ComponentType_Handler is
     new VN.Communication.CAN.Logic.Duty with
      record
         mySender 	: VN.Communication.CAN.Logic.Sender.Sender_Duty_ptr;
         currentState 	: ComponentType_Handler_State  := Unactivated;
         myCUUID 	: VN.VN_CUUID;
         myCANAddress   : VN.Communication.CAN.CAN_Address_Sender;
         units		: Unit_Table;
         hasRequested   : boolean := false;
         timer 		: Ada.Real_Time.Time;
      end record;

end VN.Communication.CAN.Logic.ComponentType_Handler;

