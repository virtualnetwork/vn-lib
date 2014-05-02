-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- CUUID_Handler handles the discovery process on the CAN network.
-- CUUID_Handler sends out the RequestCUUID message to which other units will respond.
-- The function ReadEntry can then be used to retrieve information about
-- the units of the CAN network.

-- CUUID_Handler takes a procedure pointer HelloProc as input when initiated.
-- This procedure is called each time new unit is discovered.


pragma Profile (Ravenscar);

with Ada.Real_Time;

with VN.Communication.CAN.CAN_Filtering;
with VN.Communication.CAN.Logic;
with VN.Communication.CAN.Logic.Sender;

package VN.Communication.CAN.Logic.CUUID_Handler is

   type CUUID_Handler is
     new VN.Communication.CAN.Logic.Duty with private;

   type CUUID_Handler_ptr is access all CUUID_Handler'Class;

   overriding procedure Update(this : in out CUUID_Handler; msgIn : VN.Communication.CAN.CAN_Message_Logical; bMsgReceived : boolean;
                               msgOut : out VN.Communication.CAN.CAN_Message_Logical; bWillSend : out boolean);

   procedure Activate(this : in out CUUID_Handler; theCUUID : VN.VN_CUUID;
                      CANAddress : VN.Communication.CAN.CAN_Address_Sender;
                      sender : VN.Communication.CAN.Logic.Sender.Sender_Duty_ptr);

   --Should be obsolete soon:
   procedure ReadEntry(this : in out CUUID_Handler; index : VN.Communication.CAN.CAN_Address_Sender;
                       unitCUUID : out VN.VN_CUUID; isSM_CAN : out boolean; isSet : out Boolean);

private

   procedure HelloProc(this 	  : in out CUUID_Handler;
                       sender     : in VN.Communication.CAN.Logic.Sender.Sender_Duty_ptr;
                       CANAddress : VN.Communication.CAN.CAN_Address_Sender;
                       isSM_CAN   : Boolean);

   type Unit_Entry is
      record
         unitCUUID : VN.VN_CUUID;
         isSM_CAN  : Boolean;
         isFirstCUUIDHalfSet  : Boolean := false;
         isSecondCUUIDHalfSet : Boolean := false;
         isComponentTypeSet   : boolean := false;
      end record;

   type Unit_Table is array(VN.Communication.CAN.CAN_Address_Sender) of Unit_Entry;
   type CUUID_Responder_State is (Unactivated, Activated);

   --ToDO: Put this in a config file of some sort:
   DELAY_TIME : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds(1400);

   type CUUID_Handler is
     new VN.Communication.CAN.Logic.Duty with
      record
         mySender 	: VN.Communication.CAN.Logic.Sender.Sender_Duty_ptr;
         currentState 	: CUUID_Responder_State  := Unactivated;
         myCUUID 	: VN.VN_CUUID;
         myCANAddress   : VN.Communication.CAN.CAN_Address_Sender;
         units		: Unit_Table;
         hasRequested   : boolean := false;
         timer 		: Ada.Real_Time.Time;
      end record;

end VN.Communication.CAN.Logic.CUUID_Handler;

