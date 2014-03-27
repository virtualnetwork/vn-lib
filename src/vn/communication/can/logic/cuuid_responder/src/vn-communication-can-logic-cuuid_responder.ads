-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- CUUID_Responder responds to the RequestCUUID message. Shall be used by all units (nodes or SM-CANs).
-- CUUID_Responder shall be activated once one has been assigned a CAN address.

pragma Profile (Ravenscar);

with VN.Communication.CAN.Logic;


package VN.Communication.CAN.Logic.CUUID_Responder is

   type CUUID_Responder is new VN.Communication.CAN.Logic.Duty with private;
   type CUUID_Responder_ptr is access all CUUID_Responder'Class;

   overriding procedure Update(this : in out CUUID_Responder; msgIn : VN.Communication.CAN.Logic.CAN_Message_Logical; bMsgReceived : boolean;
                               msgOut : out VN.Communication.CAN.Logic.CAN_Message_Logical; bWillSend : out boolean);

   procedure Activate(this : in out CUUID_Responder; theCUUID : VN.Communication.CAN.Logic.CUUID;
                      CANAddress : VN.Communication.CAN.CAN_Address_Sender; isSM_CAN : boolean);

private

   type CUUID_Responder_State is (Unactivated, Activated, SendSecondCUUIDHalf, SendType);

   type CUUID_Responder is new VN.Communication.CAN.Logic.Duty with
      record
         currentState 	: CUUID_Responder_State := Unactivated;
         myCUUID 	: VN.Communication.CAN.Logic.CUUID;
         myCANAddress   : VN.Communication.CAN.CAN_Address_Sender;
         isSM_CAN	: boolean;
      end record;
end VN.Communication.CAN.Logic.CUUID_Responder;
