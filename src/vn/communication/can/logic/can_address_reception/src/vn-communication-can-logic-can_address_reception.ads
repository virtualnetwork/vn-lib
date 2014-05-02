-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- CAN_Address_Reception will be activated by an SM-CAN if it is assigned as
-- slave. CAN_Address_Reception will remain unactivated if the SM-CAN wins the
-- SM-CAN master negotiation process.
-- CAN_Address_Reception will be assigned a CAN address from the SM-CAN master
-- on the CAN network.


pragma Profile (Ravenscar);

with Ada.Real_Time;
with VN.Communication.CAN.CAN_Filtering;
with VN.Communication.CAN.Logic;

package VN.Communication.CAN.Logic.CAN_Address_Reception is

   type CAN_Assignment_Node(theUCID : access VN.Communication.CAN.UCID;
                            theFilter : VN.Communication.CAN.CAN_Filtering.CAN_Filter_Access) is
     new VN.Communication.CAN.Logic.Duty with private;

   type CAN_Assignment_Node_ptr is access all CAN_Assignment_Node'Class;

   overriding procedure Update(this : in out CAN_Assignment_Node; msgIn : VN.Communication.CAN.CAN_Message_Logical; bMsgReceived : boolean;
                               msgOut : out VN.Communication.CAN.CAN_Message_Logical; bWillSend : out boolean);

   procedure Activate(this : in out CAN_Assignment_Node);

   procedure Address(this : in out CAN_Assignment_Node; address : out CAN_Address_Sender;
                     isAssigned : out boolean);

private

   type CAN_Assignment_Node_State is (Unactivated, Start, Started, Assigned);

   TIME_TO_WAIT_FOR_ADDRESS : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds(1000);

   type CAN_Assignment_Node(theUCID : access VN.Communication.CAN.UCID;
                            theFilter : VN.Communication.CAN.CAN_Filtering.CAN_Filter_Access) is
     new VN.Communication.CAN.Logic.Duty with
      record
         currentState 	: CAN_Assignment_Node_State := Unactivated;
         timer 		: Ada.Real_Time.Time;
         myUCID 	: VN.Communication.CAN.UCID := theUCID.all;
         myCANAddress 	: CAN_Address_Sender;
      end record;

end VN.Communication.CAN.Logic.CAN_Address_Reception;
