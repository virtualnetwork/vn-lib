-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- Implementation of the state machine for the SM-CAN master negotioation process
-- of the VN-CAN protocol.
-- Once the SM-CAN master negotioation process is completed, the process of assigning
-- CAN addresses can begin.


with Ada.Real_Time;
with VN.Communication.CAN.Logic;

package VN.Communication.CAN.Logic.SM_CAN_MasterNegotiation is

   type SM_CAN_Mode is (UNDETERMINED, MASTER, SLAVE);


   type SM_CAN_MN_Duty(theUCID : access VN.Communication.CAN.UCID) is
     new VN.Communication.CAN.Logic.Duty with private;

   type SM_CAN_MN_Duty_ptr is access SM_CAN_MN_Duty'Class;


   overriding procedure Update(this : in out SM_CAN_MN_Duty; msgIn : VN.Communication.CAN.CAN_Message_Logical; bMsgReceived : boolean;
                               msgOut : out VN.Communication.CAN.CAN_Message_Logical; bWillSend : out boolean);

   function CurrentMode(this : in SM_CAN_MN_Duty) return SM_CAN_Mode;

private

   WAIT_TIME : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds(Integer(2000));

   type SM_CAN_MN_State is (Start, Started, Slave, Master);

   type SM_CAN_MN_Duty(theUCID : access VN.Communication.CAN.UCID) is
     new VN.Communication.CAN.Logic.Duty with
      record
         currentState : SM_CAN_MN_State := Start;
         timer    : Ada.Real_Time.Time;
         myUCID   : VN.Communication.CAN.UCID := theUCID.all;
     --    mainFilter : VN.Communication.CAN.CAN_Filtering.Filter_ID_Type;
      end record;

end VN.Communication.CAN.Logic.SM_CAN_MasterNegotiation;

