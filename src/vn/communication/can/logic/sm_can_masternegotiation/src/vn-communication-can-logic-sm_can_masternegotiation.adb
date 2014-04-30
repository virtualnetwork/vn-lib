-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- Implementation of the state machine for the SM-CAN master negotioation process
-- of the VN-CAN protocol.
-- Once the SM-CAN master negotioation process is completed, the process of assigning
-- CAN addresses can begin.

with VN.Communication.CAN.Logic.Message_Utils;
with Ada.Real_Time;
use Ada.Real_Time;

package body VN.Communication.CAN.Logic.SM_CAN_MasterNegotiation is

   procedure Update(this : in out SM_CAN_MN_Duty; msgIn : VN.Communication.CAN.CAN_Message_Logical; bMsgReceived : boolean;
                    msgOut : out VN.Communication.CAN.CAN_Message_Logical; bWillSend : out boolean) is

      bHasMessage : Boolean := bMsgReceived;
   begin

      case this.currentState is
         when Start =>
            VN.Communication.CAN.Logic.DebugOutput(Integer(this.myUCID)'Img & ": SM_CAN_MasterNegotiation.Update: Start sent RequestCANAddress", 4);

            VN.Communication.CAN.Logic.Message_Utils.RequestCANAddressToMessage(msgOut, this.myUCID, true);
            this.timer := Ada.Real_Time.Clock;
            bWillSend := true;
            this.currentState := Started;

         when Started =>
            if bHasMessage then
               if msgIn.isNormal then
                  VN.Communication.CAN.Logic.DebugOutput(Integer(this.myUCID)'Img & ": SM_CAN_MasterNegotiation.Update: Started, normal message received, became Slave", 4);

                  --testing:
--                      VN.Communication.CAN.Logic.DebugOutput(" msgPrio= " & msgIn.msgPrio'Img &
--                                             " msgType= " & msgIn.msgType'Img &
--                                             " Length= " & msgIn.Length'Img &
--                                             " Receiver= " & msgIn.Receiver'Img &
--                                             " Sender= " & msgIn.Sender'Img, 4);


                  this.currentState := Slave;
                  bWillSend := false;
                  return;
               elsif not msgIn.isNormal then
                  declare
                     theUCID : VN.Communication.CAN.UCID;
                     bIs_SM_CAN : boolean;
                  begin
                     VN.Communication.CAN.Logic.Message_Utils.RequestCANAddressFromMessage(msgIn, theUCID, bIs_SM_CAN);

                     if bIs_SM_CAN and theUCID < this.myUCID then
                        VN.Communication.CAN.Logic.DebugOutput(Integer(this.myUCID)'Img & ": SM_CAN_MasterNegotiation.Update: Started, RequestCANAddress from lower UCID = " & theUCID'Img & " received, became slave", 4);
                        this.currentState := Slave;
                        bWillSend := false;
                        return;
                     elsif bIs_SM_CAN then
                        VN.Communication.CAN.Logic.DebugOutput(Integer(this.myUCID)'Img & ": SM_CAN_MasterNegotiation.Update: Started, RequestCANAddress from higher UCID = " & theUCID'Img & " received, replied", 4);
                        VN.Communication.CAN.Logic.Message_Utils.RequestCANAddressToMessage(msgOut, this.myUCID, true);
                        bWillSend := true;
                        this.timer := Ada.Real_Time.Clock;
                     else
                        bWillSend := false;
                     end if;
                  end;
               end if;
            end if;

            --if we have been in state Started for long enough, become master.
            if (Ada.Real_Time.Clock - this.timer) > WAIT_TIME then

               VN.Communication.CAN.Logic.DebugOutput(Integer(this.myUCID)'Img & ": SM_CAN_MasterNegotiation.Update: Started, became master", 4);

               this.currentState := Master;
               bWillSend := true;
               VN.Communication.CAN.Logic.Message_Utils.CANMasterAssignedToMessage(msgOut, 0);
               return;

            else
               VN.Communication.CAN.Logic.DebugOutput(Integer(this.myUCID)'Img & ": SM_CAN_MasterNegotiation.Update: Time not passed", 6);
               bWillSend := false;
               null;
            end if;
         when Slave =>
            bWillSend := false;
         when Master =>
            -- no need to respond to RequestCANAddress from SM-CANs since an
            -- AssignCANAddress message will be sent anyway.
            bWillSend := false;
      end case;
   end Update;

   function CurrentMode(this : in SM_CAN_MN_Duty) return SM_CAN_Mode is
   begin
      if this.currentState = Master then
         return MASTER;
      elsif this.currentState = Slave then
         return SLAVE;
      else
         return UNDETERMINED;
      end if;
   end CurrentMode;
end VN.Communication.CAN.Logic.SM_CAN_MasterNegotiation;
