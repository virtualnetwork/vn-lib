-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- Sender_Duty is a VN.Communication.CAN.Logic interface for sending VN-messages. Sender_Duty
-- owns several Sender unit duties.
-- Before it can be used, Sender_Duty will need to be activated. This cannot
-- be done until one has been assigned a CAN address.
-- Sender_Duty has a send buffer, if no Sender units are available when a VN
-- message is to be sent it will be written to this buffer.
-- The message will then be sent when a Sender unit becomes available.

package body VN.Communication.CAN.Logic.Sender is

   overriding procedure Update(this : in out Sender_Duty; msgIn : VN.Communication.CAN.CAN_Message_Logical; bMsgReceived : boolean;
                               msgOut : out VN.Communication.CAN.CAN_Message_Logical; bWillSend : out boolean) is
   begin
      case this.currentState is
         when Unactivated =>
            bWillSend:=false;
         when Activated =>

            bWillSend := false;

            --If the message is meant for a certain Sender Unit, then give the message to this sender unit:
            if bMsgReceived and then msgIn.Receiver = this.myCANAddress then
               for i in this.units'range loop
                  if this.units(i).isActive and then this.units(i).Receiver = msgIn.Sender then
--                       VN.Communication.CAN.Logic.DebugOutput(i'Img& ": ", 4);
                     this.units(i).Update(msgIn, bMsgReceived, msgOut, bWillSend);
                     return;
                  end if;
               end loop;
               bWillSend := false; --this case should not happen, but if a message was not meant for any sender unit, do't send anything
--                 return;
            end if;


            --If there is a VN message to send, assign it to a Sender Unit (if available):
            if not Send_Buffer_pack.Empty(this.sendBuffer) then
               declare
                  freeUnit : Sender_Unit_Duty_ptr := this.GetFreeUnit;
                  msg : VN.Communication.CAN.Logic.VN_Message_Internal;
               begin

                  if freeUnit /= null then
                     Send_Buffer_pack.Remove(msg, this.sendBuffer);
                     freeUnit.Send(msg);
                     freeUnit.Update(msgIn, false, msgOut, bWillSend);
                     return;
                  end if;
               end;
            end if;

            --otherwise, find an active Sender Unit and let it send a message (iterate which one you take):
            bWillSend := false;
            loop
               if this.units(this.iterator).isActive  then
--                    VN.Communication.CAN.Logic.DebugOutput(this.iterator'Img& ": ", 4);
                  this.units(this.iterator).Update(msgIn, bMsgReceived, msgOut, bWillSend);
               end if;

               this.iterator := this.iterator + 1;
               if this.iterator > this.units'Last then
                  this.iterator := this.units'First;
                  exit;
               end if;

               if bWillSend then
                  exit;
               end if;
            end loop;
      end case;
   end Update;

   procedure Activate(this : in out Sender_Duty; address : VN.Communication.CAN.CAN_Address_Sender) is
   begin
      if this.currentState = Unactivated then
         this.currentState := Activated;
         this.myCANAddress := address;

         for i in this.units'range loop
            this.units(i).Activate(address);
         end loop;
      end if;
   end Activate;

   procedure SendVNMessage(this : in out Sender_Duty; msg : VN_Message_Internal;
                           result : out VN.Send_Status) is
   begin

      if Send_Buffer_pack.Full(this.sendBuffer) then
         result := VN.ERROR_BUFFER_FULL;
      else
         Send_Buffer_pack.Insert(msg, this.sendBuffer);
         result := VN.OK;
      end if;
   end SendVNMessage;

   function GetFreeUnit(this : in Sender_Duty) return Sender_Unit_Duty_ptr is
   begin
      for i in this.units'range loop
         if not this.units(i).isActive then
            return  this.units(i);
         end if;
      end loop;
      return null;
   end GetFreeUnit;

end VN.Communication.CAN.Logic.Sender;
