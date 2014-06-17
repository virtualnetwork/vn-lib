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
-- Implements the state machine for receiving VN messages. Receiver_Duty will
-- use an instane of Receiver_Unit_Duty to send a VN message.
-- Before it can be used, Receiver_Duty will need to be activated. This cannot
-- be done until one has been assigned a CAN address.
-- Receiver_Duty has a receive buffer. Each Receiver_Unit_Duty has a pointer
-- (access variable) to this received. When a Receiver_Unit_Duty has received
-- a VN message, it will write it to this buffer.


with VN.Communication.CAN.Logic.Message_Utils;

package body VN.Communication.CAN.Logic.Receiver is

   overriding procedure Update(this : in out Receiver_Duty; msgIn : VN.Communication.CAN.CAN_Message_Logical; bMsgReceived : boolean;
                               msgOut : out VN.Communication.CAN.CAN_Message_Logical; bWillSend : out boolean) is

      freeUnit 	: Receiver_Unit_Duty_ptr;
      pending  	: VN.Communication.CAN.Logic.Receiver_Unit.Pending_Sender;
      rec 	: VN.Communication.CAN.CAN_Address_Receiver;

      alreadyReceiving : boolean := false; --not needed
   begin

      case this.currentState is
         when Unactivated =>
            bWillSend:=false;

         when Activated =>

            bWillSend := false;
            if bMsgReceived and then msgIn.isNormal and then msgIn.Receiver = this.myCANAddress then
               if msgIn.msgType = VN.Communication.CAN.Logic.START_TRANSMISSION then

                  -- check if we are already receiving a VN message from the sender of this StartTransmission message
                  for i in this.units'Range loop
                     if this.units(i).isActive and then this.units(i).Sender = msgIn.Sender then
                        alreadyReceiving := true;

                        this.units(i).Update(msgIn, bMsgReceived, msgOut, bWillSend);
                        return;
                     end if;
                  end loop;

                  if (not alreadyReceiving) and (not Pending_Senders_pack.Full(this.pendingSenders)) then
                     VN.Communication.CAN.Logic.Message_Utils.StartTransmissionFromMessage(msgIn, rec, pending.sender, pending.numBytes);

                     if rec = this.myCANAddress then

                        VN.Communication.CAN.Logic.DebugOutput("StartTransmission message recieved by CAN address " & this.myCANAddress'Img &
                                                                 ", transmission pending. Sender = "
                                                               & pending.sender'Img & " numBytes= " & pending.numBytes'img, 3);

                        --Check whether this StartTransmission has been recieved eariler
                        -- (the sender might resend the StartTransmission message)
                        -- if not, add it as a pending transmission:
                        if not Pending_Senders_pack.Find(pending, this.pendingSenders) then
                           Pending_Senders_pack.Insert(pending, this.pendingSenders);
                        end if;
                     end if;
                  end if;

               elsif msgIn.msgType = VN.Communication.CAN.Logic.TRANSMISSION then
                  for i in this.units'range loop
                     if this.units(i).isActive and then this.units(i).Sender = msgIn.sender then
                        this.units(i).Update(msgIn, bMsgReceived, msgOut, bWillSend);
                        return;
                     end if;
                  end loop;
               end if;
            end if;

            if not Pending_Senders_pack.Empty(this.pendingSenders) then
               this.GetFreeUnit(freeUnit);

               if freeUnit /= null then
                  Pending_Senders_pack.Remove(pending, this.pendingSenders);

                  freeUnit.Assign(pending.sender, pending.numBytes);
                  freeUnit.Update(msgIn, false, msgOut, bWillSend);
             end if;
            end if;
      end case;
   end Update;

   procedure ReceiveVNMessage(this : in out Receiver_Duty; msg : out VN_Message_Internal;
                              status : out VN.Receive_Status) is
   begin
      if Receive_Buffer_pack.Empty(this.receiveBuffer) then
         status := VN.NO_MSG_RECEIVED;
      else
         Receive_Buffer_pack.Remove(msg, this.receiveBuffer);

         if Receive_Buffer_pack.Empty(this.receiveBuffer) then
            status := VN.MSG_RECEIVED_NO_MORE_AVAILABLE;
         else
            status := VN.MSG_RECEIVED_MORE_AVAILABLE;
         end if;
      end if;
   end ReceiveVNMessage;

   procedure Activate(this : in out Receiver_Duty; address : VN.Communication.CAN.CAN_Address_Sender) is
   begin
      if this.currentState = Unactivated then
         this.currentState := Activated;
         this.myCANAddress := address;

         DebugOutput("Receiver activated with CAN address " & address'Img, 5);

         for i in this.units'range loop
            this.units(i).Activate(address, this.receiveBuffer'Unchecked_Access, this.pendingSenders'Unchecked_Access);
         end loop;
      end if;
   end Activate;

   procedure GetFreeUnit(this : in out Receiver_Duty; ret : out Receiver_Unit_Duty_ptr) is
   begin
      for i in this.units'range loop
         if not this.units(i).isActive then
            ret := this.units(i)'Unchecked_Access;
            return;
         end if;
      end loop;
      ret := null;
      return;
   end GetFreeUnit;

end VN.Communication.CAN.Logic.Receiver;
