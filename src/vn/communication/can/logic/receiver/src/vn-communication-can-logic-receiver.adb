--  pragma Profile (Ravenscar);

with VN.Communication.CAN.Logic.Message_Utils;

package body VN.Communication.CAN.Logic.Receiver is

   overriding procedure Update(this : in out Receiver_Duty; msgIn : VN.Communication.CAN.Logic.CAN_Message_Logical; bMsgReceived : boolean;
                               msgOut : out VN.Communication.CAN.Logic.CAN_Message_Logical; bWillSend : out boolean) is
    --  use Ada.Containers;

      freeUnit 	: Receiver_Unit_Duty_ptr;
      pending  	: VN.Communication.CAN.Logic.Receiver_Unit.Pending_Sender;
      rec 	: VN.Communication.CAN.CAN_Address_Receiver;
   begin

      case this.currentState is
         when Unactivated =>
            bWillSend:=false;

         when Activated =>
            if bMsgReceived and then msgIn.isNormal and then msgIn.Receiver = this.myCANAddress then
               if msgIn.msgType = VN.Communication.CAN.Logic.START_TRANSMISSION then

                  if not Pending_Senders_pack.Full(this.pendingSenders.all) then --if pendingSenders is not full
                     VN.Communication.CAN.Logic.Message_Utils.StartTransmissionFromMessage(msgIn, rec, pending.sender, pending.numMessages);
                     if rec = this.myCANAddress then

                        VN.Communication.CAN.Logic.DebugOutput("StartTransmission message recieved, transmission pending. Sender = "
                                             & pending.sender'Img & " numMessages= " & pending.numMessages'img, 4);

                        --Check whether this StartTransmission has been recieved eariler (the sender might resend the StartTransmission message)
                        -- if not, add it as a pending transmission:
                        if not Pending_Senders_pack.Find(pending, this.pendingSenders.all) then
--                             this.pendingSenders.Append(pending);
                           Pending_Senders_pack.Insert(pending, this.pendingSenders.all);
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

            if not Pending_Senders_pack.Empty(this.pendingSenders.all) then
               freeUnit := this.GetFreeUnit;

               if freeUnit /= null then
--                 pending  := this.pendingSenders.First_Element;
                  Pending_Senders_pack.Remove(pending, this.pendingSenders.all);

            --   if freeUnit /= null then
                  freeUnit.Assign(pending.sender, pending.numMessages);
               --   this.pendingSenders.Delete_First;
                  freeUnit.Update(msgIn, false, msgOut, bWillSend);
             end if;
            end if;
      end case;
   end Update;

   procedure ReceiveVNMessage(this : in out Receiver_Duty; msg : out VN_Message_Internal; msgWasReceived : out boolean) is
   begin
      if Receive_Buffer_pack.Empty(this.receiveBuffer.all) then --this.receiveBuffer.Is_Empty then
         msgWasReceived := false;
      else
         msgWasReceived := true;
         Receive_Buffer_pack.Remove(msg, this.receiveBuffer.all);
--           msg := this.receiveBuffer.First_Element;
--           this.receiveBuffer.Delete_First;
      end if;
   end ReceiveVNMessage;

   procedure Activate(this : in out Receiver_Duty; address : VN.Communication.CAN.CAN_Address_Sender) is
   begin
      if this.currentState = Unactivated then
         this.currentState := Activated;
         this.myCANAddress := address;

         for i in this.units'range loop
            this.units(i).Activate(address, this.receiveBuffer, this.pendingSenders);
         end loop;
      end if;
   end Activate;

   function GetFreeUnit(this : in out Receiver_Duty) return Receiver_Unit_Duty_ptr is
   begin
      for i in this.units'range loop
         if not this.units(i).isActive then
            return  this.units(i);
         end if;
      end loop;
      return null;
   end GetFreeUnit;

end VN.Communication.CAN.Logic.Receiver;
