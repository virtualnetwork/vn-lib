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
-- Implements the state machine for receiving a single VN message. Receiver_Duty will
-- use an instane of Receiver_Unit_Duty to receive a VN message.
-- Before it can be used, Receiver_Unit_Duty will need to be activated. This cannot
-- be done until one has been assigned a CAN address.

pragma Profile (Ravenscar);

with VN.Communication.CAN.Logic.Message_Utils;

package body VN.Communication.CAN.Logic.Receiver_Unit is

   overriding procedure Update(this : in out Receiver_Unit_Duty; msgIn : VN.Communication.CAN.CAN_Message_Logical; bMsgReceived : boolean;
                               msgOut : out VN.Communication.CAN.CAN_Message_Logical; bWillSend : out boolean) is

      currentLength : Interfaces.Unsigned_16;
      VN_msg : VN.Communication.CAN.Logic.VN_Message_Internal;

      tempSender : Pending_Sender;

      TRANSMISSION_MSG_RECEIVED_IN_STATE_Started : exception;
   begin

      case this.currentState is
         when Idle =>
            bWillSend := false;

         when Started =>

            --ToDo: Check that no Transmission messages are received in this state in a better way than raising an exception
            if bMsgReceived and then msgIn.isNormal and then msgIn.msgType = VN.Communication.CAN.Logic.TRANSMISSION then
               raise TRANSMISSION_MSG_RECEIVED_IN_STATE_Started;
            end if;

            this.blockSize := DEFAULT_BLOCK_SIZE;

            bWillSend 		:= true;
            this.useFlowControl := true;
            this.blockCount     := 0;
            this.sequenceNumber := 0;
            this.currentState   := Transmitting;

            VN.Communication.CAN.Logic.Message_Utils.FlowControlToMessage(msgOut, VN.Communication.CAN.Convert(this.sender),
                                                        this.myCANAddress, this.useFlowControl,
                                                        this.blockSize);

            VN.Communication.CAN.Logic.DebugOutput("Receiver_Unit: FlowControl message sent to CAN addr " & this.sender'Img & ", sequence no= "& this.sequenceNumber'Img, 4);

         when Transmitting =>

            if bMsgReceived and then msgIn.isNormal and then msgIn.Sender = this.sender and then
              msgIn.Receiver = this.myCANAddress then


               if msgIn.msgType = VN.Communication.CAN.Logic.TRANSMISSION then

                  VN.Communication.CAN.Logic.DebugOutput("DeFragment. receiver= " & this.myCANAddress'img & " sender= " & this.sender'img &
                                                           " sequence no= "& this.sequenceNumber'Img & " numBytes= " & this.numBytes'Img, 6);

                  VN.Communication.CAN.Logic.Message_Utils.DeFragment(this.sequenceNumber, this.numBytes, msgIn, this.receivedData, currentLength);


                  this.sequenceNumber := this.sequenceNumber + 1;
                  this.blockCount     := this.blockCount + 1;

                  VN.Communication.CAN.Logic.DebugOutput("Receiver_Unit: Transmission message received by " &
                                         this.myCANAddress'Img & " from " & msgIn.Sender'img &
                                         "  blockCount= " & this.blockCount'Img & " blockSize=" & this.blockSize'img
                                       & " FlowCtrl= " & this.useFlowControl'Img & " sequence no= "& this.sequenceNumber'Img, 4);

                  -- Flow control is used, block is full and we are not done receiving
                  if this.useFlowControl and this.blockCount >= this.blockSize and this.sequenceNumber * 8 < this.numBytes then

                     VN.Communication.CAN.Logic.Message_Utils.FlowControlToMessage(msgOut, VN.Communication.CAN.Convert(this.sender),
                                                                 this.myCANAddress, false, this.blockSize);
                     bWillSend := true;
                     this.blockCount := 0;
                     VN.Communication.CAN.Logic.DebugOutput("Receiver_Unit: Block full, FlowControl message sent to CAN addr " & this.sender'Img &
                                                              " sequence no= "& this.sequenceNumber'Img, 4);
                     return;

                  elsif this.sequenceNumber * 8 >= this.numBytes then -- transmission done

                     VN.Communication.CAN.Logic.DebugOutput("Receiver unit on CAN address " & this.myCANAddress'Img &
                                                              ": Transmission from CAN address " & this.sender'Img & " complete,"  &
                                                              " sequence no= "& this.sequenceNumber'Img , 3, false);

                     --write the VN message to the receive buffer:
                     VN.Message.Deserialize(VN_msg.Data, this.receivedData);

                     VN.Communication.CAN.Logic.DebugOutput(" Opcode= " & VN_msg.Data.Header.Opcode'img, 3, false);

                     VN_msg.NumBytes := this.numBytes;
                     VN_msg.Receiver := VN.Communication.CAN.Convert(this.myCANAddress);
                     VN_msg.Sender   := this.sender;

                     Receive_Buffer_pack.Insert(VN_msg, this.receiveBuffer.all);

                     --if there are more messages to be sent, take another messae from the
                     --buffer and send it:
                     if not Pending_Senders_pack.Empty(this.pendingSenders.all) then

                        Pending_Senders_pack.Remove(tempSender, this.pendingSenders.all);
                        this.Assign(tempSender.sender, tempSender.numBytes);


                        VN.Communication.CAN.Logic.DebugOutput(" started new transmission from CAN address " & tempSender.sender'img, 3);

                        this.Update(msgIn, false, msgOut, bWillSend);
                        return;
                     else
                        this.currentState := Idle;
                         VN.Communication.CAN.Logic.DebugOutput(" went idle.", 3);
                     end if;
                  end if;

               elsif msgIn.msgType = VN.Communication.CAN.Logic.START_TRANSMISSION then
                  if this.sequenceNumber = 0 then
                     VN.Communication.CAN.Logic.Message_Utils.FlowControlToMessage(msgOut, VN.Communication.CAN.Convert(this.sender),
                                                        this.myCANAddress, this.useFlowControl,
                                                        this.blockSize);

                     VN.Communication.CAN.Logic.DebugOutput("Receiver_Unit: FlowControl message REsent to CAN addr " &
                                                              this.sender'Img & ", sequence no= "& this.sequenceNumber'Img, 4);
                     return;
                  else
                     VN.Communication.CAN.Logic.DebugOutput("Receiver_Unit: StartTransmission message received again from CAN addr " &
                                                              this.sender'Img & "when sequence was nonzero = "& this.sequenceNumber'Img, 4);
                  end if;
               end if;
            end if;

            bWillSend := false;
      end case;
   end Update;

   -- shall be called after a CAN address has been obtained
   procedure Activate(this : in out Receiver_Unit_Duty; address : VN.Communication.CAN.CAN_Address_Sender;
                      receiveBufferPtr : Receive_Buffer_ptr; pendingSendersPtr : Pending_Senders_ptr) is
   begin
         this.myCANAddress   := address;
         this.currentState   := Idle;
         this.receiveBuffer  := receiveBufferPtr;
         this.pendingSenders := pendingSendersPtr;
   end Activate;

   procedure Assign(this : in out Receiver_Unit_Duty; sender : VN.Communication.CAN.CAN_Address_Sender;
                    numBytes	: Interfaces.Unsigned_16) is
   begin
      this.currentState   := Started;
      this.Sender  	  := sender;
      this.numBytes       := numBytes;

      VN.Communication.CAN.Logic.DebugOutput("Receiver_Unit: Reciever unit assigned, sender= " & this.Sender'Img & " numBytes= " & this.numBytes'img, 4);
   end Assign;

   function isActive(this : in Receiver_Unit_Duty) return boolean is
   begin
      return not (this.currentState = Idle);
   end isActive;

   function Sender(this : in Receiver_Unit_Duty) return VN.Communication.CAN.CAN_Address_Sender is
   begin
      return this.Sender;
   end Sender;
end VN.Communication.CAN.Logic.Receiver_Unit;

