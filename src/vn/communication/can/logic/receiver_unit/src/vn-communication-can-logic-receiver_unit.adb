-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- Implements the state machine for receiving a single VN message. Receiver_Duty will
-- use an instane of Receiver_Unit_Duty to receive a VN message.
-- Before it can be used, Receiver_Unit_Duty will need to be activated. This cannot
-- be done until one has been assigned a CAN address.

-- ToDo: DeFragment must be implemented further

pragma Profile (Ravenscar);

with VN.Communication.CAN.Logic.Message_Utils;

package body VN.Communication.CAN.Logic.Receiver_Unit is

   overriding procedure Update(this : in out Receiver_Unit_Duty; msgIn : VN.Communication.CAN.CAN_Message_Logical; bMsgReceived : boolean;
                               msgOut : out VN.Communication.CAN.CAN_Message_Logical; bWillSend : out boolean) is

      currentLength : Interfaces.Unsigned_16;
      VN_msg : VN.Communication.CAN.Logic.VN_Message_Internal;

      tempSender : Pending_Sender;
   begin

      case this.currentState is
         when Idle =>
            bWillSend := false;

         when Started =>

            --ToDo: Check that no Transmission messages are received in this state

            this.blockSize := DEFAULT_BLOCK_SIZE;

            bWillSend 		:= true;
            this.useFlowControl := true;
            this.blockCount     := 0;
            this.sequenceNumber := 0;
            this.currentState   := Transmitting;

            VN.Communication.CAN.Logic.Message_Utils.FlowControlToMessage(msgOut, VN.Communication.CAN.Convert(this.sender),
                                                        this.myCANAddress, this.useFlowControl,
                                                        this.blockSize);

            VN.Communication.CAN.Logic.DebugOutput("Receiver_Unit: FlowControl message sent", 4);

         when Transmitting =>

            if bMsgReceived and then msgIn.isNormal and then msgIn.msgType = VN.Communication.CAN.Logic.TRANSMISSION then


               if msgIn.Sender = this.sender and msgIn.Receiver = this.myCANAddress then

                  DeFragment(this.sequenceNumber, this.numMessages, msgIn, this.receivedData, currentLength);

                  this.sequenceNumber := this.sequenceNumber + 1;
                  this.blockCount     := this.blockCount + 1;

                  VN.Communication.CAN.Logic.DebugOutput("Transmission message received by " &
                                         this.myCANAddress'Img & " from " & msgIn.Sender'img &
                                         "  blockCount= " & this.blockCount'Img & " blockSize=" & this.blockSize'img
                                       & " FlowCtrl= " & this.useFlowControl'Img, 4);

                  if this.useFlowControl and this.blockCount >= this.blockSize and this.sequenceNumber < this.numMessages then

                     VN.Communication.CAN.Logic.Message_Utils.FlowControlToMessage(msgOut, VN.Communication.CAN.Convert(this.sender),
                                                                 this.myCANAddress, false, this.blockSize);
                     bWillSend := true;
                     this.blockCount := 0;
                     VN.Communication.CAN.Logic.DebugOutput("Receiver_Unit: Block full, FlowControl message sent", 4);
                     return;

                  elsif this.sequenceNumber >= this.numMessages then

                     VN.Communication.CAN.Logic.DebugOutput("Receiver unit: Transmission complete, ", 3, false);

                     --write the VN message to the receive buffer:
                     VN.Message.Deserialize(VN_msg.Data, this.receivedData);
                     VN_msg.NumBytes := currentLength;
                     VN_msg.Receiver := VN.Communication.CAN.Convert(this.myCANAddress);
                     VN_msg.Sender   := this.sender;

                     Receive_Buffer_pack.Insert(VN_msg, this.receiveBuffer.all);

                     --if there are more messages to be sent, take another messae from the
                     --buffer and send it:
                     if not Pending_Senders_pack.Empty(this.pendingSenders.all) then

                        Pending_Senders_pack.Remove(tempSender, this.pendingSenders.all);
                        this.Assign(tempSender.sender, tempSender.numMessages);


                        VN.Communication.CAN.Logic.DebugOutput("started new transmission", 3);

                        this.Update(msgIn, false, msgOut, bWillSend);
                        return;
                     else
                        this.currentState := Idle;
                         VN.Communication.CAN.Logic.DebugOutput("went idle.", 3);
                     end if;
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
   --   if this.currentState = Unactivated then
         this.myCANAddress   := address;
         this.currentState   := Idle;
         this.receiveBuffer  := receiveBufferPtr;
         this.pendingSenders := pendingSendersPtr;
     -- end if;
   end Activate;

   procedure Assign(this : in out Receiver_Unit_Duty; sender : VN.Communication.CAN.CAN_Address_Sender;
                    numMessages	: Interfaces.Unsigned_16) is
   begin
      this.currentState   := Started;
      this.Sender  	  := sender;
      this.numMessages    := numMessages;

      VN.Communication.CAN.Logic.DebugOutput("Reciever unit assigned, sender= " & this.Sender'Img & " numMessages= " & this.numMessages'img, 4);
   end Assign;

   function isActive(this : in Receiver_Unit_Duty) return boolean is
   begin
      return not (this.currentState = Idle);
   end isActive;

   function Sender(this : in Receiver_Unit_Duty) return VN.Communication.CAN.CAN_Address_Sender is
   begin
      return this.Sender;
   end Sender;

   procedure DeFragment(seqNumber 	 : Interfaces.Unsigned_16;
                        numMessages	 : Interfaces.Unsigned_16;
                        CANMessage 	 : VN.Communication.CAN.CAN_Message_Logical;
                        VNMessageContent : in out VN.Message.VN_Message_Byte_Array;
                        currentLength 	 : out Interfaces.Unsigned_16) is

      index, startIndex, lastIndex : Integer; -- zerobased
   begin

      startIndex := Integer(seqNumber) * 8;

      for i in 0 .. CANMessage.Length - 1 loop
         index := startIndex + Integer(i);

         VNMessageContent(VNMessageContent'First + index) :=
           CANMessage.Data(CANMessage.Data'First + i);

         --reverse index on VNMessageContent:
--           VNMessageContent(VNMessageContent'Last - index) :=
--             CANMessage.Data(CANMessage.Data'First + i);
      end loop;

      currentLength := seqNumber * 8 + Interfaces.Unsigned_16(CANMessage.Length);

      -- If the last CAN message has been received, move the
      -- two last received bytes to the end of the array.
      -- These two bytes are the checksum and should allways be put at the end of the array.
--        if seqNumber = numMessages then
--           lastIndex := startIndex + Integer(CANMessage.Length) - 1;
--
--           VNMessageContent(VNMessageContent'Last)     :=  VNMessageContent(lastIndex);
--           VNMessageContent(VNMessageContent'Last - 1) :=  VNMessageContent(lastIndex - 1);
--
--           --reverse index on VNMessageContent:
--  --           VNMessageContent(VNMessageContent'First)     :=  VNMessageContent(VNMessageContent'Last - lastIndex);
--  --           VNMessageContent(VNMessageContent'First + 1) :=  VNMessageContent(VNMessageContent'Last - lastIndex - 1);
--        end if;
   end DeFragment;

end VN.Communication.CAN.Logic.Receiver_Unit;

