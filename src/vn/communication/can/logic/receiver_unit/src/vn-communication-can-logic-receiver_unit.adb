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

            --här borde egentligen man göra en kontroll att inga Transmission-meddelanden tas emot

            this.blockSize := DEFAULT_BLOCK_SIZE;

            bWillSend := true;
            this.useFlowControl := true;
            this.blockCount     := 0;
            this.sequenceNumber := 0;
            this.currentState := Transmitting;

            VN.Communication.CAN.Logic.Message_Utils.FlowControlToMessage(msgOut, VN.Communication.CAN.Convert(this.sender),
                                                        this.myCANAddress, this.useFlowControl,
                                                        this.blockSize);

            VN.Communication.CAN.Logic.DebugOutput("Receiver_Unit: FlowControl message sent", 4);

         when Transmitting =>

         --    Ada.Text_IO.Put_Line("Receiver_Unit, Transmitting: received message");

            if bMsgReceived and then msgIn.isNormal and then msgIn.msgType = VN.Communication.CAN.Logic.TRANSMISSION then
--                 Ada.Text_IO.Put_Line("Transmission message received, sender="
--                                      & msgIn.Sender'img & " Receiver= " & msgIn.Receiver'img);

               if msgIn.Sender =  this.sender and msgIn.Receiver = this.myCANAddress then

                  DeFragment(this.sequenceNumber, msgIn, this.receivedData, currentLength);

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
                     --
                     VN.Communication.CAN.Logic.DebugOutput("Receiver unit: Transmission complete, ", 4, false);

                     --write the VN message to the receive buffer:
                     VN.Message.Assignment(VN_msg.Data, this.receivedData);
                     --VN_msg.Data     := this.receivedData;
                     VN_msg.NumBytes := currentLength;
                     VN_msg.Receiver := VN.Communication.CAN.Convert(this.myCANAddress);
                     VN_msg.Sender   := this.sender;

--                       this.receiveBuffer.Append(VN_msg);
                     Receive_Buffer_pack.Insert(VN_msg, this.receiveBuffer.all);

--                       if not this.pendingSenders.Is_Empty then
                     if not Pending_Senders_pack.Empty(this.pendingSenders.all) then


                        Pending_Senders_pack.Remove(tempSender, this.pendingSenders.all);
                        this.Assign(tempSender.sender, tempSender.numMessages);

                      --  this.pendingSenders.Delete_First;
                        VN.Communication.CAN.Logic.DebugOutput("started new transmission", 4);

                        this.Update(msgIn, false, msgOut, bWillSend);
                        return;
                     else
                        this.currentState := Idle;
                         VN.Communication.CAN.Logic.DebugOutput("went idle.", 4);
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

     -- Ada.Text_IO.Put("myCANAddress=" & this.myCANAddress'Img);
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

   procedure DeFragment(seqNumber : Interfaces.Unsigned_16; CANMessage : VN.Communication.CAN.CAN_Message_Logical;
                        VNMessageContent : in out VN.Message.VN_Message_Basic; --VN.Communication.CAN.Logic.DataArray;
                        currentLength : out Interfaces.Unsigned_16) is

      procedure u8ToChar(c : out Character; u8 : in Interfaces.Unsigned_8) is
         x : Interfaces.Unsigned_8;
         for x'Address use c'Address;
      begin
         x := u8;
      end u8ToChar;

   begin

      null;
--TODO: Get this to work, needs redoing VN.Message.VN_Message_Basic

--        for i in 0 .. CANMessage.Length - 1 loop
--  --           VNMessageContent(VNMessageContent'First + Integer(seqNumber) * 8 + Integer(i)) :=
--  --             CANMessage.Data(CANMessage.Data'First + i);
--
--           u8ToChar(VNMessageContent(VNMessageContent'First + Integer(seqNumber) * 8 + Integer(i)),
--                    CANMessage.Data(CANMessage.Data'First + i));
--        end loop;
--
--        currentLength := seqNumber * 8 + Interfaces.Unsigned_16(CANMessage.Length);
   end DeFragment;

end VN.Communication.CAN.Logic.Receiver_Unit;

