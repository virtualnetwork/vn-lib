-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary: 
-- Implements the state machine for sending a single VN message. Sender_Duty will
-- use an instane of Sender_Unit_Duty to send a VN message.
-- Before it can be used, Sender_Unit_Duty will need to be activated. This cannot
-- be done until one has been assigned a CAN address.


with VN.Communication.CAN.Logic.Message_Utils;

package body VN.Communication.CAN.Logic.Sender_Unit is

   overriding procedure Update(this : in out Sender_Unit_Duty; msgIn : VN.Communication.CAN.CAN_Message_Logical; bMsgReceived : boolean;
                               msgOut : out VN.Communication.CAN.CAN_Message_Logical; bWillSend : out boolean) is
      use VN.Communication.CAN.Logic;
      use Ada.Real_Time;
   begin

      case this.currentState is
         when Idle =>
            bWillSend := false;

         when Initiated =>
            VN.Communication.CAN.Logic.Message_Utils.StartTransmissionToMessage(msgOut, this.receiver, this.myCANAddress,
                                                              NumMessagesToSend(this.ToSend.NumBytes));

            VN.Communication.CAN.Logic.DebugOutput("StartTransmission message sent from address " & this.myCANAddress'img
                                 & " to " & this.receiver'img & " numMessages= " & NumMessagesToSend(this.ToSend.NumBytes)'img, 4);

            this.currentState := Started;
            this.time := Ada.Real_Time.Clock;
            bWillSend := true;

         when Started =>
            if bMsgReceived and then msgIn.isNormal and then msgIn.msgType = VN.Communication.CAN.Logic.FLOW_CONTROL then
               if msgIn.Receiver = this.myCANAddress then

                  declare
                     sentTo : VN.Communication.CAN.CAN_Address_Receiver;
                     sentBy : VN.Communication.CAN.CAN_Address_Sender;
                  begin
                     VN.Communication.CAN.Logic.Message_Utils.FlowControlFromMessage(msgIn, sentTo, sentBy, this.useFlowControl, this.blockSize);

                     if (sentBy = this.receiver) and then (sentTo = this.myCANAddress) then

                        VN.Communication.CAN.Logic.DebugOutput("Sender_Unit with address " & this.myCANAddress'Img &
                                               " received Flow control message from address " & msgIn.Sender'Img &
                                               ", useFlowCtrl= " & this.useFlowControl'Img & " blockSize= " & this.blockSize'img, 4);

                        this.currentState := Transmitting;
                        this.blockCount := 0;
                        this.sequenceNumber := 0;
                     end if;
                  end;
               end if;
            end if;

            --resends the StartTransmission message if no FlowControl message is received
            if this.currentState = Started and Ada.Real_Time.Clock - this.time > WAIT_TIME then
               VN.Communication.CAN.Logic.Message_Utils.StartTransmissionToMessage(msgOut, this.receiver, this.myCANAddress,
                                                                 NumMessagesToSend(this.ToSend.NumBytes));

               VN.Communication.CAN.Logic.DebugOutput("StartTransmission message was resent from address " & this.myCANAddress'img
                                    & " to " & this.receiver'img & " numMessages= " & NumMessagesToSend(this.ToSend.NumBytes)'img, 4);

               this.time := Ada.Real_Time.Clock;
               this.currentState := Started;
               bWillSend := true;
            else
               bWillSend := false;
            end if;

         when Transmitting =>
            declare
               isLastMessage : boolean;
            begin

               VN.Communication.CAN.Logic.Message_Utils.TransmissionToMessage(msgOut, this.receiver, this.myCANAddress);
               Fragment(this.ToSend, this.sequenceNumber, msgOut, isLastMessage); --this also increments sequenceNumber
               bWillSend := true;

               VN.Communication.CAN.Logic.DebugOutput("Sender_Unit sent Transmission message", 4);
               if isLastMessage then
                  this.currentState := Idle;
                   VN.Communication.CAN.Logic.DebugOutput("Sender_Unit: Transmission done, went Idle", 4);
               else

                  this.blockCount := this.blockCount + 1;

                  if this.blockCount = this.blockSize then
                     this.currentState := BlockFull;
                     VN.Communication.CAN.Logic.DebugOutput("Sender_Unit whole block sent", 4);
                  else
                     this.currentState := Transmitting;
                  end if;
               end if;
            end;

         when BlockFull =>
           VN.Communication.CAN.Logic.DebugOutput("Sender_Unit in state BlockFull", 5);

            if bMsgReceived and then msgIn.isNormal and then msgIn.msgType = VN.Communication.CAN.Logic.FLOW_CONTROL then
               if msgIn.Sender = this.receiver and then msgIn.Receiver = this.myCANAddress then

                 VN.Communication.CAN.Logic.DebugOutput("Received Flow control message, blockCount reset", 4);

                  this.currentState := Transmitting;
                  this.blockCount := 0;
               end if;
           -- else
               --VN.Communication.CAN.Logic.DebugOutput("", 5, true); --newline               
            end if;
            bWillSend := false;
      end case;

   end Update;

   procedure Send(this : in out Sender_Unit_Duty; 
                  message : VN.Communication.CAN.Logic.VN_Message_Internal) is
   begin
      this.currentState := Initiated;
      this.ToSend := message;
   end Send;

   procedure Activate(this : in out Sender_Unit_Duty; address : VN.Communication.CAN.CAN_Address_Sender) is
   begin
       this.myCANAddress := address;
   end Activate;

   function isActive(this : in Sender_Unit_Duty) return boolean is
   begin
     return not (this.currentState = Idle);
   end isActive;

   function Receiver(this : in Sender_Unit_Duty) return VN.Communication.CAN.CAN_Address_Receiver is
   begin
      return this.ToSend.Receiver;
   end Receiver;

   function NumMessagesToSend(messageLength : Interfaces.Unsigned_16) return Interfaces.Unsigned_16 is
   begin
      if messageLength rem 8 = 0 then
         return messageLength / 8;
      else
         return messageLength / 8 + 1;
      end if;
   end NumMessagesToSend;


   procedure Fragment(VNMessage : VN.Communication.CAN.Logic.VN_Message_Internal; 
                      seqNumber : in out Interfaces.Unsigned_16;
                      CANMessage : in out VN.Communication.CAN.CAN_Message_Logical; isLastMessage : out  boolean) is

      procedure CharTou8(u8 : out Interfaces.Unsigned_8; c : in Character) is
         x : Character;
         for x'Address use u8'Address;
      begin
         x := c;
      end CharTou8;

      Last : integer;
   begin

      -- if the next Transmission message should be full (contain 8 bytes)
      --but this is not the last CAN message to be sent
      if (seqNumber + 1) * 8 < VNMessage.NumBytes then
         Last := 7;
         isLastMessage := false;

         -- if the next Transmission message should be full (contain 8 bytes)
         --and this is the last CAN message to be sent
      elsif (seqNumber + 1) * 8 = VNMessage.NumBytes then
         Last := 7;
         isLastMessage := true;

      else -- if the next Transmission message should contain less than 8 bytes
         Last := Integer(VNMessage.NumBytes - seqNumber * 8) - 1;
         isLastMessage := true;
      end if;

      CANMessage.Length := VN.Communication.CAN.DLC_Type(Last + 1);


--TODO: Get this to work, needs redoing VN.Message.VN_Message_Basic

--        for i in 0..Last loop
--           --           CANMessage.Data(CANMessage.Data'First + VN.Communication.CAN.Logic.DLC_Type(i)) :=
--           --             VNMessage.Data(VNMessage.Data'First + Integer(seqNumber) * 8 + i);
--           CharTou8(CANMessage.Data(CANMessage.Data'First + VN.Communication.CAN.DLC_Type(i)),
--                    VNMessage.Data(VNMessage.Data'First + Integer(seqNumber) * 8 + i));
--        end loop;

      seqNumber := seqNumber + 1;
   end Fragment;

end VN.Communication.CAN.Logic.Sender_Unit;

