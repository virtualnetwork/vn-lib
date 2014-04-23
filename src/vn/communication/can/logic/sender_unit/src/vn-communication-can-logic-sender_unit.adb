-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary: 
-- Implements the state machine for sending a single VN message. Sender_Duty will
-- use an instane of Sender_Unit_Duty to send a VN message.
-- Before it can be used, Sender_Unit_Duty will need to be activated. This cannot
-- be done until one has been assigned a CAN address.

-- ToDo: Fragment must be tested 

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
                                                              NumMessagesToSend(this.numBytesToSend));

            VN.Communication.CAN.Logic.DebugOutput("StartTransmission message sent from address " & this.myCANAddress'img
                                 & " to " & this.receiver'img & " numMessages= " & NumMessagesToSend(this.numBytesToSend)'img, 3);

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
                                                                 NumMessagesToSend(this.numBytesToSend));

               VN.Communication.CAN.Logic.DebugOutput("StartTransmission message was resent from address " & this.myCANAddress'img
                                    & " to " & this.receiver'img & " numMessages= " & NumMessagesToSend(this.numBytesToSend)'img, 3);

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
               
               VN.Communication.CAN.Logic.DebugOutput("Sender_Unit sent Transmission message from  CAN adr" & this.myCANAddress'img & " to " & this.Receiver'img, 4);
               VN.Communication.CAN.Logic.Message_Utils.Fragment(this.ToSend, this.sequenceNumber, this.numBytesToSend, msgOut, isLastMessage); --this also increments sequenceNumber
               bWillSend := true;

               
               if isLastMessage then
                  this.currentState := Idle;
                   VN.Communication.CAN.Logic.DebugOutput("Sender_Unit: Transmission done, went Idle", 3);
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
      VN.Message.Serialize(message.Data, this.ToSend);
      this.numBytesToSend := message.NumBytes;   
      this.Receiver := message.Receiver;     

      VN.Communication.CAN.Logic.DebugOutput("Send VN Message: Sender CAN addr " & this.myCANAddress'Img & 
                                               ", receiver= " & this.Receiver'Img & " NumBytes= " & message.NumBytes'Img & 
                                               " Opcode= " & message.Data.Header.Opcode'img, 3);

--        VN.Communication.CAN.Logic.DebugOutput("", 4);
--        VN.Communication.CAN.Logic.DebugOutput("Sender_Unit_Duty.Send, opcode= " & message.Data.Header.Opcode'Img, 4);
--        VN.Communication.CAN.Logic.DebugOutput("", 4);
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
      return this.Receiver;
   end Receiver;

   function NumMessagesToSend(messageLength : Interfaces.Unsigned_16) return Interfaces.Unsigned_16 is
   begin
      if messageLength rem 8 = 0 then
         return messageLength / 8;
      else
         return messageLength / 8 + 1;
      end if;
   end NumMessagesToSend;
end VN.Communication.CAN.Logic.Sender_Unit;

