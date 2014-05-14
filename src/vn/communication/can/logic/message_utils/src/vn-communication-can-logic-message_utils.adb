-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- Contains functions for coding and decoding CAN messages, i.e. putting
-- information into CAN messages and retrieving information from CAN messages.

with Interfaces;
with Ada.Exceptions;

package body VN.Communication.CAN.Logic.Message_Utils is


   procedure AddressQuestionToMessage(msg : out VN.Communication.CAN.CAN_Message_Logical; receiver : VN.Communication.CAN.CAN_Address_Receiver;
                                      sender : VN.Communication.CAN.CAN_Address_Sender; logicalAddress : VN.VN_Logical_Address;
                                      prio : VN.Communication.CAN.CAN_Message_Prio) is
   begin
      msg.isNormal := true;
      msg.msgPrio  := prio;
      msg.msgType  := VN.Communication.CAN.Logic.ADDRESS_QUESTION;
      msg.Receiver := receiver;
      msg.Sender   := sender;
      msg.Length   := 4;

      U16ToData(Interfaces.Unsigned_16(logicalAddress), msg.Data);
   end AddressQuestionToMessage;

   procedure AddressQuestionFromMessage(msg : VN.Communication.CAN.CAN_Message_Logical; logicalAddress : out VN.VN_Logical_Address) is
      INCORRECT_MESSAGE_AddressQuestion : exception;
      temp : Interfaces.Unsigned_16;
   begin
      if msg.isNormal and msg.msgType = VN.Communication.CAN.Logic.ADDRESS_QUESTION and msg.Length = 4 then
         DataToU16(msg.Data, temp);
         logicalAddress := VN.VN_Logical_Address(temp);
      else
         raise INCORRECT_MESSAGE_AddressQuestion;
      end if;
   end AddressQuestionFromMessage;

   procedure AddressAnswerToMessage(msg : out VN.Communication.CAN.CAN_Message_Logical; receiver : VN.Communication.CAN.CAN_Address_Receiver; sender : VN.Communication.CAN.CAN_Address_Sender;
                                    CANAddress : VN.Communication.CAN.CAN_Address_Sender; logicalAddress : VN.VN_Logical_Address; prio : VN.Communication.CAN.CAN_Message_Prio) is
      temp : FourBytes;
      for temp'Address use logicalAddress'Address;
   begin

      msg.isNormal := true;
      msg.msgPrio  := prio;
      msg.msgType  := VN.Communication.CAN.Logic.ADDRESS_ANSWER;
      msg.Receiver := receiver;
      msg.Sender   := sender;
      msg.Length   := 5;

      for i in temp'Range loop
         msg.Data(i) := temp(i);
      end loop;

      msg.Data(msg.Data'First + 4) := Interfaces.Unsigned_8(CANAddress);
   end AddressAnswerToMessage;


   procedure AddressAnswerFromMessage(msg : VN.Communication.CAN.CAN_Message_Logical; CANAddress : out VN.Communication.CAN.CAN_Address_Sender;
                                      logicalAddress : out VN.VN_Logical_Address) is
      INCORRECT_MESSAGE_AddressAnswer : exception;
      temp : FourBytes;
      for temp'Address use logicalAddress'Address;

   begin
      if msg.isNormal and msg.msgType = VN.Communication.CAN.Logic.ADDRESS_ANSWER and msg.Length = 5 then

         for i in temp'Range loop
            temp(i) := msg.Data(i);
         end loop;

         CANAddress := VN.Communication.CAN.CAN_Address_Sender(msg.Data(msg.Data'First + 4));
      else
         raise INCORRECT_MESSAGE_AddressAnswer;
      end if;
   end AddressAnswerFromMessage;

   procedure ComponentTypeToMessage(msg : out VN.Communication.CAN.CAN_Message_Logical;  sender : VN.Communication.CAN.CAN_Address_Sender; prio : VN.Communication.CAN.CAN_Message_Prio; isSM_CAN : boolean) is
   begin
      msg.isNormal := true;
      msg.msgPrio  := prio;
      msg.msgType  := VN.Communication.CAN.Logic.COMPONENT_TYPE;
      msg.Receiver := 255;
      msg.Sender   := sender;
      msg.Length   := 1;

      if isSM_CAN then
         msg.Data(msg.Data'First) := 3;
      else
         msg.Data(msg.Data'First) := 5;
      end if;
   end ComponentTypeToMessage;


   procedure ComponentTypeFromMessage(msg : VN.Communication.CAN.CAN_Message_Logical;  isSM_CAN : out boolean) is
      INCORRECT_MESSAGE_ComponentType : exception;
   begin
      if msg.isNormal and msg.msgType = VN.Communication.CAN.Logic.COMPONENT_TYPE and msg.Length = 1 then
         if msg.Data(msg.Data'First) = 3 then
            isSM_CAN := true;
         else
            isSM_CAN := false;
         end if;
      else
         raise INCORRECT_MESSAGE_ComponentType;
      end if;
   end ComponentTypeFromMessage;

   procedure DiscoveryRequestToMessage(msg : out VN.Communication.CAN.CAN_Message_Logical;  sender : VN.Communication.CAN.CAN_Address_Sender; prio : VN.Communication.CAN.CAN_Message_Prio) is
   begin
      msg.isNormal := true;
      msg.msgPrio  := prio;
      msg.msgType  := VN.Communication.CAN.Logic.DISCOVERY_REQUEST;
      msg.Receiver := 254;
      msg.Sender   := sender;
      msg.Length   := 0;
   end DiscoveryRequestToMessage;


--     procedure CUUIDHalfToMessage(msg : out VN.Communication.CAN.CAN_Message_Logical;  sender : VN.Communication.CAN.CAN_Address_Sender;
--                                  theCUUID : VN.VN_CUUID; firstHalf : Boolean) is
--        offset : VN.Communication.CAN.DLC_Type;
--     begin
--        msg.isNormal := true;
--        msg.msgPrio  := 0;
--        msg.Receiver := 254;
--        msg.Sender   := sender;
--        msg.Length   := 8;
--
--        if firstHalf then
--           msg.msgType  := VN.Communication.CAN.Logic.FIRST_CUUID_HALF;
--           offset := 0;
--        else
--           msg.msgType  := VN.Communication.CAN.Logic.SECOND_CUUID_HALF;
--           offset := 8;
--        end if;
--
--        for i in msg.Data'Range loop
--           msg.Data(i) := theCUUID(Integer(offset + i));
--        end loop;
--     end CUUIDHalfToMessage;


--     procedure CUUIDHalfFromMessage(msg : VN.Communication.CAN.CAN_Message_Logical; theCUUID : in out VN.VN_CUUID; firstHalf : Boolean) is
--
--        INCORRECT_MESSAGE_CUUID_Half : exception;
--        offset : VN.Communication.CAN.DLC_Type;
--     begin
--
--        if msg.isNormal and msg.Length = 8 and
--          (msg.msgType = VN.Communication.CAN.Logic.FIRST_CUUID_HALF or msg.msgType = VN.Communication.CAN.Logic.SECOND_CUUID_HALF) then
--
--           if firstHalf then
--              offset := 0;
--           else
--              offset := 8;
--           end if;
--
--           for i in msg.Data'Range loop
--              theCUUID(Integer(offset + i)) := msg.Data(i);
--           end loop;
--        else
--           raise INCORRECT_MESSAGE_CUUID_Half;
--        end if;
--     end CUUIDHalfFromMessage;


   procedure TransmissionToMessage(msg : out VN.Communication.CAN.CAN_Message_Logical; receiver : VN.Communication.CAN.CAN_Address_Receiver;
                                   sender : VN.Communication.CAN.CAN_Address_Sender) is
   begin
      msg.isNormal 	:= true;
      msg.msgPrio 	:= 0;
      msg.msgType 	:= VN.Communication.CAN.Logic.TRANSMISSION;
      msg.Receiver 	:= receiver;
      msg.Sender 	:= sender;
   end TransmissionToMessage;

   procedure FlowControlToMessage(msg : out VN.Communication.CAN.CAN_Message_Logical; receiver : VN.Communication.CAN.CAN_Address_Receiver;
                                  sender : VN.Communication.CAN.CAN_Address_Sender; useFlowControl : boolean;
                                  blockSize : Interfaces.Unsigned_16) is
   begin
      msg.isNormal 	:= true;
      msg.msgPrio 	:= 0;
      msg.msgType 	:= VN.Communication.CAN.Logic.FLOW_CONTROL;
      msg.receiver 	:= Receiver;
      msg.sender 	:= Sender;

      if useFlowControl then
         msg.Length	:= 2;
         U16ToData(blockSize, msg.Data);
      else
         msg.Length	:= 0;
      end if;
   end FlowControlToMessage;

   procedure FlowControlFromMessage(msg : VN.Communication.CAN.CAN_Message_Logical; receiver : out VN.Communication.CAN.CAN_Address_Receiver;
                                    sender : out VN.Communication.CAN.CAN_Address_Sender; useFlowControl : out boolean;
                                    blockSize : out Interfaces.Unsigned_16) is
      INCORRECT_MESSAGE_FlowControl : exception;
   begin
      if msg.isNormal and msg.msgType = VN.Communication.CAN.Logic.FLOW_CONTROL and (msg.Length = 2 or msg.Length = 0) then
         receiver := msg.Receiver;
         sender   := msg.Sender;
         if msg.Length = 2 then
            useFlowControl := true;
            DataToU16(msg.Data, blockSize);

            if blockSize = 0 then
               useFlowControl := false;
            end if;
         else
            useFlowControl := false;
         end if;
      else
         raise INCORRECT_MESSAGE_FlowControl;
      end if;
   end FlowControlFromMessage;

   procedure StartTransmissionToMessage(msg : out VN.Communication.CAN.CAN_Message_Logical; receiver : VN.Communication.CAN.CAN_Address_Receiver;
                                        sender : VN.Communication.CAN.CAN_Address_Sender; numMessages : Interfaces.Unsigned_16) is
   begin
      msg.isNormal 	:= true;
      msg.msgPrio 	:= 0;
      msg.msgType 	:= VN.Communication.CAN.Logic.START_TRANSMISSION;
      msg.receiver 	:= Receiver;
      msg.sender 	:= Sender;
      msg.Length	:= 2;

      U16ToData(numMessages, msg.Data);
   end StartTransmissionToMessage;


   procedure StartTransmissionFromMessage(msg : VN.Communication.CAN.CAN_Message_Logical; receiver : out VN.Communication.CAN.CAN_Address_Receiver;
                                          sender : out VN.Communication.CAN.CAN_Address_Sender; numMessages : out Interfaces.Unsigned_16) is

      INCORRECT_MESSAGE_StartTransmission : exception;
   begin

      if msg.isNormal and msg.msgType = VN.Communication.CAN.Logic.START_TRANSMISSION and msg.Length = 2 then
         DataToU16(msg.Data, numMessages);
         receiver    := msg.Receiver;
         sender      := msg.Sender;
      else
         raise INCORRECT_MESSAGE_StartTransmission;
      end if;
   end StartTransmissionFromMessage;



   procedure AssignCANAddressToMessage(msg : out VN.Communication.CAN.CAN_Message_Logical;
                                       theUCID : VN.Communication.CAN.UCID; theCANAddr : VN.Communication.CAN.CAN_Address_Sender) is
      x : FourBytes;
      y : VN.Communication.CAN.UCID;
      for x'Address use y'Address;
   begin
      y := theUCID;
      for i in x'Range loop
         msg.Data(VN.Communication.CAN.DLC_Type(i)) := x(i);
      end loop;

      msg.Data(msg.Data'First + 4) := Interfaces.Unsigned_8(theCANAddr);

      msg.isNormal := true;
      msg.msgPrio := 0;
      msg.msgType := VN.Communication.CAN.Logic.ASSIGN_CAN_ADDRESS;
      msg.receiver := 255;
      msg.sender := 0;
      msg.Length := 5;
   end AssignCANAddressToMessage;

   procedure AssignCANAddressFromMessage(msg : VN.Communication.CAN.CAN_Message_Logical;
                                         theUCID : out VN.Communication.CAN.UCID; theCANAddr : out VN.Communication.CAN.CAN_Address_Sender) is
      INCORRECT_MESSAGE_AssignCANAddress : exception;
      x : FourBytes;
      y : VN.Communication.CAN.UCID;
      for x'Address use y'Address;
   begin
      if msg.isNormal and msg.msgType = VN.Communication.CAN.Logic.ASSIGN_CAN_ADDRESS and msg.Length = 5 then
         for i in x'Range loop
            x(i) := msg.Data(VN.Communication.CAN.DLC_Type(i));
         end loop;
         theUCID := y;
         theCANAddr := VN.Communication.CAN.CAN_Address_Sender(msg.Data(msg.Data'First + 4));
      else
         raise INCORRECT_MESSAGE_AssignCANAddress;
      end if;
   end AssignCANAddressFromMessage;


   procedure RequestCANAddressToMessage(msg : out VN.Communication.CAN.CAN_Message_Logical;
                                        theUCID : VN.Communication.CAN.UCID; bIs_SM_CAN : boolean) is
   begin
      msg.isNormal:=false;
      msg.senderUCID:= theUCID;
      if bIs_SM_CAN then
         msg.Length  := 1;
         msg.Data(1) := 3;
      else
         msg.Length  := 0;
      end if;
   end RequestCANAddressToMessage;


   procedure RequestCANAddressFromMessage(msg : VN.Communication.CAN.CAN_Message_Logical;
                                          theUCID : out VN.Communication.CAN.UCID; bIs_SM_CAN : out boolean) is
   begin
      theUCID := msg.senderUCID;
      if msg.Length > 0 then
         if msg.Data(1) = 3 then
            bIs_SM_CAN := true;
            return;
         end if;
      end if;
      bIs_SM_CAN := false;
   end RequestCANAddressFromMessage;

   procedure CANMasterAssignedToMessage(msg : out VN.Communication.CAN.CAN_Message_Logical; prio : VN.Communication.CAN.CAN_Message_Prio) is
   begin
      msg.Sender   := 0;
      msg.Receiver := 255;
      msg.Length   := 0;
      msg.isNormal := true;
      msg.msgPrio  := prio;
      msg.msgType  := VN.Communication.CAN.Logic.CAN_MASTER_ASSIGNED;
   end CANMasterAssignedToMessage;

   function GetMessageID(msgPrio : VN.Communication.CAN.CAN_Message_Prio; msgType : VN.Communication.CAN.CAN_Message_Type;
                         receiver : VN.Communication.CAN.CAN_Address_Receiver; myAddress : VN.Communication.CAN.CAN_Address_Sender) return VN.Communication.CAN.CAN_message_ID is

      u32Prio     : Interfaces.Unsigned_32 := Interfaces.Unsigned_32(msgPrio);
      u32Type 	  : Interfaces.Unsigned_32 := Interfaces.Unsigned_32(msgType);
      u32Receiver : Interfaces.Unsigned_32 := Interfaces.Unsigned_32(receiver);
      u32Sender	  : Interfaces.Unsigned_32 := Interfaces.Unsigned_32(myAddress);
      total 	  : Interfaces.Unsigned_32 := 0;

   begin

      total := 	Interfaces.Shift_Left(u32Prio, VN.Communication.CAN.OFFSET_CAN_PRIORITY) +
        Interfaces.Shift_Left(u32Type, VN.Communication.CAN.OFFSET_CAN_TYPE) +
        Interfaces.Shift_Left(u32Receiver, VN.Communication.CAN.OFFSET_CAN_RECEIVER) +
        Interfaces.Shift_Left(u32Sender, VN.Communication.CAN.OFFSET_CAN_SENDER);

      return VN.Communication.CAN.CAN_message_ID(total);
   end GetMessageID;

   procedure Fragment(msgArray 		: VN.Message.VN_Message_Byte_Array;
                      seqNumber 	: in out Interfaces.Unsigned_16;
                      NumBytes 		: in Interfaces.Unsigned_16;
                      CANMessage 	: in out VN.Communication.CAN.CAN_Message_Logical;
                      isLastMessage 	: out  boolean) is

      Last, index, startIndex : integer; -- 0-based indices

      Fragment_Error : exception;
   begin

      -- if the next Transmission message should be full (contain 8 bytes)
      --but this is not the last CAN message to be sent
      if (seqNumber + 1) * 8 < NumBytes then
         Last := 7;
         isLastMessage := false;

         -- if the next Transmission message should be full (contain 8 bytes)
         --and this is the last CAN message to be sent
      elsif (seqNumber + 1) * 8 = NumBytes then
         Last := 7;
         isLastMessage := true;

      else -- if the next Transmission message should contain less than 8 bytes
         Last := Integer(NumBytes - seqNumber * 8) - 1;
         isLastMessage := true;
      end if;

      CANMessage.Length := VN.Communication.CAN.DLC_Type(Last + 1);

      startIndex := Integer(seqNumber) * 8; --where in the msgArray the first byte should be taken

      for i in 0..Last loop
         index := i + startIndex;

         -- If we are to take the last two bytes (the Checksum), these are always placed
         -- at the last to indices of the array.
         if index = Integer(NumBytes) - 1 then
            index := msgArray'Last - msgArray'First;
         elsif index = Integer(NumBytes) - 2 then
            index := msgArray'Last - msgArray'First - 1;
         end if;

         if CANMessage.Data'First + VN.Communication.CAN.DLC_Type(i) > CANMessage.Data'Last then
            Ada.Exceptions.Raise_Exception(Fragment_Error'Identity, "CANMessage error, i= " & i'Img);
         end if;

         if msgArray'First + index > msgArray'Last then
             Ada.Exceptions.Raise_Exception(Fragment_Error'Identity, "msgArray error, index= " & index'Img & " seqNumber= " & seqNumber'img);
         end if;

         CANMessage.Data(CANMessage.Data'First + VN.Communication.CAN.DLC_Type(i)) :=
           msgArray(msgArray'First + index);


         declare
            temp : integer := msgArray'First + index;
         begin
            VN.Communication.CAN.Logic.DebugOutput("Fragment. Array(" & temp'Img & ") := " & msgArray(temp)'img, 6);
         end;

--           declare
--              temp : integer := msgArray'First + index;
--           begin
--              VN.Text_IO.Put_Line("Fragment, Array(" & temp'Img & ") := " & msgArray(temp)'img);
--           end;

         --reverse index on msgArray:
--           CANMessage.Data(CANMessage.Data'First + VN.Communication.CAN.DLC_Type(i)) :=
--             msgArray(msgArray'Last - index);
      end loop;

      seqNumber := seqNumber + 1;
   end Fragment;

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

         declare
            temp : integer := VNMessageContent'First + index;
         begin
            VN.Communication.CAN.Logic.DebugOutput("DeFragment. Array(" & temp'Img & ") := " & VNMessageContent(temp)'img, 6);
         end;


         --reverse index on VNMessageContent:
--           VNMessageContent(VNMessageContent'Last - index) :=
--             CANMessage.Data(CANMessage.Data'First + i);
      end loop;

      currentLength := seqNumber * 8 + Interfaces.Unsigned_16(CANMessage.Length);

      -- If the last CAN message has been received, move the
      -- two last received bytes to the end of the array.
      -- These two bytes are the checksum and should allways be put at the end of the array.
      if seqNumber + 1 = numMessages then
         lastIndex := startIndex + Integer(CANMessage.Length);

         VNMessageContent(VNMessageContent'Last)     :=  VNMessageContent(lastIndex);
         VNMessageContent(VNMessageContent'Last - 1) :=  VNMessageContent(lastIndex - 1);

         declare
            temp : integer := VNMessageContent'Last - 1;
         begin
            VN.Communication.CAN.Logic.DebugOutput("DeFragment. Last message. Array(" & temp'Img & ") := " & VNMessageContent(VNMessageContent'Last - 1)'img &
                                                     ",  Array(" & VNMessageContent'Last'Img & ") := " & VNMessageContent(VNMessageContent'Last )'img, 6);
         end;

         --reverse index on VNMessageContent:
--           VNMessageContent(VNMessageContent'First)     :=  VNMessageContent(VNMessageContent'Last - lastIndex);
--           VNMessageContent(VNMessageContent'First + 1) :=  VNMessageContent(VNMessageContent'Last - lastIndex - 1);
      end if;
   end DeFragment;


   procedure DataToU16(Data : VN.Communication.CAN.Byte8; u16 : out Interfaces.Unsigned_16) is
      x : TwoBytes;
      y : Interfaces.Unsigned_16;
      for x'Address use y'Address;
   begin
      for i in x'Range loop
         x(i) := Data(VN.Communication.CAN.DLC_Type(i));
      end loop;
      u16 := y;
   end DataToU16;

   procedure U16ToData(u16 : Interfaces.Unsigned_16; Data : out VN.Communication.CAN.Byte8) is
      x : TwoBytes;
      y : Interfaces.Unsigned_16;
      for x'Address use y'Address;
   begin
      y := u16;
      for i in x'Range loop
         Data(VN.Communication.CAN.DLC_Type(i)) := x(i);
      end loop;
   end U16ToData;

end VN.Communication.CAN.Logic.Message_Utils;

