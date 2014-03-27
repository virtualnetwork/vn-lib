-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- Contains functions for coding and decoding CAN messages, i.e. putting
-- information into CAN messages and retrieving information from CAN messages.


with VN.Communication.CAN.Logic;

package VN.Communication.CAN.Logic.Message_Utils is

--     type MessageTypeEnum is (UNKNOWN, ANY_NORMAL_CAN_MSG, REQUEST_CAN_ADDRESS, ASSIGN_CAN_ADDRESS,
--                              CAN_MASTER_ASSIGNED, ADDRESS_QUESTION, ADDRESS_ANSWER, PING, PONG,
--                              START_TRANSMISSION, FLOW_CONTROL, TRANSMISSION, FIRST_CUUID_HALF, SECOND_CUUID_HALF);
--     for MessageTypeEnum use
--       (UNKNOWN	 	  => -3,
--        ANY_NORMAL_CAN_MSG  => -2,
--        REQUEST_CAN_ADDRESS => -1,
--        ASSIGN_CAN_ADDRESS  => 0,
--        CAN_MASTER_ASSIGNED => 1,
--        ADDRESS_QUESTION    => 2,
--        ADDRESS_ANSWER	  => 3,
--        PING		  => 4,
--        PONG		  => 5,
--        START_TRANSMISSION  => 6,
--        FLOW_CONTROL	  => 7,
--        TRANSMISSION	  => 8,
--        FIRST_CUUID_HALF	  => 9,
--        SECOND_CUUID_HALF   => 10);


   procedure AddressQuestionToMessage(msg : out VN.Communication.CAN.CAN_Message_Logical; receiver : VN.Communication.CAN.CAN_Address_Receiver;
                                      sender : VN.Communication.CAN.CAN_Address_Sender; logicalAddress : VN.Communication.CAN.Logic.Logical_Address; prio : CAN_Message_Prio);

   procedure AddressQuestionFromMessage(msg : VN.Communication.CAN.CAN_Message_Logical; logicalAddress : out VN.Communication.CAN.Logic.Logical_Address);

   procedure AddressAnswerToMessage(msg : out VN.Communication.CAN.CAN_Message_Logical; receiver : VN.Communication.CAN.CAN_Address_Receiver; sender : VN.Communication.CAN.CAN_Address_Sender;
                                    CANAddress : VN.Communication.CAN.CAN_Address_Sender; logicalAddress : VN.Communication.CAN.Logic.Logical_Address; prio : CAN_Message_Prio);

   procedure AddressAnswerFromMessage(msg : VN.Communication.CAN.CAN_Message_Logical; CANAddress : out VN.Communication.CAN.CAN_Address_Sender;
                                      logicalAddress : out VN.Communication.CAN.Logic.Logical_Address);

   procedure AssignLogicalAddressToMessage(msg : out VN.Communication.CAN.CAN_Message_Logical; receiver : VN.Communication.CAN.CAN_Address_Receiver; sender : VN.Communication.CAN.CAN_Address_Sender;
                                           prio : CAN_Message_Prio; logicalAddress : VN.Communication.CAN.Logic.Logical_Address);

   procedure AssignLogicalAddressFromMessage(msg : VN.Communication.CAN.CAN_Message_Logical; logicalAddress : out VN.Communication.CAN.Logic.Logical_Address);

   procedure ComponentTypeToMessage(msg : out VN.Communication.CAN.CAN_Message_Logical; sender : VN.Communication.CAN.CAN_Address_Sender; prio : CAN_Message_Prio; isSM_CAN : boolean);

   procedure ComponentTypeFromMessage(msg : VN.Communication.CAN.CAN_Message_Logical; isSM_CAN : out boolean);

   procedure RequestCUUIDToMessage(msg : out VN.Communication.CAN.CAN_Message_Logical; sender : VN.Communication.CAN.CAN_Address_Sender; prio : CAN_Message_Prio);

   procedure CUUIDHalfToMessage(msg : out VN.Communication.CAN.CAN_Message_Logical; sender : VN.Communication.CAN.CAN_Address_Sender;
                                theCUUID : VN.Communication.CAN.Logic.CUUID; firstHalf : Boolean);

   procedure CUUIDHalfFromMessage(msg : VN.Communication.CAN.CAN_Message_Logical; theCUUID : in out VN.Communication.CAN.Logic.CUUID; firstHalf : Boolean);

   procedure TransmissionToMessage(msg : out VN.Communication.CAN.CAN_Message_Logical; receiver : VN.Communication.CAN.CAN_Address_Receiver;
                                        sender : VN.Communication.CAN.CAN_Address_Sender);

--     procedure TransmissionFromMessage(msg : VN.Communication.CAN.CAN_Message_Logical; receiver : out VN.Communication.CAN.CAN_Address_Receiver;
--                                            sender : out VN.Communication.CAN.CAN_Address_Sender);

   procedure FlowControlToMessage(msg : out VN.Communication.CAN.CAN_Message_Logical; receiver : VN.Communication.CAN.CAN_Address_Receiver;
                                  sender : VN.Communication.CAN.CAN_Address_Sender; useFlowControl : boolean;
                                  blockSize : Interfaces.Unsigned_16);

   procedure FlowControlFromMessage(msg : VN.Communication.CAN.CAN_Message_Logical; receiver : out VN.Communication.CAN.CAN_Address_Receiver;
                                    sender : out VN.Communication.CAN.CAN_Address_Sender; useFlowControl : out boolean;
                                    blockSize : out Interfaces.Unsigned_16);

   procedure StartTransmissionToMessage(msg : out VN.Communication.CAN.CAN_Message_Logical; receiver : VN.Communication.CAN.CAN_Address_Receiver;
                                        sender : VN.Communication.CAN.CAN_Address_Sender; numMessages : Interfaces.Unsigned_16);

   procedure StartTransmissionFromMessage(msg : VN.Communication.CAN.CAN_Message_Logical; receiver : out VN.Communication.CAN.CAN_Address_Receiver;
                                          sender : out VN.Communication.CAN.CAN_Address_Sender; numMessages : out Interfaces.Unsigned_16);

   procedure AssignCANAddressToMessage(msg : out VN.Communication.CAN.CAN_Message_Logical;
                                       theUCID : VN.Communication.CAN.UCID; theCANAddr : VN.Communication.CAN.CAN_Address_Sender);

   procedure AssignCANAddressFromMessage(msg : VN.Communication.CAN.CAN_Message_Logical;
                                          theUCID : out VN.Communication.CAN.UCID; theCANAddr : out VN.Communication.CAN.CAN_Address_Sender);

   procedure RequestCANAddressToMessage(msg : out VN.Communication.CAN.CAN_Message_Logical;
                                        theUCID : VN.Communication.CAN.UCID; bIs_SM_CAN : boolean);

   procedure RequestCANAddressFromMessage(msg : VN.Communication.CAN.CAN_Message_Logical;
                                          theUCID : out VN.Communication.CAN.UCID; bIs_SM_CAN : out boolean);

   procedure CANMasterAssignedToMessage(msg : out VN.Communication.CAN.CAN_Message_Logical; prio : CAN_Message_Prio);

   function GetMessageID(msgPrio : VN.Communication.CAN.CAN_Message_Prio; msgType : VN.Communication.CAN.CAN_Message_Type;
                         receiver : VN.Communication.CAN.CAN_Address_Receiver; myAddress : VN.Communication.CAN.CAN_Address_Sender) return VN.Communication.CAN.CAN_message_ID;

private

   type TwoBytes is array(VN.Communication.CAN.Byte8'First .. VN.Communication.CAN.Byte8'First + 1) of Interfaces.Unsigned_8;
   for TwoBytes'Size use 16;

   type FourBytes is array(VN.Communication.CAN.Byte8'First .. VN.Communication.CAN.Byte8'First + 3) of Interfaces.Unsigned_8;
   for FourBytes'Size use 32;

   procedure DataToU16(Data : VN.Communication.CAN.Byte8; u16 : out Interfaces.Unsigned_16);

   procedure U16ToData(u16 : Interfaces.Unsigned_16; Data : out VN.Communication.CAN.Byte8);

end VN.Communication.CAN.Logic.Message_Utils;
