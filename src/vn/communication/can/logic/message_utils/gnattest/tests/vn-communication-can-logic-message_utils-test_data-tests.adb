--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into VN.Communication.CAN.Logic.Message_Utils.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

with VN.Message;
use VN.Message;

with VN.Message.Factory;
with VN.Message.Local_Hello;

package body VN.Communication.CAN.Logic.Message_Utils.Test_Data.Tests is


--  begin read only
   procedure Test_AddressQuestionToMessage (Gnattest_T : in out Test);
   procedure Test_AddressQuestionToMessage_8f619b (Gnattest_T : in out Test) renames Test_AddressQuestionToMessage;
--  id:2.1/8f619b4e13e55896/AddressQuestionToMessage/1/0/
   procedure Test_AddressQuestionToMessage (Gnattest_T : in out Test) is
   --  vn-communication-can-logic-message_utils.ads:33:4:AddressQuestionToMessage
--  end read only

       msg : VN.Communication.CAN.CAN_Message_Logical;
      logicalAddr, logicalAddr2 : VN.VN_Logical_Address;


      sender : VN.Communication.CAN.CAN_Address_Sender := 65;
      receiver : VN.Communication.CAN.CAN_Address_Receiver := 26;

   begin

      logicalAddr := 9693;

      AddressQuestionToMessage(msg, receiver, sender, logicalAddr, 0);

      AUnit.Assertions.Assert
        (msg.msgType = VN.Communication.CAN.Logic.ADDRESS_QUESTION and msg.isNormal and msg.Receiver = receiver and msg.Sender=sender and msg.Length=4,
         "AddressQuestionToMessage failed. 1");

      AddressQuestionFromMessage(msg, logicalAddr2);

      AUnit.Assertions.Assert
        (logicalAddr = logicalAddr2,
         "AddressQuestionToMessage failed. 2");

--  begin read only
   end Test_AddressQuestionToMessage;
--  end read only


--  begin read only
   procedure Test_AddressQuestionFromMessage (Gnattest_T : in out Test);
   procedure Test_AddressQuestionFromMessage_7ff120 (Gnattest_T : in out Test) renames Test_AddressQuestionFromMessage;
--  id:2.1/7ff1207ba0c2c38a/AddressQuestionFromMessage/1/0/
   procedure Test_AddressQuestionFromMessage (Gnattest_T : in out Test) is
   --  vn-communication-can-logic-message_utils.ads:36:4:AddressQuestionFromMessage
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (true,
         "Test not implemented.");

--  begin read only
   end Test_AddressQuestionFromMessage;
--  end read only


--  begin read only
   procedure Test_AddressAnswerToMessage (Gnattest_T : in out Test);
   procedure Test_AddressAnswerToMessage_3e9999 (Gnattest_T : in out Test) renames Test_AddressAnswerToMessage;
--  id:2.1/3e99996ce9f84285/AddressAnswerToMessage/1/0/
   procedure Test_AddressAnswerToMessage (Gnattest_T : in out Test) is
   --  vn-communication-can-logic-message_utils.ads:38:4:AddressAnswerToMessage
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (true,
         "Test not implemented.");

--  begin read only
   end Test_AddressAnswerToMessage;
--  end read only


--  begin read only
   procedure Test_AddressAnswerFromMessage (Gnattest_T : in out Test);
   procedure Test_AddressAnswerFromMessage_8ef58e (Gnattest_T : in out Test) renames Test_AddressAnswerFromMessage;
--  id:2.1/8ef58eb09ccb77c9/AddressAnswerFromMessage/1/0/
   procedure Test_AddressAnswerFromMessage (Gnattest_T : in out Test) is
   --  vn-communication-can-logic-message_utils.ads:41:4:AddressAnswerFromMessage
--  end read only

      pragma Unreferenced (Gnattest_T);

      msg : VN.Communication.CAN.CAN_Message_Logical;
      logicalAddr, logicalAddr2 : VN.VN_Logical_Address;


      CANaddr, CANaddr2 : VN.Communication.CAN.CAN_Address_Sender;

      sender : VN.Communication.CAN.CAN_Address_Sender := 65;
      receiver : VN.Communication.CAN.CAN_Address_Receiver := 26;
   begin

      CANaddr := 36;
      logicalAddr := 972;

      AddressAnswerToMessage(msg, receiver, sender, CANaddr, logicalAddr, 0);

      AUnit.Assertions.Assert
        (msg.msgType = VN.Communication.CAN.Logic.ADDRESS_ANSWER and msg.isNormal and msg.Receiver = receiver and msg.Sender=sender and msg.Length=5,
         "AddressAnswerToMessage failed. 1");

      AddressAnswerFromMessage(msg, CANaddr2, logicalAddr2);

      AUnit.Assertions.Assert
        (CANaddr2 = CANaddr and logicalAddr = logicalAddr2,
         "AddressAnswerToMessage failed. 2");


--  begin read only
   end Test_AddressAnswerFromMessage;
--  end read only


--  begin read only
   procedure Test_AssignLogicalAddressToMessage (Gnattest_T : in out Test);
   procedure Test_AssignLogicalAddressToMessage_5a4d87 (Gnattest_T : in out Test) renames Test_AssignLogicalAddressToMessage;
--  id:2.1/5a4d879c5a0d73a0/AssignLogicalAddressToMessage/1/0/
   procedure Test_AssignLogicalAddressToMessage (Gnattest_T : in out Test) is
   --  vn-communication-can-logic-message_utils.ads:44:4:AssignLogicalAddressToMessage
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (true,
         "Test not implemented.");

--  begin read only
   end Test_AssignLogicalAddressToMessage;
--  end read only


--  begin read only
   procedure Test_AssignLogicalAddressFromMessage (Gnattest_T : in out Test);
   procedure Test_AssignLogicalAddressFromMessage_2674a2 (Gnattest_T : in out Test) renames Test_AssignLogicalAddressFromMessage;
--  id:2.1/2674a2ed2fe86e88/AssignLogicalAddressFromMessage/1/0/
   procedure Test_AssignLogicalAddressFromMessage (Gnattest_T : in out Test) is
   --  vn-communication-can-logic-message_utils.ads:47:4:AssignLogicalAddressFromMessage
--  end read only

      pragma Unreferenced (Gnattest_T);

      msg : VN.Communication.CAN.CAN_Message_Logical;
      addr, addr2 : VN.VN_Logical_Address;

      sender : VN.Communication.CAN.CAN_Address_Sender := 65;
      receiver : VN.Communication.CAN.CAN_Address_Receiver := 26;
   begin

      addr := 365;

      AssignLogicalAddressToMessage(msg, receiver, sender, 0, addr);

      AUnit.Assertions.Assert
        (msg.msgType = VN.Communication.CAN.Logic.ASSIGN_LOGICAL_ADDR and msg.isNormal and msg.Receiver = receiver and msg.Sender=sender and msg.Length=4,
         "AssignLogicalAddress failed. 1");

      AssignLogicalAddressFromMessage(msg, addr2);

      AUnit.Assertions.Assert
        (addr2 = addr,
         "AssignLogicalAddress failed. 1");

--  begin read only
   end Test_AssignLogicalAddressFromMessage;
--  end read only


--  begin read only
   procedure Test_ComponentTypeToMessage (Gnattest_T : in out Test);
   procedure Test_ComponentTypeToMessage_d30185 (Gnattest_T : in out Test) renames Test_ComponentTypeToMessage;
--  id:2.1/d30185d730062f1a/ComponentTypeToMessage/1/0/
   procedure Test_ComponentTypeToMessage (Gnattest_T : in out Test) is
   --  vn-communication-can-logic-message_utils.ads:49:4:ComponentTypeToMessage
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (true,
         "Test not implemented.");

--  begin read only
   end Test_ComponentTypeToMessage;
--  end read only


--  begin read only
   procedure Test_ComponentTypeFromMessage (Gnattest_T : in out Test);
   procedure Test_ComponentTypeFromMessage_c3da10 (Gnattest_T : in out Test) renames Test_ComponentTypeFromMessage;
--  id:2.1/c3da104b6c9460df/ComponentTypeFromMessage/1/0/
   procedure Test_ComponentTypeFromMessage (Gnattest_T : in out Test) is
   --  vn-communication-can-logic-message_utils.ads:51:4:ComponentTypeFromMessage
--  end read only

      pragma Unreferenced (Gnattest_T);

      msg : VN.Communication.CAN.CAN_Message_Logical;
      isSM_CAN : boolean :=false;
      isSM_CAN2 : boolean :=false;

      sender : VN.Communication.CAN.CAN_Address_Sender := 65;
   begin

      ComponentTypeToMessage(msg, sender, 0, isSM_CAN);
      AUnit.Assertions.Assert
        (msg.isNormal and msg.Receiver = VN.Communication.CAN.CAN_Address_Receiver(254) and msg.Sender=sender and msg.Length=1 and
           ((isSM_CAN and msg.Data(msg.Data'First) = 3) or (not isSM_CAN and msg.Data(msg.Data'First) = 5)),
         "ComponentTypeFromMessage failed. 1");

      ComponentTypeFromMessage(msg, isSM_CAN2);

      AUnit.Assertions.Assert
        (isSM_CAN2 = isSM_CAN,
         "ComponentTypeFromMessage failed. 2");

--  begin read only
   end Test_ComponentTypeFromMessage;
--  end read only


--  begin read only
   procedure Test_RequestCUUIDToMessage (Gnattest_T : in out Test);
   procedure Test_RequestCUUIDToMessage_71c1f4 (Gnattest_T : in out Test) renames Test_RequestCUUIDToMessage;
--  id:2.1/71c1f4902d857978/RequestCUUIDToMessage/1/0/
   procedure Test_RequestCUUIDToMessage (Gnattest_T : in out Test) is
   --  vn-communication-can-logic-message_utils.ads:53:4:RequestCUUIDToMessage
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "RequestCUUIDToMessage, Test not implemented.");

--  begin read only
   end Test_RequestCUUIDToMessage;
--  end read only


--  begin read only
   procedure Test_CUUIDHalfToMessage (Gnattest_T : in out Test);
   procedure Test_CUUIDHalfToMessage_9a153f (Gnattest_T : in out Test) renames Test_CUUIDHalfToMessage;
--  id:2.1/9a153f86ea5a2583/CUUIDHalfToMessage/1/0/
   procedure Test_CUUIDHalfToMessage (Gnattest_T : in out Test) is
   --  vn-communication-can-logic-message_utils.ads:55:4:CUUIDHalfToMessage
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "CUUIDHalfToMessage, Test not implemented.");

--  begin read only
   end Test_CUUIDHalfToMessage;
--  end read only


--  begin read only
   procedure Test_CUUIDHalfFromMessage (Gnattest_T : in out Test);
   procedure Test_CUUIDHalfFromMessage_55905e (Gnattest_T : in out Test) renames Test_CUUIDHalfFromMessage;
--  id:2.1/55905e3666794617/CUUIDHalfFromMessage/1/0/
   procedure Test_CUUIDHalfFromMessage (Gnattest_T : in out Test) is
   --  vn-communication-can-logic-message_utils.ads:58:4:CUUIDHalfFromMessage
--  end read only

      pragma Unreferenced (Gnattest_T);


      msg : VN.Communication.CAN.CAN_Message_Logical;
      sender : VN.Communication.CAN.CAN_Address_Sender := 1;
      theCUUID : VN.VN_CUUID;

      theCUUID2 : VN.VN_CUUID;
   begin

      CUUIDHalfToMessage(msg, sender, theCUUID, true);

      AUnit.Assertions.Assert
        (msg.isNormal and msg.Sender = sender and msg.msgType = VN.Communication.CAN.Logic.FIRST_CUUID_HALF,
         "CUUIDHalfFromMessage failed. 1");

      CUUIDHalfFromMessage(msg, theCUUID2, true);

      AUnit.Assertions.Assert
        (theCUUID2(1) = theCUUID(1) and theCUUID2(2) = theCUUID(2) and theCUUID2(3) = theCUUID(3) and
           theCUUID2(4) = theCUUID(4) and theCUUID2(5) = theCUUID(5) and theCUUID2(6) = theCUUID(6) and
           theCUUID2(7) = theCUUID(7) and theCUUID2(8) = theCUUID(8),
         "CUUIDHalfFromMessage failed. 2");


      CUUIDHalfToMessage(msg, sender, theCUUID, false);

      AUnit.Assertions.Assert
        (msg.Sender = sender and msg.msgType = VN.Communication.CAN.Logic.SECOND_CUUID_HALF,
         "CUUIDHalfFromMessage failed. 3");

      CUUIDHalfFromMessage(msg, theCUUID2, false);

      AUnit.Assertions.Assert
        (theCUUID2(9) = theCUUID(9) and theCUUID2(10) = theCUUID(10) and theCUUID2(11) = theCUUID(11) and
           theCUUID2(12) = theCUUID(12) and theCUUID2(13) = theCUUID(13) and theCUUID2(14) = theCUUID(14) and
           theCUUID2(15) = theCUUID(15) and theCUUID2(16) = theCUUID(16),
         "CUUIDHalfFromMessage failed. 4");

--  begin read only
   end Test_CUUIDHalfFromMessage;
--  end read only


--  begin read only
   procedure Test_TransmissionToMessage (Gnattest_T : in out Test);
   procedure Test_TransmissionToMessage_bed12f (Gnattest_T : in out Test) renames Test_TransmissionToMessage;
--  id:2.1/bed12f8a81882333/TransmissionToMessage/1/0/
   procedure Test_TransmissionToMessage (Gnattest_T : in out Test) is
   --  vn-communication-can-logic-message_utils.ads:60:4:TransmissionToMessage
--  end read only

      pragma Unreferenced (Gnattest_T);
   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "TransmissionToMessage, Test not implemented.");

--  begin read only
   end Test_TransmissionToMessage;
--  end read only


--  begin read only
   procedure Test_FlowControlToMessage (Gnattest_T : in out Test);
   procedure Test_FlowControlToMessage_f17442 (Gnattest_T : in out Test) renames Test_FlowControlToMessage;
--  id:2.1/f17442ac61cf509a/FlowControlToMessage/1/0/
   procedure Test_FlowControlToMessage (Gnattest_T : in out Test) is
   --  vn-communication-can-logic-message_utils.ads:66:4:FlowControlToMessage
--  end read only

      pragma Unreferenced (Gnattest_T);

      msg : VN.Communication.CAN.CAN_Message_Logical;
      receiver : VN.Communication.CAN.CAN_Address_Receiver;
      sender : VN.Communication.CAN.CAN_Address_Sender;
      useFlowControl : boolean;
      blockSize : Interfaces.Unsigned_16;

      receiver2 : VN.Communication.CAN.CAN_Address_Receiver;
      sender2 : VN.Communication.CAN.CAN_Address_Sender;
      useFlowControl2 : boolean;
      blockSize2 : Interfaces.Unsigned_16;

   begin

      receiver := 1;
      sender := 2;
      useFlowControl := true;
      blockSize := 3;

      FlowControlToMessage(msg, receiver, sender, useFlowControl, blockSize);

      AUnit.Assertions.Assert
        (msg.msgType = VN.Communication.CAN.Logic.FLOW_CONTROL and msg.isNormal and msg.Sender = sender and msg.Receiver = receiver,
         "FlowControlToMessage failed 1");

      FlowControlFromMessage(msg, receiver2, sender2, useFlowControl2, blockSize2);

      AUnit.Assertions.Assert
        (receiver = receiver2 and sender = sender2 and useFlowControl = useFlowControl2 and blockSize = blockSize2,
         "FlowControlToMessage failed 2");


--  begin read only
   end Test_FlowControlToMessage;
--  end read only


--  begin read only
   procedure Test_FlowControlFromMessage (Gnattest_T : in out Test);
   procedure Test_FlowControlFromMessage_472233 (Gnattest_T : in out Test) renames Test_FlowControlFromMessage;
--  id:2.1/472233c07d347fd1/FlowControlFromMessage/1/0/
   procedure Test_FlowControlFromMessage (Gnattest_T : in out Test) is
   --  vn-communication-can-logic-message_utils.ads:70:4:FlowControlFromMessage
--  end read only

      pragma Unreferenced (Gnattest_T);
      msg : VN.Communication.CAN.CAN_Message_Logical;
      receiver : VN.Communication.CAN.CAN_Address_Receiver;
      sender : VN.Communication.CAN.CAN_Address_Sender;
      useFlowControl : boolean;
      blockSize : Interfaces.Unsigned_16;

      receiver2 : VN.Communication.CAN.CAN_Address_Receiver;
      sender2 : VN.Communication.CAN.CAN_Address_Sender;
      useFlowControl2 : boolean;
      blockSize2 : Interfaces.Unsigned_16;

   begin

      receiver := 103;
      sender := 20;
      useFlowControl := false;
      blockSize := 3;

      FlowControlToMessage(msg, receiver, sender, useFlowControl, blockSize);

      AUnit.Assertions.Assert
        (msg.msgType = VN.Communication.CAN.Logic.FLOW_CONTROL and msg.isNormal and msg.Sender = sender and msg.Receiver = receiver,
         "FlowControlToMessage failed 1");

      FlowControlFromMessage(msg, receiver2, sender2, useFlowControl2, blockSize2);

      AUnit.Assertions.Assert
        (receiver = receiver2 and sender = sender2 and useFlowControl = useFlowControl2,
         "FlowControlToMessage failed 2");

--  begin read only
   end Test_FlowControlFromMessage;
--  end read only


--  begin read only
   procedure Test_StartTransmissionToMessage (Gnattest_T : in out Test);
   procedure Test_StartTransmissionToMessage_6ea075 (Gnattest_T : in out Test) renames Test_StartTransmissionToMessage;
--  id:2.1/6ea07543e25c0e48/StartTransmissionToMessage/1/0/
   procedure Test_StartTransmissionToMessage (Gnattest_T : in out Test) is
   --  vn-communication-can-logic-message_utils.ads:74:4:StartTransmissionToMessage
--  end read only

      pragma Unreferenced (Gnattest_T);

      msg : VN.Communication.CAN.CAN_Message_Logical;

      numMessages : Interfaces.Unsigned_16;
      receiver : VN.Communication.CAN.CAN_Address_Receiver;
      sender : VN.Communication.CAN.CAN_Address_Sender;

      receiver2 : VN.Communication.CAN.CAN_Address_Receiver;
      sender2 : VN.Communication.CAN.CAN_Address_Sender;
      numMessages2 : Interfaces.Unsigned_16;
   begin

      receiver := 3;
      sender := 2;
      numMessages := 3;

      StartTransmissionToMessage(msg, receiver, sender, numMessages);
      StartTransmissionFromMessage(msg, receiver2, sender2, numMessages2);

      AUnit.Assertions.Assert
        (msg.msgType = VN.Communication.CAN.Logic.START_TRANSMISSION and msg.isNormal and msg.Sender = sender and msg.Receiver = receiver
           and receiver = receiver2 and sender = sender2 and numMessages = numMessages2,
         "StartTransmission failed");

--  begin read only
   end Test_StartTransmissionToMessage;
--  end read only


--  begin read only
   procedure Test_StartTransmissionFromMessage (Gnattest_T : in out Test);
   procedure Test_StartTransmissionFromMessage_2dc8f5 (Gnattest_T : in out Test) renames Test_StartTransmissionFromMessage;
--  id:2.1/2dc8f595694b604d/StartTransmissionFromMessage/1/0/
   procedure Test_StartTransmissionFromMessage (Gnattest_T : in out Test) is
   --  vn-communication-can-logic-message_utils.ads:77:4:StartTransmissionFromMessage
--  end read only

      pragma Unreferenced (Gnattest_T);

      msg : VN.Communication.CAN.CAN_Message_Logical;
      numMessages : Interfaces.Unsigned_16;
      receiver : VN.Communication.CAN.CAN_Address_Receiver;
      sender : VN.Communication.CAN.CAN_Address_Sender;

      receiver2 : VN.Communication.CAN.CAN_Address_Receiver;
      sender2 : VN.Communication.CAN.CAN_Address_Sender;
      numMessages2 : Interfaces.Unsigned_16;

   begin

      receiver := 9;
      sender := 20;
      numMessages := 6;

      StartTransmissionToMessage(msg, receiver, sender, numMessages);
      StartTransmissionFromMessage(msg, receiver2, sender2, numMessages2);

      AUnit.Assertions.Assert
        (msg.msgType = VN.Communication.CAN.Logic.START_TRANSMISSION and msg.isNormal and msg.Sender = sender and msg.Receiver = receiver
         and receiver = receiver2 and sender = sender2 and numMessages = numMessages2,
         "StartTransmission failed");


--  begin read only
   end Test_StartTransmissionFromMessage;
--  end read only


--  begin read only
   procedure Test_AssignCANAddressToMessage (Gnattest_T : in out Test);
   procedure Test_AssignCANAddressToMessage_924e82 (Gnattest_T : in out Test) renames Test_AssignCANAddressToMessage;
--  id:2.1/924e82fe823fe2be/AssignCANAddressToMessage/1/0/
   procedure Test_AssignCANAddressToMessage (Gnattest_T : in out Test) is
   --  vn-communication-can-logic-message_utils.ads:80:4:AssignCANAddressToMessage
--  end read only

      pragma Unreferenced (Gnattest_T);

      msg : VN.Communication.CAN.CAN_Message_Logical;

      theUCID : VN.Communication.CAN.UCID;
      theCANAddr : VN.Communication.CAN.CAN_Address_Sender;

      theUCID2 : VN.Communication.CAN.UCID;
      theCANAddr2 : VN.Communication.CAN.CAN_Address_Sender;

   begin

      theUCID := 2000;
      theCANAddr := 36;

      AssignCANAddressToMessage(msg, theUCID, theCANAddr);

      AssignCANAddressFromMessage(msg, theUCID2, theCANAddr2);

      AUnit.Assertions.Assert
        (msg.msgType = VN.Communication.CAN.Logic.ASSIGN_CAN_ADDRESS and msg.isNormal and
           msg.Sender = VN.Communication.CAN.CAN_Address_Sender(0) and msg.Receiver = VN.Communication.CAN.CAN_Address_Receiver(255) and
           theUCID2 = theUCID and theUCID2 = theUCID and theCANAddr2 = theCANAddr,
         "AssignCANAddress failed");

--  begin read only
   end Test_AssignCANAddressToMessage;
--  end read only


--  begin read only
   procedure Test_AssignCANAddressFromMessage (Gnattest_T : in out Test);
   procedure Test_AssignCANAddressFromMessage_bf50fb (Gnattest_T : in out Test) renames Test_AssignCANAddressFromMessage;
--  id:2.1/bf50fb4b94b3d415/AssignCANAddressFromMessage/1/0/
   procedure Test_AssignCANAddressFromMessage (Gnattest_T : in out Test) is
   --  vn-communication-can-logic-message_utils.ads:83:4:AssignCANAddressFromMessage
--  end read only

      pragma Unreferenced (Gnattest_T);

      msg : VN.Communication.CAN.CAN_Message_Logical;

      theUCID : VN.Communication.CAN.UCID;
      theCANAddr : VN.Communication.CAN.CAN_Address_Sender;

      theUCID2 : VN.Communication.CAN.UCID;
      theCANAddr2 : VN.Communication.CAN.CAN_Address_Sender;

   begin

      theUCID := 2100;
      theCANAddr := 3;

      AssignCANAddressToMessage(msg, theUCID, theCANAddr);

      AssignCANAddressFromMessage(msg, theUCID2, theCANAddr2);

      AUnit.Assertions.Assert
        (msg.isNormal and msg.Sender = VN.Communication.CAN.CAN_Address_Sender(0) and msg.Receiver = VN.Communication.CAN.CAN_Address_Receiver(255) and
           theUCID2 = theUCID and theUCID2 = theUCID and theCANAddr2 = theCANAddr,
         "AssignCANAddress failed");

--  begin read only
   end Test_AssignCANAddressFromMessage;
--  end read only


--  begin read only
   procedure Test_RequestCANAddressToMessage (Gnattest_T : in out Test);
   procedure Test_RequestCANAddressToMessage_0c44d1 (Gnattest_T : in out Test) renames Test_RequestCANAddressToMessage;
--  id:2.1/0c44d1dba15cd2cf/RequestCANAddressToMessage/1/0/
   procedure Test_RequestCANAddressToMessage (Gnattest_T : in out Test) is
   --  vn-communication-can-logic-message_utils.ads:86:4:RequestCANAddressToMessage
--  end read only

      pragma Unreferenced (Gnattest_T);
      msg : VN.Communication.CAN.CAN_Message_Logical;
      theUCID : VN.Communication.CAN.UCID;
      bIs_SM_CAN : boolean;

      bIs_SM_CAN2 : boolean;
      theUCID2 : VN.Communication.CAN.UCID;

   begin

      theUCID := 2100;
      bIs_SM_CAN := true;

      RequestCANAddressToMessage(msg, theUCID, bIs_SM_CAN);

      RequestCANAddressFromMessage(msg, theUCID2, bIs_SM_CAN2);

      AUnit.Assertions.Assert
        (msg.isNormal = false and msg.SenderUCID = theUCID and theUCID2 = theUCID and theUCID2 = theUCID and bIs_SM_CAN = bIs_SM_CAN2,
        "RequestCANAddress failed");

--  begin read only
   end Test_RequestCANAddressToMessage;
--  end read only


--  begin read only
   procedure Test_RequestCANAddressFromMessage (Gnattest_T : in out Test);
   procedure Test_RequestCANAddressFromMessage_4529ba (Gnattest_T : in out Test) renames Test_RequestCANAddressFromMessage;
--  id:2.1/4529bae474beffb3/RequestCANAddressFromMessage/1/0/
   procedure Test_RequestCANAddressFromMessage (Gnattest_T : in out Test) is
   --  vn-communication-can-logic-message_utils.ads:89:4:RequestCANAddressFromMessage
--  end read only

      pragma Unreferenced (Gnattest_T);

      msg : VN.Communication.CAN.CAN_Message_Logical;
      theUCID : VN.Communication.CAN.UCID;
      bIs_SM_CAN : boolean;

      bIs_SM_CAN2 : boolean;
      theUCID2 : VN.Communication.CAN.UCID;

   begin

      theUCID := 210;
      bIs_SM_CAN := false;

      RequestCANAddressToMessage(msg, theUCID, bIs_SM_CAN);

      RequestCANAddressFromMessage(msg, theUCID2, bIs_SM_CAN2);

      AUnit.Assertions.Assert
        (msg.isNormal = false and msg.SenderUCID = theUCID and theUCID2 = theUCID and bIs_SM_CAN = bIs_SM_CAN2,
        "RequestCANAddress failed");

--  begin read only
   end Test_RequestCANAddressFromMessage;
--  end read only


--  begin read only
   procedure Test_CANMasterAssignedToMessage (Gnattest_T : in out Test);
   procedure Test_CANMasterAssignedToMessage_d7a91b (Gnattest_T : in out Test) renames Test_CANMasterAssignedToMessage;
--  id:2.1/d7a91b4dfdbc5b53/CANMasterAssignedToMessage/1/0/
   procedure Test_CANMasterAssignedToMessage (Gnattest_T : in out Test) is
   --  vn-communication-can-logic-message_utils.ads:92:4:CANMasterAssignedToMessage
--  end read only

      pragma Unreferenced (Gnattest_T);

      msg : VN.Communication.CAN.CAN_Message_Logical;

   begin

      CANMasterAssignedToMessage(msg, 0);

      AUnit.Assertions.Assert
        (msg.isNormal and msg.msgType = VN.Communication.CAN.Logic.CAN_MASTER_ASSIGNED and
           msg.Sender = VN.Communication.CAN.CAN_Address_Sender(0) and msg.Receiver = VN.Communication.CAN.CAN_Address_Receiver(255),
         "CANMasterAssigned failed");

--  begin read only
   end Test_CANMasterAssignedToMessage;
--  end read only


--  begin read only
   procedure Test_GetMessageID (Gnattest_T : in out Test);
   procedure Test_GetMessageID_2917d6 (Gnattest_T : in out Test) renames Test_GetMessageID;
--  id:2.1/2917d6d24aa3a8a6/GetMessageID/1/0/
   procedure Test_GetMessageID (Gnattest_T : in out Test) is
   --  vn-communication-can-logic-message_utils.ads:94:4:GetMessageID
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "GetMessageID, Test not implemented.");

--  begin read only
   end Test_GetMessageID;
--  end read only


--  begin read only
   procedure Test_Fragment (Gnattest_T : in out Test);
   procedure Test_Fragment_2d8518 (Gnattest_T : in out Test) renames Test_Fragment;
--  id:2.1/2d851815cb7a2e92/Fragment/1/0/
   procedure Test_Fragment (Gnattest_T : in out Test) is
   --  vn-communication-can-logic-message_utils.ads:98:4:Fragment
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (true,
         "Test not implemented.");

--  begin read only
   end Test_Fragment;
--  end read only


--  begin read only
   procedure Test_DeFragment (Gnattest_T : in out Test);
   procedure Test_DeFragment_744d32 (Gnattest_T : in out Test) renames Test_DeFragment;
--  id:2.1/744d32bc894e4700/DeFragment/1/0/
   procedure Test_DeFragment (Gnattest_T : in out Test) is
   --  vn-communication-can-logic-message_utils.ads:104:4:DeFragment
--  end read only

      pragma Unreferenced (Gnattest_T);

      msgBasic1 : VN.Message.VN_Message_Basic := VN.Message.Factory.Create(VN.Message.Type_Local_Hello);
      msgBasic2 : VN.Message.VN_Message_Basic;
      msgLocalHello1, msgLocalHello2 : VN.Message.Local_Hello.VN_Message_Local_Hello;
      msgArray1, msgArray2 : VN.Message.VN_Message_Byte_Array;

      msgCAN : VN.Communication.CAN.CAN_Message_Logical;
      seqNum : Interfaces.Unsigned_16 := 0;
      currentLength : Interfaces.Unsigned_16 := 0;

      numLoops, numBytes : Integer;
      isLastMsg : boolean := false;
   begin

      VN.Message.Local_Hello.To_Local_Hello(msgBasic1, msgLocalHello1);

      msgLocalHello1.Header.Version := 1;
      msgLocalHello1.Header.Priority := 2;
      msgLocalHello1.Header.Destination := 4;
      msgLocalHello1.Header.Source := 5;
      msgLocalHello1.Header.Flags := 6;
      msgLocalHello1.Header.Ext_Header := 8;
      msgLocalHello1.Checksum := 257;

      msgLocalHello1.CUUID := (others => 6);
      msgLocalHello1.Component_Type := VN.Message.SM_x;

      VN.Message.Local_Hello.To_Basic(msgLocalHello1, msgBasic1);

      numBytes := Integer(msgLocalHello1.Header.Payload_Length) +
        VN.Message.CHECKSUM_SIZE + VN.Message.HEADER_SIZE;

      VN.Message.Serialize(msgBasic1, msgArray1);

      if numBytes mod 8 = 0 then
         numLoops := numBytes / 8;
      else
         numLoops := numBytes / 8 + 1;
      end if;

      for i in 1..numLoops loop
         Fragment(msgArray1, seqNum, Interfaces.Unsigned_16(numBytes), msgCAN, isLastMsg);

         DeFragment(seqNum, Interfaces.Unsigned_16(numBytes), msgCAN, msgArray2, currentLength);
      end loop;

      AUnit.Assertions.Assert
        (isLastMsg,
         "isLastMsg incorrect");

      VN.Message.Deserialize(msgBasic2, msgArray2);

      VN.Message.Local_Hello.To_Local_Hello(msgBasic2, msgLocalHello2);

      AUnit.Assertions.Assert
        (msgLocalHello1.Header.Destination = msgLocalHello2.Header.Destination,
         "msgLocalHello2.Header.Destination incorrect");

      AUnit.Assertions.Assert
        (msgLocalHello1.Header.Priority = msgLocalHello2.Header.Priority,
         "msgLocalHello2.Header.Priority incorrect");


      AUnit.Assertions.Assert
        (msgLocalHello1.Header.Source = msgLocalHello2.Header.Source,
         "msgLocalHello2.Header.Source incorrect");


      AUnit.Assertions.Assert
        (msgLocalHello1.Header.Flags = msgLocalHello2.Header.Flags,
         "msgLocalHello2.Header.Flags incorrect");


      AUnit.Assertions.Assert
        (msgLocalHello1.Header.Ext_Header = msgLocalHello2.Header.Ext_Header,
         "msgLocalHello2.Header.Ext_Header incorrect");


      AUnit.Assertions.Assert
        (msgLocalHello1.Checksum = msgLocalHello2.Checksum,
         "msgLocalHello2.Checksum incorrect");


      AUnit.Assertions.Assert
        (msgLocalHello1.Header.Destination = msgLocalHello2.Header.Destination,
         "msgLocalHello2.Header.Destination incorrect");


      AUnit.Assertions.Assert
        (msgLocalHello1.CUUID = msgLocalHello2.CUUID,
         "msgLocalHello2.CUUID incorrect");


      AUnit.Assertions.Assert
        (msgLocalHello1.Component_Type = msgLocalHello2.Component_Type,
         "msgLocalHello2.Component_Type incorrect");

      --  begin read only
   end Test_DeFragment;
--  end read only


--  begin read only
   procedure Test_DataToU16 (Gnattest_T : in out Test);
   procedure Test_DataToU16_97e63f (Gnattest_T : in out Test) renames Test_DataToU16;
--  id:2.1/97e63fcee33bd4e6/DataToU16/1/0/
   procedure Test_DataToU16 (Gnattest_T : in out Test) is
   --  vn-communication-can-logic-message_utils.ads:119:4:DataToU16
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : VN.Communication.CAN.Byte8;
      u16 : Interfaces.Unsigned_16;
      u16expected : Interfaces.Unsigned_16;

      begin

      u16expected := 601;
      U16ToData(u16expected, Data);
      DataToU16(Data, u16);

      AUnit.Assertions.Assert
        (u16expected = u16,
         "DataToU16 failed");

--  begin read only
   end Test_DataToU16;
--  end read only


--  begin read only
   procedure Test_U16ToData (Gnattest_T : in out Test);
   procedure Test_U16ToData_ff1217 (Gnattest_T : in out Test) renames Test_U16ToData;
--  id:2.1/ff12177b6b62133c/U16ToData/1/0/
   procedure Test_U16ToData (Gnattest_T : in out Test) is
   --  vn-communication-can-logic-message_utils.ads:121:4:U16ToData
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : VN.Communication.CAN.Byte8;
      u16 : Interfaces.Unsigned_16;
      u16expected : Interfaces.Unsigned_16;

   begin

      u16expected := 302;
      U16ToData(u16expected, Data);

      DataToU16(Data, u16);

      AUnit.Assertions.Assert
        (u16expected = u16,
         "U16ToData failed");

--  begin read only
   end Test_U16ToData;
--  end read only

end VN.Communication.CAN.Logic.Message_Utils.Test_Data.Tests;
