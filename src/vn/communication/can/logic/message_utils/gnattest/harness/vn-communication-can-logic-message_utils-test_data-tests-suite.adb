--  This package has been generated automatically by GNATtest.
--  Do not edit any part of it, see GNATtest documentation for more details.

--  begin read only
with AUnit.Test_Caller;
with Gnattest_Generated;

package body VN.Communication.CAN.Logic.Message_Utils.Test_Data.Tests.Suite is

   package Runner_1 is new AUnit.Test_Caller
     (GNATtest_Generated.GNATtest_Standard.VN.Communication.CAN.Logic.Message_Utils.Test_Data.Tests.Test);

   Result : aliased AUnit.Test_Suites.Test_Suite;

   Case_1_1_Test_AddressQuestionToMessage_8f619b : aliased Runner_1.Test_Case;
   Case_2_1_Test_AddressQuestionFromMessage_7ff120 : aliased Runner_1.Test_Case;
   Case_3_1_Test_AddressAnswerToMessage_3e9999 : aliased Runner_1.Test_Case;
   Case_4_1_Test_AddressAnswerFromMessage_8ef58e : aliased Runner_1.Test_Case;
   Case_5_1_Test_AssignLogicalAddressToMessage_5a4d87 : aliased Runner_1.Test_Case;
   Case_6_1_Test_AssignLogicalAddressFromMessage_2674a2 : aliased Runner_1.Test_Case;
   Case_7_1_Test_ComponentTypeToMessage_d30185 : aliased Runner_1.Test_Case;
   Case_8_1_Test_ComponentTypeFromMessage_c3da10 : aliased Runner_1.Test_Case;
   Case_9_1_Test_RequestCUUIDToMessage_71c1f4 : aliased Runner_1.Test_Case;
   Case_10_1_Test_CUUIDHalfToMessage_9a153f : aliased Runner_1.Test_Case;
   Case_11_1_Test_CUUIDHalfFromMessage_55905e : aliased Runner_1.Test_Case;
   Case_12_1_Test_TransmissionToMessage_bed12f : aliased Runner_1.Test_Case;
   Case_13_1_Test_FlowControlToMessage_f17442 : aliased Runner_1.Test_Case;
   Case_14_1_Test_FlowControlFromMessage_472233 : aliased Runner_1.Test_Case;
   Case_15_1_Test_StartTransmissionToMessage_6ea075 : aliased Runner_1.Test_Case;
   Case_16_1_Test_StartTransmissionFromMessage_2dc8f5 : aliased Runner_1.Test_Case;
   Case_17_1_Test_AssignCANAddressToMessage_924e82 : aliased Runner_1.Test_Case;
   Case_18_1_Test_AssignCANAddressFromMessage_bf50fb : aliased Runner_1.Test_Case;
   Case_19_1_Test_RequestCANAddressToMessage_0c44d1 : aliased Runner_1.Test_Case;
   Case_20_1_Test_RequestCANAddressFromMessage_4529ba : aliased Runner_1.Test_Case;
   Case_21_1_Test_CANMasterAssignedToMessage_d7a91b : aliased Runner_1.Test_Case;
   Case_22_1_Test_GetMessageID_2917d6 : aliased Runner_1.Test_Case;
   Case_23_1_Test_Fragment_2d8518 : aliased Runner_1.Test_Case;
   Case_24_1_Test_DeFragment_744d32 : aliased Runner_1.Test_Case;
   Case_25_1_Test_DataToU16_97e63f : aliased Runner_1.Test_Case;
   Case_26_1_Test_U16ToData_ff1217 : aliased Runner_1.Test_Case;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin

      Runner_1.Create
        (Case_1_1_Test_AddressQuestionToMessage_8f619b,
         "vn-communication-can-logic-message_utils.ads:33:4:",
         Test_AddressQuestionToMessage_8f619b'Access);
      Runner_1.Create
        (Case_2_1_Test_AddressQuestionFromMessage_7ff120,
         "vn-communication-can-logic-message_utils.ads:36:4:",
         Test_AddressQuestionFromMessage_7ff120'Access);
      Runner_1.Create
        (Case_3_1_Test_AddressAnswerToMessage_3e9999,
         "vn-communication-can-logic-message_utils.ads:38:4:",
         Test_AddressAnswerToMessage_3e9999'Access);
      Runner_1.Create
        (Case_4_1_Test_AddressAnswerFromMessage_8ef58e,
         "vn-communication-can-logic-message_utils.ads:41:4:",
         Test_AddressAnswerFromMessage_8ef58e'Access);
      Runner_1.Create
        (Case_5_1_Test_AssignLogicalAddressToMessage_5a4d87,
         "vn-communication-can-logic-message_utils.ads:44:4:",
         Test_AssignLogicalAddressToMessage_5a4d87'Access);
      Runner_1.Create
        (Case_6_1_Test_AssignLogicalAddressFromMessage_2674a2,
         "vn-communication-can-logic-message_utils.ads:47:4:",
         Test_AssignLogicalAddressFromMessage_2674a2'Access);
      Runner_1.Create
        (Case_7_1_Test_ComponentTypeToMessage_d30185,
         "vn-communication-can-logic-message_utils.ads:49:4:",
         Test_ComponentTypeToMessage_d30185'Access);
      Runner_1.Create
        (Case_8_1_Test_ComponentTypeFromMessage_c3da10,
         "vn-communication-can-logic-message_utils.ads:51:4:",
         Test_ComponentTypeFromMessage_c3da10'Access);
      Runner_1.Create
        (Case_9_1_Test_RequestCUUIDToMessage_71c1f4,
         "vn-communication-can-logic-message_utils.ads:53:4:",
         Test_RequestCUUIDToMessage_71c1f4'Access);
      Runner_1.Create
        (Case_10_1_Test_CUUIDHalfToMessage_9a153f,
         "vn-communication-can-logic-message_utils.ads:55:4:",
         Test_CUUIDHalfToMessage_9a153f'Access);
      Runner_1.Create
        (Case_11_1_Test_CUUIDHalfFromMessage_55905e,
         "vn-communication-can-logic-message_utils.ads:58:4:",
         Test_CUUIDHalfFromMessage_55905e'Access);
      Runner_1.Create
        (Case_12_1_Test_TransmissionToMessage_bed12f,
         "vn-communication-can-logic-message_utils.ads:60:4:",
         Test_TransmissionToMessage_bed12f'Access);
      Runner_1.Create
        (Case_13_1_Test_FlowControlToMessage_f17442,
         "vn-communication-can-logic-message_utils.ads:66:4:",
         Test_FlowControlToMessage_f17442'Access);
      Runner_1.Create
        (Case_14_1_Test_FlowControlFromMessage_472233,
         "vn-communication-can-logic-message_utils.ads:70:4:",
         Test_FlowControlFromMessage_472233'Access);
      Runner_1.Create
        (Case_15_1_Test_StartTransmissionToMessage_6ea075,
         "vn-communication-can-logic-message_utils.ads:74:4:",
         Test_StartTransmissionToMessage_6ea075'Access);
      Runner_1.Create
        (Case_16_1_Test_StartTransmissionFromMessage_2dc8f5,
         "vn-communication-can-logic-message_utils.ads:77:4:",
         Test_StartTransmissionFromMessage_2dc8f5'Access);
      Runner_1.Create
        (Case_17_1_Test_AssignCANAddressToMessage_924e82,
         "vn-communication-can-logic-message_utils.ads:80:4:",
         Test_AssignCANAddressToMessage_924e82'Access);
      Runner_1.Create
        (Case_18_1_Test_AssignCANAddressFromMessage_bf50fb,
         "vn-communication-can-logic-message_utils.ads:83:4:",
         Test_AssignCANAddressFromMessage_bf50fb'Access);
      Runner_1.Create
        (Case_19_1_Test_RequestCANAddressToMessage_0c44d1,
         "vn-communication-can-logic-message_utils.ads:86:4:",
         Test_RequestCANAddressToMessage_0c44d1'Access);
      Runner_1.Create
        (Case_20_1_Test_RequestCANAddressFromMessage_4529ba,
         "vn-communication-can-logic-message_utils.ads:89:4:",
         Test_RequestCANAddressFromMessage_4529ba'Access);
      Runner_1.Create
        (Case_21_1_Test_CANMasterAssignedToMessage_d7a91b,
         "vn-communication-can-logic-message_utils.ads:92:4:",
         Test_CANMasterAssignedToMessage_d7a91b'Access);
      Runner_1.Create
        (Case_22_1_Test_GetMessageID_2917d6,
         "vn-communication-can-logic-message_utils.ads:94:4:",
         Test_GetMessageID_2917d6'Access);
      Runner_1.Create
        (Case_23_1_Test_Fragment_2d8518,
         "vn-communication-can-logic-message_utils.ads:98:4:",
         Test_Fragment_2d8518'Access);
      Runner_1.Create
        (Case_24_1_Test_DeFragment_744d32,
         "vn-communication-can-logic-message_utils.ads:104:4:",
         Test_DeFragment_744d32'Access);
      Runner_1.Create
        (Case_25_1_Test_DataToU16_97e63f,
         "vn-communication-can-logic-message_utils.ads:119:4:",
         Test_DataToU16_97e63f'Access);
      Runner_1.Create
        (Case_26_1_Test_U16ToData_ff1217,
         "vn-communication-can-logic-message_utils.ads:121:4:",
         Test_U16ToData_ff1217'Access);

      Result.Add_Test (Case_1_1_Test_AddressQuestionToMessage_8f619b'Access);
      Result.Add_Test (Case_2_1_Test_AddressQuestionFromMessage_7ff120'Access);
      Result.Add_Test (Case_3_1_Test_AddressAnswerToMessage_3e9999'Access);
      Result.Add_Test (Case_4_1_Test_AddressAnswerFromMessage_8ef58e'Access);
      Result.Add_Test (Case_5_1_Test_AssignLogicalAddressToMessage_5a4d87'Access);
      Result.Add_Test (Case_6_1_Test_AssignLogicalAddressFromMessage_2674a2'Access);
      Result.Add_Test (Case_7_1_Test_ComponentTypeToMessage_d30185'Access);
      Result.Add_Test (Case_8_1_Test_ComponentTypeFromMessage_c3da10'Access);
      Result.Add_Test (Case_9_1_Test_RequestCUUIDToMessage_71c1f4'Access);
      Result.Add_Test (Case_10_1_Test_CUUIDHalfToMessage_9a153f'Access);
      Result.Add_Test (Case_11_1_Test_CUUIDHalfFromMessage_55905e'Access);
      Result.Add_Test (Case_12_1_Test_TransmissionToMessage_bed12f'Access);
      Result.Add_Test (Case_13_1_Test_FlowControlToMessage_f17442'Access);
      Result.Add_Test (Case_14_1_Test_FlowControlFromMessage_472233'Access);
      Result.Add_Test (Case_15_1_Test_StartTransmissionToMessage_6ea075'Access);
      Result.Add_Test (Case_16_1_Test_StartTransmissionFromMessage_2dc8f5'Access);
      Result.Add_Test (Case_17_1_Test_AssignCANAddressToMessage_924e82'Access);
      Result.Add_Test (Case_18_1_Test_AssignCANAddressFromMessage_bf50fb'Access);
      Result.Add_Test (Case_19_1_Test_RequestCANAddressToMessage_0c44d1'Access);
      Result.Add_Test (Case_20_1_Test_RequestCANAddressFromMessage_4529ba'Access);
      Result.Add_Test (Case_21_1_Test_CANMasterAssignedToMessage_d7a91b'Access);
      Result.Add_Test (Case_22_1_Test_GetMessageID_2917d6'Access);
      Result.Add_Test (Case_23_1_Test_Fragment_2d8518'Access);
      Result.Add_Test (Case_24_1_Test_DeFragment_744d32'Access);
      Result.Add_Test (Case_25_1_Test_DataToU16_97e63f'Access);
      Result.Add_Test (Case_26_1_Test_U16ToData_ff1217'Access);

      return Result'Access;

   end Suite;

end VN.Communication.CAN.Logic.Message_Utils.Test_Data.Tests.Suite;
--  end read only
