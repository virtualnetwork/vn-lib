--  This package has been generated automatically by GNATtest.
--  Do not edit any part of it, see GNATtest documentation for more details.

--  begin read only
with AUnit.Test_Caller;
with Gnattest_Generated;

package body VN.Communication.CAN.CAN_Filtering.CAN_Filter_Type_Test_Data.CAN_Filter_Type_Tests.Suite is

   package Runner_1 is new AUnit.Test_Caller
     (GNATtest_Generated.GNATtest_Standard.VN.Communication.CAN.CAN_Filtering.CAN_Filter_Type_Test_Data.CAN_Filter_Type_Tests.Test_CAN_Filter_Type);

   Result : aliased AUnit.Test_Suites.Test_Suite;

   Case_1_1_Test_Create_Filter_fa8804 : aliased Runner_1.Test_Case;
   Case_2_1_Test_Change_Filter_af7866 : aliased Runner_1.Test_Case;
   Case_3_1_Test_Remove_Filter_671035 : aliased Runner_1.Test_Case;
   Case_4_1_Test_Get_Filter_d32e8e : aliased Runner_1.Test_Case;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin

      Runner_1.Create
        (Case_1_1_Test_Create_Filter_fa8804,
         "vn-communication-can-can_filtering.ads:21:4:",
         Test_Create_Filter_fa8804'Access);
      Runner_1.Create
        (Case_2_1_Test_Change_Filter_af7866,
         "vn-communication-can-can_filtering.ads:26:4:",
         Test_Change_Filter_af7866'Access);
      Runner_1.Create
        (Case_3_1_Test_Remove_Filter_671035,
         "vn-communication-can-can_filtering.ads:31:4:",
         Test_Remove_Filter_671035'Access);
      Runner_1.Create
        (Case_4_1_Test_Get_Filter_d32e8e,
         "vn-communication-can-can_filtering.ads:34:4:",
         Test_Get_Filter_d32e8e'Access);

      Result.Add_Test (Case_1_1_Test_Create_Filter_fa8804'Access);
      Result.Add_Test (Case_2_1_Test_Change_Filter_af7866'Access);
      Result.Add_Test (Case_3_1_Test_Remove_Filter_671035'Access);
      Result.Add_Test (Case_4_1_Test_Get_Filter_d32e8e'Access);

      return Result'Access;

   end Suite;

end VN.Communication.CAN.CAN_Filtering.CAN_Filter_Type_Test_Data.CAN_Filter_Type_Tests.Suite;
--  end read only
