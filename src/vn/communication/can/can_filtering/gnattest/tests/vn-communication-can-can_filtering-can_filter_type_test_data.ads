--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with AUnit.Test_Fixtures;

with GNATtest_Generated;

package VN.Communication.CAN.CAN_Filtering.CAN_Filter_Type_Test_Data is

   type CAN_Filter_Type_Access is access all GNATtest_Generated.GNATtest_Standard.VN.Communication.CAN.CAN_Filtering.CAN_Filter_Type'Class;

--  begin read only
   type Test_CAN_Filter_Type is new AUnit.Test_Fixtures.Test_Fixture
--  end read only
   with record
      Fixture : CAN_Filter_Type_Access;
   end record;

   procedure Set_Up (Gnattest_T : in out Test_CAN_Filter_Type);
   procedure Tear_Down (Gnattest_T : in out Test_CAN_Filter_Type);

end VN.Communication.CAN.CAN_Filtering.CAN_Filter_Type_Test_Data;
