--  This package has been generated automatically by GNATtest.
--  Do not edit any part of it, see GNATtest documentation for more details.

--  begin read only
with AUnit.Test_Caller;
with Gnattest_Generated;

package body Physical_Logical.Test_Data.Tests.Suite is

   package Runner_1 is new AUnit.Test_Caller
     (GNATtest_Generated.GNATtest_Standard.Physical_Logical.Test_Data.Tests.Test);

   Result : aliased AUnit.Test_Suites.Test_Suite;

   Case_1_1_Test_PhysicalToLogical_9c67bd : aliased Runner_1.Test_Case;
   Case_2_1_Test_LogicalToPhysical_f36476 : aliased Runner_1.Test_Case;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin

      Runner_1.Create
        (Case_1_1_Test_PhysicalToLogical_9c67bd,
         "physical_logical.ads:26:4:",
         Test_PhysicalToLogical_9c67bd'Access);
      Runner_1.Create
        (Case_2_1_Test_LogicalToPhysical_f36476,
         "physical_logical.ads:28:4:",
         Test_LogicalToPhysical_f36476'Access);

      Result.Add_Test (Case_1_1_Test_PhysicalToLogical_9c67bd'Access);
      Result.Add_Test (Case_2_1_Test_LogicalToPhysical_f36476'Access);

      return Result'Access;

   end Suite;

end Physical_Logical.Test_Data.Tests.Suite;
--  end read only
