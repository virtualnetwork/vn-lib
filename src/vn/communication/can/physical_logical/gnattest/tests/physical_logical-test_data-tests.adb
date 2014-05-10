--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Physical_Logical.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Physical_Logical.Test_Data.Tests is


--  begin read only
   procedure Test_PhysicalToLogical (Gnattest_T : in out Test);
   procedure Test_PhysicalToLogical_9c67bd (Gnattest_T : in out Test) renames Test_PhysicalToLogical;
--  id:2.1/9c67bdf5c552128e/PhysicalToLogical/1/0/
   procedure Test_PhysicalToLogical (Gnattest_T : in out Test) is
   --  physical_logical.ads:26:4:PhysicalToLogical
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_PhysicalToLogical;
--  end read only


--  begin read only
   procedure Test_LogicalToPhysical (Gnattest_T : in out Test);
   procedure Test_LogicalToPhysical_f36476 (Gnattest_T : in out Test) renames Test_LogicalToPhysical;
--  id:2.1/f364768a1b746936/LogicalToPhysical/1/0/
   procedure Test_LogicalToPhysical (Gnattest_T : in out Test) is
   --  physical_logical.ads:28:4:LogicalToPhysical
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_LogicalToPhysical;
--  end read only

end Physical_Logical.Test_Data.Tests;
