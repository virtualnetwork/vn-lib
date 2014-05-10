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
      use VN.Communication.CAN;
      use Interfaces;

      logMsg1, logMsg2 : VN.Communication.CAN.CAN_Message_Logical;
      physMsg : Physical_Logical.CAN_Message_Physical;

   begin

--        logMsg1.isNormal := false;
--        logMsg1.Length := 1;
--
--        for n in VN.Communication.CAN.UCID'Range loop
--           logMsg1.SenderUCID := n;
--
--           Physical_Logical.LogicalToPhysical(logMsg1, physMsg);
--           Physical_Logical.PhysicalToLogical(physMsg, logMsg2);
--
--           AUnit.Assertions.Assert
--             (logMsg1.isNormal = logMsg2.isNormal and logMsg1.SenderUCID = logMsg2.SenderUCID
--              and logMsg1.Length = logMsg2.Length and logMsg1.Data(1) = logMsg2.Data(1),
--              "Error logMsg1 /= logMsg2, n= " & n'Img & " logMsg1.SenderUCID= " & logMsg1.SenderUCID'Img &
--                " logMsg2.SenderUCID= " & logMsg2.SenderUCID'Img);
--        end loop;


      logMsg1.isNormal := false;
      logMsg1.Length := 0;

      for i in VN.Communication.CAN.UCID'Range loop
         logMsg1.SenderUCID := i;

         Physical_Logical.LogicalToPhysical(logMsg1, physMsg);
         Physical_Logical.PhysicalToLogical(physMsg, logMsg2);

         AUnit.Assertions.Assert
           (logMsg1.isNormal = logMsg2.isNormal and logMsg1.SenderUCID = logMsg2.SenderUCID
            and logMsg1.Length = logMsg2.Length,
            "Error logMsg1 /= logMsg2, i= " & i'Img & " logMsg1.SenderUCID= " & logMsg1.SenderUCID'Img &
              " logMsg2.SenderUCID= " & logMsg2.SenderUCID'Img);
      end loop;

      logMsg1.isNormal := true;

      for i in VN.Communication.CAN.CAN_Message_Prio'Range loop
         logMsg1.msgPrio := i;

         for j in VN.Communication.CAN.CAN_Address_Receiver'Range loop
            logMsg1.Receiver := j;

            for k in VN.Communication.CAN.CAN_Address_Sender'Range loop
               logMsg1.Sender := k;

               for l in VN.Communication.CAN.DLC_Type'Range loop
                  logMsg1.Length := l;

                  for m in logMsg1.Data'Range loop
                     logMsg1.Data(m) := 3;
                  end loop;

                  Physical_Logical.LogicalToPhysical(logMsg1, physMsg);
                  Physical_Logical.PhysicalToLogical(physMsg, logMsg2);

                  AUnit.Assertions.Assert
                    (logMsg1.isNormal = logMsg2.isNormal and logMsg1.msgPrio = logMsg2.msgPrio and
                       logMsg1.msgType = logMsg2.msgType and logMsg1.Receiver = logMsg2.Receiver and
                         logMsg1.Sender = logMsg2.Sender and logMsg1.Length = logMsg2.Length,
                     "Error logMsg1 /= logMsg2, i= " & i'Img & ", j= " & j'Img & ", k= " & k'Img &
                       ", l= " & l'Img);

                  for m in 1 .. logMsg2.Length loop
                     AUnit.Assertions.Assert
                       (logMsg1.Data(m) = logMsg2.Data(m), "Error logMsg1.Data /= logMsg2.Data");
                  end loop;
               end loop;
            end loop;
         end loop;
      end loop;

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
        (true,
         "Test not implemented.");

--  begin read only
   end Test_LogicalToPhysical;
--  end read only

end Physical_Logical.Test_Data.Tests;
