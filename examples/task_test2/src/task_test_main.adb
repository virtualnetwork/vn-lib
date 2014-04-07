
with VN;
with VN.Communication;
with VN.Communication.CAN;
With Task_Test;

with Malloc;

with GNAT.IO;
--with System.BB.Interrupts;

--with VN.Communication.CAN.Logic.SM;

procedure Task_Test_Main is

   theUCID : aliased VN.Communication.CAN.UCID;
   theCUUID : aliased VN.VN_CUUID;

   --d : VN.Communication.CAN.Logic.SM.SM_Duty(theUCID'Access, theCUUID'Access);

begin

   GNAT.IO.Put_Line("Hello world");
   null;
end Task_Test_Main;
