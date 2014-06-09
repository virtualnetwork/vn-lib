with Global_Settings;
with Subnet_Manager_Local;
with Subnet_Manager_Local_X;
with Subnet_Manager_Local_CAN;
--  with SM_X;

with Central_Addressing_Service;
with Lookup_Service;
with Application;
with AppX;
with VN;

with Ada.Real_Time;
use Ada.Real_Time;


procedure Manager is
   myPeriod : Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds(1000);
   Next_Period : Ada.Real_Time.Time;
begin
   VN.Text_IO.Put_Line("Hello world!!");

   Global_Settings.Com_Application.Init;
   Global_Settings.Com_CAS.Init;
   Global_Settings.Com_LS.Init;
   Global_Settings.Com_App2.Init;
   Global_Settings.PO_Wrapper_To_SM_L.Init;
   Global_Settings.PO_Wrapper_SM_CAN_To_SM_L.Init;

   Global_Settings.Com_SM_L.Add_Interface(Global_Settings.PO_Router'Access);
   Global_Settings.PO_Router.Add_Interface(Global_Settings.PO_Wrapper_To_Application'Access);
   Global_Settings.PO_Router.Add_Interface(Global_Settings.PO_Wrapper_To_CAS'Access);
   Global_Settings.PO_Router.Add_Interface(Global_Settings.PO_Wrapper_To_LS'Access);
   Global_Settings.PO_Router.Add_Interface(Global_Settings.PO_Wrapper_To_SM_x'Access);
   Global_Settings.PO_Router.Add_Interface(Global_Settings.PO_Wrapper_SM_L_To_SM_CAN'Access);

   Global_Settings.Com_SM_x.Add_Interface(Global_Settings.PO_Router_SM_x'Access);
   Global_Settings.PO_Router_SM_x.Add_Interface(Global_Settings.PO_Wrapper_To_App2'Access);
   Global_Settings.PO_Router_SM_x.Add_Interface(Global_Settings.PO_Wrapper_To_SM_L'Access);

   Global_Settings.Com_SM_CAN.Add_Interface(Global_Settings.PO_Router_SM_CAN'Access);
   Global_Settings.PO_Router_SM_CAN.Add_Interface(Global_Settings.PO_Wrapper_SM_CAN_To_SM_L'Access);
--     Global_Settings.PO_Router_SM_CAN.Add_Interface(Global_Settings.CANInterface'Access);

   VN.Text_IO.Put_Line("Main entering infinte loop");
   Next_Period := Ada.Real_Time.Clock;
   loop
      Next_Period := Next_Period + myPeriod;

--        VN.Text_IO.Put_Line("<Manager main function heartbeat>");
      delay until Next_Period;
   end loop;
end Manager;
