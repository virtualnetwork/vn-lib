with Global_Settings;
with Subnet_Manager_Local;
with Central_Addressing_Service;
with Application;

procedure Main is
begin
   Global_Settings.Com_Application.Init;
   Global_Settings.Com_SM_L.Add_PO_Wrapper(Global_Settings.PO_Wrapper_To_App'Access);
end Main;
