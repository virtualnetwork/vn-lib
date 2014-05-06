with Global_Settings;
with Subnet_Manager_Local;
with Central_Addressing_Service;
with Application;

procedure Main is
begin
   Global_Settings.Com_Application.Init;
   Global_Settings.Com_SM_L.Add_Interface(Global_Settings.PO_Router'Access);
   Global_Settings.PO_Router.Add_Interface(Global_Settings.PO_Wrapper_To_App'Access);
end Main;
