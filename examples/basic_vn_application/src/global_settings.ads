with Ada.Real_Time;
with System;
with Logger.Print_Out;
with VN.Message.Factory;
with VN.Communication.PO;
with VN.Communication.PO_Wrapper;
with VN.Communication.PO_Routing;
with VN.Communication.Routing;

package Global_Settings is

   -- Common start time for all applications.
   protected Start_time is
      procedure Get(Time: out Ada.Real_Time.Time);
   private
      pragma Priority(System.Priority'Last);
      Start: Ada.Real_Time.Time;
      First_Time: Boolean := True;
   end Start_Time;

   Logging: aliased Logger.Print_Out.Print_Out_Logger;

   CUUID_App : aliased VN.VN_CUUID := (others => 10);
   CUUID_SM : aliased VN.VN_CUUID := (others => 20);
   -- Communication between Application, CAS and SM-L
   PO_To_Application : aliased VN.Communication.PO.VN_PO;
--   PO_To_CAS         : VN.Communication.PO.VN_PO_Access
--                                             := new VN.Communication.PO.VN_PO;

   -- Communication object for Application
   Com_Application   : VN.Communication.PO_Wrapper.VN_PO_Wrapper(
                                                            PO_To_Application'Access,
                                                            CUUID_App'Access,
                                                            VN.Message.Other,
                                                            False);

--   -- Communication object for Central Addressing Service
--   Com_CAS           : VN.Communication.IPC.IPC_Wrapper(PO_To_Application,
--                                                           False);

   -- Communication object for SM-L
   -- 1. Create a VN.Communication.Protocol_Routing.Protocol_Routing_Type
   -- 2. Create a VN.Communication.PO_Routing.PO_Router
   -- 3. Add PO_Router to Protocol_Router.
   -- 4. Create all needed PO_Wrappers for the SM-L
   -- 5. Add all PO_Wrappers to the PO_Router.
   -- PO_Router_For_SM_L: VN.Communication.PO_Routing.PO_Router;
   PO_Router: aliased VN.Communication.Routing.Router;
   Com_SM_L : VN.Communication.Routing.Router;

   PO_Wrapper_To_App : aliased VN.Communication.PO_Wrapper.VN_PO_Wrapper(
                                                            PO_To_Application'Access,
                                                            CUUID_SM'Access,
                                                            VN.Message.SM_L,
                                                            True);



--
--      Com_SM_L          : VN.Communication.PO_Wrapper.VN_PO_Wrapper(
--                                                            PO_To_Application,
--                                                            CUUID_SM'Access,
--                                                            VN.Message.SM_L,
--                                                            True);
--
end Global_Settings;
