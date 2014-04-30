with Ada.Real_Time;
with System;
with VN.Message.Factory;
with VN.Communication.PO;
with VN.Communication.IPC;

package Global_Settings is

   -- Common start time for all applications.
   protected Start_time is
      procedure Get(Time: out Ada.Real_Time.Time);
   private
      pragma Priority(System.Priority'Last);
      Start: Ada.Real_Time.Time;
      First_Time: Boolean := True;
   end Start_Time;

   -- Communication between Application, CAS and SM-L
   PO_To_Application : VN.Communication.PO.VN_PO_Access
                                             := new VN.Communication.PO.VN_PO;
--   PO_To_CAS         : VN.Communication.PO.VN_PO_Access
--                                             := new VN.Communication.PO.VN_PO;

   -- Communication object for Application
   Com_Application   : VN.Communication.IPC.IPC_Wrapper(PO_To_Application,
                                                           False);

--   -- Communication object for Central Addressing Service
--   Com_CAS           : VN.Communication.IPC.IPC_Wrapper(PO_To_Application,
--                                                           False);

   -- Communication object for SM-L
   Com_SM_L          : VN.Communication.IPC.IPC_Wrapper(PO_To_Application,
                                                           True);
   -- Com_SM_L          : VN.Communication.Protocol_Router(???);

end Global_Settings;
