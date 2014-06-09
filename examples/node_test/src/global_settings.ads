with Ada.Real_Time;
with System;
with Logging.Print_Out;
with VN.Message.Factory;
with VN.Communication.PO;
with VN.Communication.PO_Wrapper;
with VN.Communication.PO_Routing;

with VN.Communication.Protocol_Routing;

-- CAN:
--  with VN.Communication.CAN;
--  with VN.Communication.CAN.CAN_Filtering;
--  with VN.Communication.CAN.CAN_Interface;
--  with VN.Communication.CAN.CAN_Task;

with Ada.Real_Time;


package Global_Settings is

   -- Common start time for all applications.
   protected Start_time is
      procedure Get(Time: out Ada.Real_Time.Time);
   private
      pragma Priority(System.Priority'Last);
      Start: Ada.Real_Time.Time;
      First_Time: Boolean := True;
   end Start_Time;

   Logger: aliased Logging.Print_Out.Print_Out_Logger;

   CUUID_SM    : aliased VN.VN_CUUID := (others => 10);
   CUUID_App2  : aliased VN.VN_CUUID := (others => 20);
   CUUID_SM_CAN  : aliased VN.VN_CUUID := (others => 30);

   Cycle_Time_Applications : constant Positive := 2110000;
   Cycle_Time_SM_L         : constant Positive := 500000;

   -- Communication between Application, CAS, LS and SM-L
   PO_SM_L_To_App2        : aliased VN.Communication.PO.VN_PO;
   PO_SM_L_To_SM_CAN      : aliased VN.Communication.PO.VN_PO;

   -- Communication object for Application
   Com_App2            : VN.Communication.PO_Wrapper.VN_PO_Wrapper(
                                                            PO_SM_L_To_App2'Access,
                                                            CUUID_App2'Access,
                                                            VN.Message.Other,
                                                            False);

   -- Communication object for SM-L
   -- 1. Create a VN.Communication.Protocol_Routing.Protocol_Routing_Type
   --    for routing between protocols.
   Com_SM_L   : VN.Communication.Protocol_Routing.Protocol_Routing_Type;
   Com_SM_CAN : VN.Communication.Protocol_Routing.Protocol_Routing_Type;

   -- 2. Create a VN.Communication.Protocol_Routing.Protocol_Routing_Type
   --    for routing within Protected Object Subnet (PO_Router)
   PO_Router        : aliased VN.Communication.Protocol_Routing.Protocol_Routing_Type;
   PO_Router_SM_CAN : aliased VN.Communication.Protocol_Routing.Protocol_Routing_Type;

   -- 3. Create all needed PO_Wrappers for the SM-L

   PO_Wrapper_To_App2: aliased VN.Communication.PO_Wrapper.VN_PO_Wrapper(
                                                            PO_SM_L_To_App2'Access,
                                                            CUUID_SM'Access,
                                                            VN.Message.SM_L,
                                                            True);

   PO_Wrapper_SM_L_To_SM_CAN: aliased VN.Communication.PO_Wrapper.VN_PO_Wrapper(
                                                            PO_SM_L_To_SM_CAN'Access,
                                                            CUUID_SM'Access,
                                                            VN.Message.SM_L,
                                                            False);

   PO_Wrapper_SM_CAN_To_SM_L: aliased VN.Communication.PO_Wrapper.VN_PO_Wrapper(
                                                            PO_SM_L_To_SM_CAN'Access,
                                                            CUUID_SM_CAN'Access,
                                                            VN.Message.SM_x,
                                                            True);

   -- 4. Add PO_Router to Protocol_Router (during run time, main.adb).
   -- 5. Add all PO_Wrappers to the PO_Router (during run time, main.adb).


--- ************* CAN ****************************
--     theFilter : aliased VN.Communication.CAN.CAN_Filtering.CAN_Filter_Type;
--     CANPeriod : aliased Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds(10);
--     U1 : aliased VN.Communication.CAN.UCID := VN.Communication.CAN.UCID(10);
--
--     CANInterface : aliased VN.Communication.CAN.CAN_Interface.CAN_Interface_Type
--       (U1'Unchecked_Access, CUUID_SM_x'Unchecked_Access,
--        theFilter'Unchecked_Access, VN.Communication.CAN.CAN_Interface.SM_CAN);
--
--     myTask : aliased VN.Communication.CAN.Can_Task.CAN_Task_Type
--       (CANInterface'Access, System.Priority'Last, CANPeriod'Access, theFilter'Unchecked_Access);

-- ************************************************

end Global_Settings;
