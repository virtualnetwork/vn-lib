with Ada.Real_Time;
with System;
with Logging.Print_Out;
with VN.Message.Factory;

--  with VN.Communication.PO;
--  with VN.Communication.PO_Wrapper;
--  with VN.Communication.PO_Routing;
-- with VN.Communication.Temp_Protocol_Routing;
--  with VN.Communication.Protocol_Routing;

-- CAN:
with VN.Communication.CAN;
with VN.Communication.CAN.CAN_Filtering;
with VN.Communication.CAN.CAN_Interface;
with VN.Communication.CAN.Can_Task;

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
   CUUID_App   : aliased VN.VN_CUUID := (others => 88);
   Cycle_Time_Applications : constant Positive := 2110000;
   Cycle_Time_SM_L         : constant Positive := 500000;

--- ************* CAN ****************************
   theFilter : aliased VN.Communication.CAN.CAN_Filtering.CAN_Filter_Type;
   CANPeriod : aliased Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds(300);
   U1 : aliased VN.Communication.CAN.UCID := VN.Communication.CAN.UCID(30);

   Com_Application : aliased VN.Communication.CAN.CAN_Interface.CAN_Interface_Type
     (U1'Unchecked_Access, CUUID_App'Unchecked_Access,
      theFilter'Unchecked_Access, VN.Communication.CAN.CAN_Interface.Node);

   myTask : aliased VN.Communication.CAN.Can_Task.CAN_Task_Type
     (Com_Application'Access, System.Priority'Last, CANPeriod'Access, theFilter'Unchecked_Access);

-- ************************************************

end Global_Settings;
