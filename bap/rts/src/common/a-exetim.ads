------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                   A D A . E X E C U T I O N _ T I M E                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Task_Identification;
with Ada.Real_Time;

package Ada.Execution_Time is

   type CPU_Time is private;

   CPU_Time_First : constant CPU_Time;
   CPU_Time_Last  : constant CPU_Time;
   CPU_Time_Unit  : constant Duration := Ada.Real_Time.Time_Unit;
   CPU_Tick       : constant Ada.Real_Time.Time_Span;

   function Clock
     (T : Ada.Task_Identification.Task_Id :=
            Ada.Task_Identification.Current_Task)
      return CPU_Time;

   function "+"
     (Left  : CPU_Time;
      Right : Ada.Real_Time.Time_Span) return CPU_Time;

   function "+"
     (Left  : Ada.Real_Time.Time_Span;
      Right : CPU_Time) return CPU_Time;

   function "-"
     (Left  : CPU_Time;
      Right : Ada.Real_Time.Time_Span) return CPU_Time;

   function "-"
     (Left  : CPU_Time;
      Right : CPU_Time) return Ada.Real_Time.Time_Span;

   function "<"  (Left, Right : CPU_Time) return Boolean;
   function "<=" (Left, Right : CPU_Time) return Boolean;
   function ">"  (Left, Right : CPU_Time) return Boolean;
   function ">=" (Left, Right : CPU_Time) return Boolean;

   procedure Split
     (T  : CPU_Time;
      SC : out Ada.Real_Time.Seconds_Count;
      TS : out Ada.Real_Time.Time_Span);

   function Time_Of
      (SC : Ada.Real_Time.Seconds_Count;
       TS : Ada.Real_Time.Time_Span := Ada.Real_Time.Time_Span_Zero)
       return CPU_Time;

   Interrupt_Clocks_Supported          : constant Boolean := True;
   Separate_Interrupt_Clocks_Supported : constant Boolean := True;

   function Clock_For_Interrupts return CPU_Time;

private

   type CPU_Time is new Ada.Real_Time.Time;

   CPU_Time_First : constant CPU_Time  := CPU_Time (Ada.Real_Time.Time_First);
   CPU_Time_Last  : constant CPU_Time  := CPU_Time (Ada.Real_Time.Time_Last);

   CPU_Tick : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Tick;

   pragma Import (Intrinsic, "<");
   pragma Import (Intrinsic, "<=");
   pragma Import (Intrinsic, ">");
   pragma Import (Intrinsic, ">=");

end Ada.Execution_Time;
