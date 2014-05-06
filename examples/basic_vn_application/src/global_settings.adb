package body Global_Settings is

   -- Common start time for all applications.
   protected body Start_time is

      procedure Get(Time: out Ada.Real_Time.Time) is
      begin
         if First_Time then
            First_Time := False;
            Start := Ada.Real_Time.Clock;
         end if;
         Time := Start;
      end Get;

   end Start_Time;

end Global_Settings;
