with Ada.Text_IO;

package body VN.Communication.CAN.Logic is

   procedure DebugOutput(str : String; level : Integer; newLine : boolean := true) is
   begin
      if level <= GIVE_DEBUG_OUTPUT then
         Ada.Text_IO.Put(str);
         if newLine then
            Ada.Text_IO.New_Line;
         end if;
      end if;
   end DebugOutput;

end VN.Communication.CAN.Logic;
