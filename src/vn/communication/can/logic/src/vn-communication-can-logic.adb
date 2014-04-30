-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- VN.Communication.CAN.Logic is a package that implements the logic 
-- of the VN-CAN protocol itself. 

with VN;

package body VN.Communication.CAN.Logic is

   procedure DebugOutput(str : String; level : Integer; newLine : boolean := true) is
   begin
      if level <= GIVE_DEBUG_OUTPUT then
         VN.Text_IO.Put(str);
         if newLine then
            VN.Text_IO.New_Line;
         end if;
      end if;
   end DebugOutput;

--     procedure Assignment (destination : out VN_Message_Internal; source : in VN_Message_Internal) is
--     begin
--  
--        VN.Message.Assignment(destination.Data, source.Data);
--  
--        destination.NumBytes := source.NumBytes;
--        destination.Receiver := source.Receiver;
--        destination.Sender := source.Sender;
--     end Assignment;

end VN.Communication.CAN.Logic;
