
with CAN_Driver;
with GNAT.IO;

procedure CAN_Driver_Main is 
   t : Integer := 0;
   
begin 

   t := Integer(CAN_Driver.Test);
   GNAT.IO.Put_Line("Test= " & t'Img);
end CAN_Driver_Main;
