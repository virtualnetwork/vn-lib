with Ada.Real_Time;
with Ada.Text_IO;
with Global_Settings;
with VN.Message.Local_Hello;

package body Subnet_Manager_Local is

   task body SM_L is
      use Ada.Real_Time;
      use VN;
      use VN.Message.Local_Hello;
      i: Integer := 1;

      Basic_Msg: VN.Message.VN_Message_Basic;
      Local_Hello_Msg: VN.Message.Local_Hello.VN_Message_Local_Hello;
      Status: VN.Receive_Status;
      Version: VN.Message.VN_Version;

      Next_Period : Ada.Real_Time.Time;
      Period : constant Ada.Real_Time.Time_Span :=
                           Ada.Real_Time.Microseconds(Cycle_Time);
   begin
      Ada.Text_IO.Put_Line("Task type SM_L - Start, ID: "
                              & Integer'Image(Task_ID));

      Global_Settings.Start_Time.Get(Next_Period);
      loop
         delay until Next_Period;
         ----------------------------

         Global_Settings.Com_SM_L.Receive(Basic_Msg, Status);

         if Status = VN.MSG_RECEIVED_NO_MORE_AVAILABLE or
            Status = VN.MSG_RECEIVED_MORE_AVAILABLE    then

            -- TODO: Check OpCode and convert to correct type
            To_Local_Hello(Basic_Msg, Local_Hello_Msg);

            Ada.Text_IO.Put("SM_L Received: " & VN.Message.VN_Component_Type'Image(Local_Hello_Msg.Component_Type));
            Ada.Text_IO.Put_Line("");

         end if;

         ----------------------------
         Next_Period := Next_Period + Period;
         i := i + 1;
         exit when i = 40;
      end loop;
      Ada.Text_IO.Put_Line("Task type SM_L - End, ID:"
                              & Integer'Image(Task_ID));
   end SM_L;

   -- Start one instance of the SM-L
   SM_L1: SM_L(20, 100000, 80, 3);

end Subnet_Manager_Local;
