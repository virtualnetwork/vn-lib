with Ada.Real_Time;
with Ada.Text_IO;
with Global_Settings;
with VN.Application_Information;
with VN.Message.Local_Hello;

package body Subnet_Manager_Local is

   task body SM_L is
      use Ada.Real_Time;
      use VN;
      use VN.Message.Local_Hello;
      i: Integer := 1;

      SM_L_Info: VN.Application_Information.VN_Application_Information;

      Basic_Msg: VN.Message.VN_Message_Basic;
      Local_Hello_Msg: VN.Message.Local_Hello.VN_Message_Local_Hello;
      Status: VN.Receive_Status;
      Version: VN.Message.VN_Version;

      Next_Period : Ada.Real_Time.Time;
      Period : constant Ada.Real_Time.Time_Span :=
                           Ada.Real_Time.Microseconds(Cycle_Time);
   begin
      SM_L_Info.Component_Type := VN.Message.SM_L;
      SM_L_Info.Logical_Address := 10;
      --Ada.Text_IO.Put_Line("Task type SM_L - Start, ID: "
      --                        & Integer'Image(Task_ID));

      Global_Settings.Start_Time.Get(Next_Period);
      loop
         delay until Next_Period;
         ----------------------------
         Ada.Text_IO.Put_Line("STAT SM-L: Runs");

         -- TODO: This subprogram never returns. Debug Communication chain.
         Global_Settings.Com_SM_L.Receive(Basic_Msg, Status);

         Ada.Text_IO.Put_Line("STAT SM-L: Message handling started.");

         if Status = VN.NO_MSG_RECEIVED then
            Ada.Text_IO.Put_Line("RECV SM-L: Empty.");
         end if;

         if Status = VN.MSG_RECEIVED_NO_MORE_AVAILABLE or
            Status = VN.MSG_RECEIVED_MORE_AVAILABLE    then

            Ada.Text_IO.Put("RECV SM-L: ");
            Global_Settings.Logger.Log(Basic_Msg);

            -- TODO: Check OpCode and convert to correct type
            To_Local_Hello(Basic_Msg, Local_Hello_Msg);

            --Ada.Text_IO.Put("SM_L Received: " & VN.Message.VN_Component_Type'Image(Local_Hello_Msg.Component_Type));
            --Ada.Text_IO.Put_Line("");

         end if;

         ----------------------------
         Next_Period := Next_Period + Period;
         i := i + 1;
         exit when i = 40;
      end loop;

      Ada.Text_IO.Put_Line("STAT SM-L: Stops");
      --Ada.Text_IO.Put_Line("Task type SM_L - End, ID:"
       --                      & Integer'Image(Task_ID));
   end SM_L;

   -- Start one instance of the SM-L
   SM_L1: SM_L(20, 100000, 80, 3);

end Subnet_Manager_Local;
