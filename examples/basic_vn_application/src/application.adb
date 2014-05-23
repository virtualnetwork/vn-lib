with Ada.Real_Time;
with Ada.Text_IO;
with Global_Settings;
with VN.Application_Information;
with VN.Message.Factory;
with VN.Message.Local_Hello;

package body Application is

   task body VN_Application is
      use Ada.Real_Time;
      use VN;
      use VN.Message.Local_Hello;

      i: Integer := 1;

      App_Info: VN.Application_Information.VN_Application_Information;

      Basic_Msg: VN.Message.VN_Message_Basic;
      Local_Hello_Msg: VN.Message.Local_Hello.VN_Message_Local_Hello;

      Recv_Status: VN.Receive_Status;
      Send_Status: VN.Send_Status;

      Next_Period : Ada.Real_Time.Time;
      Period : constant Ada.Real_Time.Time_Span :=
                           Ada.Real_Time.Microseconds(Cycle_Time);
   begin
      App_Info.Component_Type := VN.Message.Other;
      App_Info.Logical_Address := 2;
      -- App_Info.CUUID := ???;

      Global_Settings.Start_Time.Get(Next_Period);
      Ada.Text_IO.Put_Line("APPL STAT: Starts.");

      ----------------------------
      loop
         delay until Next_Period;

         ----------------------------
         -- Receive loop
         ----------------------------
         Global_Settings.Com_Application.Receive(Basic_Msg, Recv_Status);

         if Recv_Status = VN.NO_MSG_RECEIVED then
            Ada.Text_IO.Put_Line("APPL RECV: Empty.");
         elsif Recv_Status = VN.MSG_RECEIVED_NO_MORE_AVAILABLE or
            Recv_Status = VN.MSG_RECEIVED_MORE_AVAILABLE    then

            Ada.Text_IO.Put("APPL RECV: ");
            Global_Settings.Logger.Log(Basic_Msg);

            -- TODO: Check OpCode and convert to correct type
            To_Local_Hello(Basic_Msg, Local_Hello_Msg);

         end if;

         ----------------------------
         -- Send loop
         ----------------------------
         if App_Info.Has_Logical_Address then
            null;
         elsif not App_Info.Has_Logical_Address then
            null;
            -- Prepare message to be sent
            --Basic_Msg := VN.Message.Factory.Create(VN.Message.Type_Local_Hello);
            --Basic_Msg.Header.Destination := 0;
            --To_Local_Hello(Basic_Msg, Local_Hello_Msg);
            --App_Info.Get_Application_Information(Local_Hello_Msg);
            --To_Basic(Local_Hello_Msg, Basic_Msg);

            -- Send message
            --Ada.Text_IO.Put("APPL SEND: ");
            --Global_Settings.Logger.Log(Basic_Msg);
            --Global_Settings.Com_Application.Send(Basic_Msg, Send_Status);
         end if;


         Next_Period := Next_Period + Period;
         i := i + 1;
         exit when i = 6;
      end loop;
      ----------------------------

   end VN_Application;

   -- Start one instance of the SM-L
   App: VN_Application(30, 5000000, 30, 3);

end Application;
