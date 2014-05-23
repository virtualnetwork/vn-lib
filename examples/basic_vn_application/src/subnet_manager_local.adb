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
      Counter_For_Testing: Integer := 1;

      SM_L_Info: VN.Application_Information.VN_Application_Information;

      Basic_Msg: VN.Message.VN_Message_Basic;
      Local_Hello_Msg: VN.Message.Local_Hello.VN_Message_Local_Hello;

      Recv_Status: VN.Receive_Status;
      Send_Status: VN.Send_Status;

      Version: VN.Message.VN_Version;

      Next_Period : Ada.Real_Time.Time;
      Period : constant Ada.Real_Time.Time_Span :=
                           Ada.Real_Time.Microseconds(Cycle_Time);
   begin
      SM_L_Info.Component_Type := VN.Message.SM_L;
      SM_L_Info.Logical_Address := 2;

      Global_Settings.Start_Time.Get(Next_Period);
      Ada.Text_IO.Put_Line("SM-L STAT: Starts.");

      ----------------------------
      loop
         delay until Next_Period;

         ----------------------------
         -- Receive loop
         ----------------------------
         Global_Settings.Com_SM_L.Receive(Basic_Msg, Recv_Status);

         --Ada.Text_IO.Put_Line("SM-L RECV Status: " &
         --                       VN.Receive_Status'Image(Recv_Status));

         if Recv_Status = VN.NO_MSG_RECEIVED then
            Ada.Text_IO.Put_Line("SM-L RECV: Empty.");

         elsif Recv_Status = VN.MSG_RECEIVED_NO_MORE_AVAILABLE or
            Recv_Status = VN.MSG_RECEIVED_MORE_AVAILABLE    then

            Ada.Text_IO.Put("SM-L RECV: ");
            Global_Settings.Logger.Log(Basic_Msg);

            -- TODO: Check OpCode and convert to correct type
            To_Local_Hello(Basic_Msg, Local_Hello_Msg);

         end if;

         ----------------------------
         -- Send loop
         ----------------------------
        -- if SM_L_Info.Has_Logical_Address then
        --    null;
        -- end if;
         --if SM_L.Has_Logical_Address then
         --   null;
         --elsif not App_Info.Has_Logical_Address then
         --   -- Prepare message to be sent
         --   Basic_Msg := VN.Message.Factory.Create(VN.Message.Type_Local_Hello);
         --   Basic_Msg.Header.Destination := 0;
         --   To_Local_Hello(Basic_Msg, Local_Hello_Msg);
         --   App_Info.Get_Application_Information(Local_Hello_Msg);
         --   To_Basic(Local_Hello_Msg, Basic_Msg);

         --   -- Send message
         --   Ada.Text_IO.Put("APPL SEND: ");
         --   Global_Settings.Logger.Log(Basic_Msg);
         --   Global_Settings.Com_Application.Send(Basic_Msg, Send_Status);
         --end if;

         Next_Period := Next_Period + Period;
         Counter_For_Testing := Counter_For_Testing + 1;
         exit when Counter_For_Testing = 30;
      end loop;
      ----------------------------

      Ada.Text_IO.Put_Line("SM-L Stop.");

   end SM_L;

   -- Start one instance of the SM-L
   SM_L1: SM_L(20, 1000000, 80, 3);

end Subnet_Manager_Local;
