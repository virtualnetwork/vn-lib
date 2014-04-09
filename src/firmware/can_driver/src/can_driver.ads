with Interfaces;
with Interfaces.C;

package CAN_Driver is

   type Data_Array is array(0..7) of Interfaces.C.signed_char;

   type CAN_Message_Physical is
      record
         ID		: Interfaces.C.unsigned; --??
         Length   	: Interfaces.C.unsigned; --??
         Data     	: Data_Array;
      end record;
   pragma Convention (C, CAN_Message_Physical);

   type CAN_Message_Physical_Access is access all CAN_Message_Physical;

   procedure CAN_Get_Msg_Filter_Mask(x : Interfaces.C.int; y : Interfaces.C.unsigned_char; z : Interfaces.C.unsigned_char);
   pragma Import(C, CAN_Get_Msg_Filter_Mask, "MSS_CAN_get_msg_filter_mask");

--CAN_MSGOBJECT
--CAN_RXMSGOBJECT
--mss_can_instance_t
--MSS_CAN_send_message_n(&g_can0, 0, &pMsg);
--MSS_CAN_get_message_n(&g_can0, 0, &rx_buf)
--MSS_CAN_init(&g_can0, CAN_SPEED_32M_500K, (PCAN_CONFIG_REG)0, 6, 6);
--CAN_SPEED_32M_500K
--PCAN_CONFIG_REG

--MSS_CAN_set_mode(&g_can0,CANOP_MODE_NORMAL);
--MSS_CAN_start(&g_can0);
--MSS_CAN_config_buffer_n(&g_can0, 0, &rx_msg);



--     procedure Send(msg : CAN_Message_Physical_Access);
--     pragma Import(C, Send, "Send_CAN_Message");
--
--     function Receive(msg : CAN_Message_Physical_Access) return Interfaces.C.int;
--     pragma Import(C, Receive, "Receive_CAN_Message");
--
--     function Test return Interfaces.C.int;
--     pragma Import(C, Test, "test");

   procedure Temp(msg : CAN_Message_Physical);

end CAN_Driver;
