with Interfaces;
with Interfaces.C;

package CAN_Driver is


-- C specific data structures -------------------------------------------------
-- see mss_can.h and mss_can.c for detals


--     type CAN_DEVICE_Access is CAN_DEVICE;
--
--     type mss_can_instance_t is
--        record
--           hw_reg 	 : CAN_DEVICE_Access;
--           basic_can_rx_mb : Interfaces.c.unsigned_char;
--           basic_can_tx_mb : Interfaces.c.unsigned_char;
--        end record;
--     pragma Convention (C, mss_can_instance_t);
--
--     type NO_ID_Type is mod 2**3;
--     for NO_ID_Type'Size use 3;
--     type CAN_ID_Type is mod 2**29;
--     for CAN_ID_Type'Size use 29;
--
--     type Data_Array is array(0..7) of Interfaces.C.signed_char;
--
--     type NA0_Type is mod 2**16;
--     for NA0_Type'Size use 16;
--     type DLC_Type is mod 2**4;
--     for DLC_Type'Size use 4;
--     type IDE_Type is mod 2**1;
--     for IDE_Type'Size use 1;
--     type RTR_Type is mod 2**1;
--     for RTR_Type'Size use 1;
--     type NA1_Type is mod 2**10;
--     for NA1_Type'Size use 10;
--
--     type CAN_MSGOBJECT is
--        record
--           No_ID		: NO_ID_Type;
--           ID		: CAN_ID_Type;
--           Data     	: Data_Array;
--           NA0		: NA0_Type;
--           DLC		: DLC_Type;
--           IDE		: IDE_Type;
--           RTR		: RTR_Type;
--           NA1		: NA1_Type;
--        end record;
--     pragma Convention (C, CAN_MSGOBJECT);
--
--     for CAN_MSGOBJECT use record
--        No_ID	at 0 range 0..3
--        ID 	at 0 range 4..31;
--        Data	at 0 range 32..95;
--        NA0	at 0 range 96..111;
--        DLC	at 0 range 112..115;
--        IDE	at 0 range 116..116;
--        RTR	at 0 range 117..117;
--        NA1	at 0 range 118..127;
--     end record;
--     for CAN_MSGOBJECT'Bit_Order use High_Order_First;
--
--     type PCAN_MSGOBJECT is access all CAN_MSGOBJECT;
--
--     type CAN_TXMSGOBJECT is
--        record
--           ID		: Interfaces.C.unsigned;
--           Length   	: Interfaces.C.unsigned;
--           Data     	: Data_Array;
--           NA0		: NA0_Type;
--           DLC		: DLC_Type;
--           IDE		: IDE_Type;
--           RTR		: RTR_Type;
--           NA1		: NA1_Type;
--        end record;
--     pragma Convention (C, CAN_TXMSGOBJECT);
--
--     for CAN_TXMSGOBJECT use record
--        ID 	at 0 range 0..15;
--        Length 	at 0 range 16..31;
--        Data	at 0 range 32..95;
--        NA0	at 0 range 96..111;
--        DLC	at 0 range 112..115;
--        IDE	at 0 range 116..116;
--        RTR	at 0 range 117..117;
--        NA1	at 0 range 118..127;
--     end record;
--     for CAN_TXMSGOBJECT'Bit_Order use High_Order_First;
--
--     type PCAN_MSGOBJECT is access all CAN_TXMSGOBJECT;


   -- Imports of procedures and functions -------------------------------------------------

--     procedure CAN_Get_Msg_Filter_Mask(x : Interfaces.C.int; y : Interfaces.C.unsigned_char; z : Interfaces.C.unsigned_char);
--     pragma Import(C, CAN_Get_Msg_Filter_Mask, "MSS_CAN_get_msg_filter_mask");

--CAN_MSGOBJECT ok
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
--MSS_CAN_set_int_ebl()
--MSS_CAN_set_global_int_ebl

   --MSS_CAN_get_global_int_ebl??
   --MSS_CAN_clear_int_status()??


--     procedure Send(msg : CAN_Message_Physical_Access);
--     pragma Import(C, Send, "Send_CAN_Message");
--
--     function Receive(msg : CAN_Message_Physical_Access) return Interfaces.C.int;
--     pragma Import(C, Receive, "Receive_CAN_Message");
--
   function Test return Interfaces.C.int;
   pragma Import(C, Test, "test");

   procedure Init;

end CAN_Driver;
