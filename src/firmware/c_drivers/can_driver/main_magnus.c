
// This file has been written by Magnus Norgren, no rights reserved.

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

#include "mss_can.h"

#define CAN_TX

int main()
{
    int32_t err;
    int32_t i;
    volatile int32_t dly;
    CAN_CONFIG_REG CANReg;
    CAN_MSGOBJECT tx_msg;
    CAN_FILTEROBJECT pFilter;
    CAN_MSGOBJECT rx_msg;

    CANReg.CFG_BITRATE      = 219;
    CANReg.CFG_TSEG1        = 12;
    CANReg.CFG_TSEG2        = 1;
    CANReg.AUTO_RESTART     = 0;
    CANReg.CFG_SJW          = 0;
    CANReg.SAMPLING_MODE    = 0;
    CANReg.EDGE_MODE        = 0;
    CANReg.ENDIAN           = 1;

   // printf("\r\n\n CAN test\r\n\n");

    printf("\r\n Initializing CAN-Controller... ");
    fflush(stdout);

/*
    err = MSS_CAN_init(
        &g_can0,
        CAN_SPEED_MANUAL,
        &CANReg,
        6, // Number of Basic CAN rx mailboxes
        6  // Number of Basic CAN tx mailboxes
    );
*/

    err = MSS_CAN_init(
        &g_can0,
        //CAN_SPEED_32M_10K,
        CAN_SET_BITRATE(24) | CAN_SET_TSEG1(12) | CAN_SET_TSEG2(1), // 100kbit/s ???
        (PCAN_CONFIG_REG)0,
        6,
        6
    );

    if (err == CAN_OK) {
        printf("Done!\r\n");
    }
    else {
        printf("Failed! Error: %ld\r\n\n", err);
    }


    MSS_CAN_set_mode(&g_can0, CANOP_MODE_NORMAL);
    MSS_CAN_start(&g_can0);

    tx_msg.ID       = 0x120;
    //tx_msg.DATALOW  = 0xA5A5A5A5;
    //tx_msg.DATAHIGH = 0x5A5A5A5A;
    tx_msg.DATALOW  = 0x00000000;
    tx_msg.DATAHIGH = 0x00000000;
    //tx_msg.L      = ((0 << 20) | 0x00080000);
    //tx_msg.L =((1<<20)| 0x00080000);
    tx_msg.NA0      = 1;
    tx_msg.DLC      = 8;
    tx_msg.IDE      = 1;
    tx_msg.RTR      = 0;

    pFilter.ACR.L   = 0x00000000 ;
    pFilter.AMR.L   = 0xFFFFFFFF;
    pFilter.AMCR_D.MASK = 0xFFFF;
    pFilter.AMCR_D.CODE = 0x00;

    err = MSS_CAN_config_buffer(&g_can0, &pFilter);
    if (err != CAN_OK) {
        printf("\n\r Message Buffer configuration Error\r\n");
    }

#ifdef CAN_TX
    while (1) {
        err = MSS_CAN_send_message_ready(&g_can0);
        if (err == CAN_OK) {

            printf(" Sending msg... ");
            fflush(stdout);

            err = MSS_CAN_send_message(&g_can0, &tx_msg);
            if (err == CAN_VALID_MSG) {
                printf("Done!\r\n");
            }
            else {
                printf("Failed! Error: %ld\r\n", err);
            }

            tx_msg.DATALOW++;
            tx_msg.DATAHIGH--;

            dly = 100000;
            while (dly--);
        }

    }
#else
    while (1) {
        if (MSS_CAN_get_message_av(&g_can0) == CAN_VALID_MSG) {
            printf("\r\n New CAN msg (bytes):\r\n");
            MSS_CAN_get_message(&g_can0, &rx_msg);

            for (i = 0; i < 8; i++) {
                printf(" 0x%X\r\n", 0xFF & rx_msg.DATA[i]);
            }

            printf("\r\n New CAN msg (32-bit unsigned):\r\n");
            printf(" 0x%lX\r\n", rx_msg.DATAHIGH);
            printf(" 0x%lX\r\n", rx_msg.DATALOW);
        }
    }
#endif

    while (1);

    return 0;
}
