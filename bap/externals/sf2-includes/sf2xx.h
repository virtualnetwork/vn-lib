/* ---------------------------------------------------------------------------
--                                                                          --
--                           GNAT RAVENSCAR for NXT                         --
--                                                                          --
--                       Copyright (C) 2010, AdaCore                        --
--                                                                          --
-- This is free software; you can  redistribute it  and/or modify it under  --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. This is distributed in the hope that it will be useful, but WITH-  --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--------------------------------------------------------------------------- */

#ifndef Sf2M3SAM7S_H
#  define Sf2M3SAM7S_H

#  define CLOCK_FREQUENCY 48054850


// AIC peripheral ids
#  define Sf2M3_PERIPHERAL_ID_FIQ		0
#  define Sf2M3_PERIPHERAL_ID_SYSIRQ	1
#  define Sf2M3_PERIPHERAL_ID_PIOA		2
#  define Sf2M3_PERIPHERAL_ID_ADC		4
#  define Sf2M3_PERIPHERAL_ID_SPI		5
#  define Sf2M3_PERIPHERAL_ID_US0		6
#  define Sf2M3_PERIPHERAL_ID_US1		7
#  define Sf2M3_PERIPHERAL_ID_SSC		8
#  define Sf2M3_PERIPHERAL_ID_TWI		9
#  define Sf2M3_PERIPHERAL_ID_PWMC		10
#  define Sf2M3_PERIPHERAL_ID_UDP		11
#  define Sf2M3_PERIPHERAL_ID_TC0		12
#  define Sf2M3_PERIPHERAL_ID_TC1		13
#  define Sf2M3_PERIPHERAL_ID_TC2		14
#  define Sf2M3_PERIPHERAL_ID_IRQ0		30
#  define Sf2M3_PERIPHERAL_ID_IRQ1		31


typedef volatile unsigned int Sf2M3_REG;	// Hardware register definition

// *****************************************************************************
//              SOFTWARE API DEFINITION  FOR System Peripherals
// *****************************************************************************
typedef struct _Sf2M3S_SYSC {
  Sf2M3_REG SYSC_AIC_SMR[32];	// Source Mode Register
  Sf2M3_REG SYSC_AIC_SVR[32];	// Source Vector Register
  Sf2M3_REG SYSC_AIC_IVR;	// IRQ Vector Register
  Sf2M3_REG SYSC_AIC_FVR;	// FIQ Vector Register
  Sf2M3_REG SYSC_AIC_ISR;	// Interrupt Status Register
  Sf2M3_REG SYSC_AIC_IPR;	// Interrupt Pending Register
  Sf2M3_REG SYSC_AIC_IMR;	// Interrupt Mask Register
  Sf2M3_REG SYSC_AIC_CISR;	// Core Interrupt Status Register
  Sf2M3_REG Reserved0[2];	//
  Sf2M3_REG SYSC_AIC_IECR;	// Interrupt Enable Command Register
  Sf2M3_REG SYSC_AIC_IDCR;	// Interrupt Disable Command Register
  Sf2M3_REG SYSC_AIC_ICCR;	// Interrupt Clear Command Register
  Sf2M3_REG SYSC_AIC_ISCR;	// Interrupt Set Command Register
  Sf2M3_REG SYSC_AIC_EOICR;	// End of Interrupt Command Register
  Sf2M3_REG SYSC_AIC_SPU;	// Spurious Vector Register
  Sf2M3_REG SYSC_AIC_DCR;	// Debug Control Register (Protect)
  Sf2M3_REG Reserved1[1];	//
  Sf2M3_REG SYSC_AIC_FFER;	// Fast Forcing Enable Register
  Sf2M3_REG SYSC_AIC_FFDR;	// Fast Forcing Disable Register
  Sf2M3_REG SYSC_AIC_FFSR;	// Fast Forcing Status Register
  Sf2M3_REG Reserved2[45];	//
  Sf2M3_REG SYSC_DBGU_CR;	// Control Register
  Sf2M3_REG SYSC_DBGU_MR;	// Mode Register
  Sf2M3_REG SYSC_DBGU_IER;	// Interrupt Enable Register
  Sf2M3_REG SYSC_DBGU_IDR;	// Interrupt Disable Register
  Sf2M3_REG SYSC_DBGU_IMR;	// Interrupt Mask Register
  Sf2M3_REG SYSC_DBGU_CSR;	// Channel Status Register
  Sf2M3_REG SYSC_DBGU_RHR;	// Receiver Holding Register
  Sf2M3_REG SYSC_DBGU_THR;	// Transmitter Holding Register
  Sf2M3_REG SYSC_DBGU_BRGR;	// Baud Rate Generator Register
  Sf2M3_REG Reserved3[7];	//
  Sf2M3_REG SYSC_DBGU_C1R;	// Chip ID1 Register
  Sf2M3_REG SYSC_DBGU_C2R;	// Chip ID2 Register
  Sf2M3_REG SYSC_DBGU_FNTR;	// Force NTRST Register
  Sf2M3_REG Reserved4[45];	//
  Sf2M3_REG SYSC_DBGU_RPR;	// Receive Pointer Register
  Sf2M3_REG SYSC_DBGU_RCR;	// Receive Counter Register
  Sf2M3_REG SYSC_DBGU_TPR;	// Transmit Pointer Register
  Sf2M3_REG SYSC_DBGU_TCR;	// Transmit Counter Register
  Sf2M3_REG SYSC_DBGU_RNPR;	// Receive Next Pointer Register
  Sf2M3_REG SYSC_DBGU_RNCR;	// Receive Next Counter Register
  Sf2M3_REG SYSC_DBGU_TNPR;	// Transmit Next Pointer Register
  Sf2M3_REG SYSC_DBGU_TNCR;	// Transmit Next Counter Register
  Sf2M3_REG SYSC_DBGU_PTCR;	// PDC Transfer Control Register
  Sf2M3_REG SYSC_DBGU_PTSR;	// PDC Transfer Status Register
  Sf2M3_REG Reserved5[54];	//
  Sf2M3_REG SYSC_PIOA_PER;	// PIO Enable Register
  Sf2M3_REG SYSC_PIOA_PDR;	// PIO Disable Register
  Sf2M3_REG SYSC_PIOA_PSR;	// PIO Status Register
  Sf2M3_REG Reserved6[1];	//
  Sf2M3_REG SYSC_PIOA_OER;	// Output Enable Register
  Sf2M3_REG SYSC_PIOA_ODR;	// Output Disable Register
  Sf2M3_REG SYSC_PIOA_OSR;	// Output Status Register
  Sf2M3_REG Reserved7[1];	//
  Sf2M3_REG SYSC_PIOA_IFER;	// Input Filter Enable Register
  Sf2M3_REG SYSC_PIOA_IFDR;	// Input Filter Disable Register
  Sf2M3_REG SYSC_PIOA_IFSR;	// Input Filter Status Register
  Sf2M3_REG Reserved8[1];	//
  Sf2M3_REG SYSC_PIOA_SODR;	// Set Output Data Register
  Sf2M3_REG SYSC_PIOA_CODR;	// Clear Output Data Register
  Sf2M3_REG SYSC_PIOA_ODSR;	// Output Data Status Register
  Sf2M3_REG SYSC_PIOA_PDSR;	// Pin Data Status Register
  Sf2M3_REG SYSC_PIOA_IER;	// Interrupt Enable Register
  Sf2M3_REG SYSC_PIOA_IDR;	// Interrupt Disable Register
  Sf2M3_REG SYSC_PIOA_IMR;	// Interrupt Mask Register
  Sf2M3_REG SYSC_PIOA_ISR;	// Interrupt Status Register
  Sf2M3_REG SYSC_PIOA_MDER;	// Multi-driver Enable Register
  Sf2M3_REG SYSC_PIOA_MDDR;	// Multi-driver Disable Register
  Sf2M3_REG SYSC_PIOA_MDSR;	// Multi-driver Status Register
  Sf2M3_REG Reserved9[1];	//
  Sf2M3_REG SYSC_PIOA_PPUDR;	// Pull-up Disable Register
  Sf2M3_REG SYSC_PIOA_PPUER;	// Pull-up Enable Register
  Sf2M3_REG SYSC_PIOA_PPUSR;	// Pad Pull-up Status Register
  Sf2M3_REG Reserved10[1];	//
  Sf2M3_REG SYSC_PIOA_ASR;	// Select A Register
  Sf2M3_REG SYSC_PIOA_BSR;	// Select B Register
  Sf2M3_REG SYSC_PIOA_ABSR;	// AB Select Status Register
  Sf2M3_REG Reserved11[9];	//
  Sf2M3_REG SYSC_PIOA_OWER;	// Output Write Enable Register
  Sf2M3_REG SYSC_PIOA_OWDR;	// Output Write Disable Register
  Sf2M3_REG SYSC_PIOA_OWSR;	// Output Write Status Register
  Sf2M3_REG Reserved12[469];	//
  Sf2M3_REG SYSC_PMC_SCER;	// System Clock Enable Register
  Sf2M3_REG SYSC_PMC_SCDR;	// System Clock Disable Register
  Sf2M3_REG SYSC_PMC_SCSR;	// System Clock Status Register
  Sf2M3_REG Reserved13[1];	//
  Sf2M3_REG SYSC_PMC_PCER;	// Peripheral Clock Enable Register
  Sf2M3_REG SYSC_PMC_PCDR;	// Peripheral Clock Disable Register
  Sf2M3_REG SYSC_PMC_PCSR;	// Peripheral Clock Status Register
  Sf2M3_REG Reserved14[1];	//
  Sf2M3_REG SYSC_PMC_MOR;	// Main Oscillator Register
  Sf2M3_REG SYSC_PMC_MCFR;	// Main Clock  Frequency Register
  Sf2M3_REG Reserved15[1];	//
  Sf2M3_REG SYSC_PMC_PLLR;	// PLL Register
  Sf2M3_REG SYSC_PMC_MCKR;	// Master Clock Register
  Sf2M3_REG Reserved16[3];	//
  Sf2M3_REG SYSC_PMC_PCKR[8];	// Programmable Clock Register
  Sf2M3_REG SYSC_PMC_IER;	// Interrupt Enable Register
  Sf2M3_REG SYSC_PMC_IDR;	// Interrupt Disable Register
  Sf2M3_REG SYSC_PMC_SR;		// Status Register
  Sf2M3_REG SYSC_PMC_IMR;	// Interrupt Mask Register
  Sf2M3_REG Reserved17[36];	//
  Sf2M3_REG SYSC_RSTC_RCR;	// Reset Control Register
  Sf2M3_REG SYSC_RSTC_RSR;	// Reset Status Register
  Sf2M3_REG SYSC_RSTC_RMR;	// Reset Mode Register
  Sf2M3_REG Reserved18[5];	//
  Sf2M3_REG SYSC_RTTC_RTMR;	// Real-time Mode Register
  Sf2M3_REG SYSC_RTTC_RTAR;	// Real-time Alarm Register
  Sf2M3_REG SYSC_RTTC_RTVR;	// Real-time Value Register
  Sf2M3_REG SYSC_RTTC_RTSR;	// Real-time Status Register
  Sf2M3_REG SYSC_PITC_PIMR;	// Period Interval Mode Register
  Sf2M3_REG SYSC_PITC_PISR;	// Period Interval Status Register
  Sf2M3_REG SYSC_PITC_PIVR;	// Period Interval Value Register
  Sf2M3_REG SYSC_PITC_PIIR;	// Period Interval Image Register
  Sf2M3_REG SYSC_WDTC_WDCR;	// Watchdog Control Register
  Sf2M3_REG SYSC_WDTC_WDMR;	// Watchdog Mode Register
  Sf2M3_REG SYSC_WDTC_WDSR;	// Watchdog Status Register
  Sf2M3_REG Reserved19[5];	//
  Sf2M3_REG SYSC_SYSC_VREG;	// Voltage Regulator Mode Register
} Sf2M3S_SYSC, *Sf2M3PS_SYSC;

// -------- VREG : (SYSC Offset: 0xd60) Voltage Regulator Mode Register --------
#  define Sf2M3_SYSC_PSTDBY     ((unsigned int) 0x1 <<  0)	// (SYSC) Voltage Regulator Power Mode

// *****************************************************************************
//              SOFTWARE API DEFINITION  FOR Advanced Interrupt Controller
// *****************************************************************************
typedef struct _Sf2M3S_AIC {
  Sf2M3_REG AIC_SMR[32];		// Source Mode Register
  Sf2M3_REG AIC_SVR[32];		// Source Vector Register
  Sf2M3_REG AIC_IVR;		// IRQ Vector Register
  Sf2M3_REG AIC_FVR;		// FIQ Vector Register
  Sf2M3_REG AIC_ISR;		// Interrupt Status Register
  Sf2M3_REG AIC_IPR;		// Interrupt Pending Register
  Sf2M3_REG AIC_IMR;		// Interrupt Mask Register
  Sf2M3_REG AIC_CISR;		// Core Interrupt Status Register
  Sf2M3_REG Reserved0[2];	//
  Sf2M3_REG AIC_IECR;		// Interrupt Enable Command Register
  Sf2M3_REG AIC_IDCR;		// Interrupt Disable Command Register
  Sf2M3_REG AIC_ICCR;		// Interrupt Clear Command Register
  Sf2M3_REG AIC_ISCR;		// Interrupt Set Command Register
  Sf2M3_REG AIC_EOICR;		// End of Interrupt Command Register
  Sf2M3_REG AIC_SPU;		// Spurious Vector Register
  Sf2M3_REG AIC_DCR;		// Debug Control Register (Protect)
  Sf2M3_REG Reserved1[1];	//
  Sf2M3_REG AIC_FFER;		// Fast Forcing Enable Register
  Sf2M3_REG AIC_FFDR;		// Fast Forcing Disable Register
  Sf2M3_REG AIC_FFSR;		// Fast Forcing Status Register
} Sf2M3S_AIC, *Sf2M3PS_AIC;

// -------- AIC_SMR : (AIC Offset: 0x0) Control Register --------
#  define Sf2M3_AIC_PRIOR       ((unsigned int) 0x7 <<  0)	// (AIC) Priority Level
#  define 	Sf2M3_AIC_PRIOR_LOWEST               ((unsigned int) 0x0)	// (AIC) Lowest priority level
#  define 	Sf2M3_AIC_PRIOR_HIGHEST              ((unsigned int) 0x7)	// (AIC) Highest priority level
#  define Sf2M3_AIC_SRCTYPE     ((unsigned int) 0x3 <<  5)	// (AIC) Interrupt Source Type
#  define 	Sf2M3_AIC_SRCTYPE_INT_LEVEL_SENSITIVE  ((unsigned int) 0x0 <<  5)	// (AIC) Internal Sources Code Label Level Sensitive
#  define 	Sf2M3_AIC_SRCTYPE_INT_EDGE_TRIGGERED   ((unsigned int) 0x1 <<  5)	// (AIC) Internal Sources Code Label Edge triggered
#  define 	Sf2M3_AIC_SRCTYPE_EXT_HIGH_LEVEL       ((unsigned int) 0x2 <<  5)	// (AIC) External Sources Code Label High-level Sensitive
#  define 	Sf2M3_AIC_SRCTYPE_EXT_POSITIVE_EDGE    ((unsigned int) 0x3 <<  5)	// (AIC) External Sources Code Label Positive Edge triggered
// -------- AIC_CISR : (AIC Offset: 0x114) AIC Core Interrupt Status Register --------
#  define Sf2M3_AIC_NFIQ        ((unsigned int) 0x1 <<  0)	// (AIC) NFIQ Status
#  define Sf2M3_AIC_NIRQ        ((unsigned int) 0x1 <<  1)	// (AIC) NIRQ Status
// -------- AIC_DCR : (AIC Offset: 0x138) AIC Debug Control Register (Protect) --------
#  define Sf2M3_AIC_DCR_PROT    ((unsigned int) 0x1 <<  0)	// (AIC) Protection Mode
#  define Sf2M3_AIC_DCR_GMSK    ((unsigned int) 0x1 <<  1)	// (AIC) General Mask

// *****************************************************************************
//              SOFTWARE API DEFINITION  FOR Debug Unit
// *****************************************************************************
typedef struct _Sf2M3S_DBGU {
  Sf2M3_REG DBGU_CR;		// Control Register
  Sf2M3_REG DBGU_MR;		// Mode Register
  Sf2M3_REG DBGU_IER;		// Interrupt Enable Register
  Sf2M3_REG DBGU_IDR;		// Interrupt Disable Register
  Sf2M3_REG DBGU_IMR;		// Interrupt Mask Register
  Sf2M3_REG DBGU_CSR;		// Channel Status Register
  Sf2M3_REG DBGU_RHR;		// Receiver Holding Register
  Sf2M3_REG DBGU_THR;		// Transmitter Holding Register
  Sf2M3_REG DBGU_BRGR;		// Baud Rate Generator Register
  Sf2M3_REG Reserved0[7];	//
  Sf2M3_REG DBGU_C1R;		// Chip ID1 Register
  Sf2M3_REG DBGU_C2R;		// Chip ID2 Register
  Sf2M3_REG DBGU_FNTR;		// Force NTRST Register
  Sf2M3_REG Reserved1[45];	//
  Sf2M3_REG DBGU_RPR;		// Receive Pointer Register
  Sf2M3_REG DBGU_RCR;		// Receive Counter Register
  Sf2M3_REG DBGU_TPR;		// Transmit Pointer Register
  Sf2M3_REG DBGU_TCR;		// Transmit Counter Register
  Sf2M3_REG DBGU_RNPR;		// Receive Next Pointer Register
  Sf2M3_REG DBGU_RNCR;		// Receive Next Counter Register
  Sf2M3_REG DBGU_TNPR;		// Transmit Next Pointer Register
  Sf2M3_REG DBGU_TNCR;		// Transmit Next Counter Register
  Sf2M3_REG DBGU_PTCR;		// PDC Transfer Control Register
  Sf2M3_REG DBGU_PTSR;		// PDC Transfer Status Register
} Sf2M3S_DBGU, *Sf2M3PS_DBGU;

// -------- DBGU_CR : (DBGU Offset: 0x0) Debug Unit Control Register --------
#  define Sf2M3_US_RSTRX        ((unsigned int) 0x1 <<  2)	// (DBGU) Reset Receiver
#  define Sf2M3_US_RSTTX        ((unsigned int) 0x1 <<  3)	// (DBGU) Reset Transmitter
#  define Sf2M3_US_RXEN         ((unsigned int) 0x1 <<  4)	// (DBGU) Receiver Enable
#  define Sf2M3_US_RXDIS        ((unsigned int) 0x1 <<  5)	// (DBGU) Receiver Disable
#  define Sf2M3_US_TXEN         ((unsigned int) 0x1 <<  6)	// (DBGU) Transmitter Enable
#  define Sf2M3_US_TXDIS        ((unsigned int) 0x1 <<  7)	// (DBGU) Transmitter Disable
// -------- DBGU_MR : (DBGU Offset: 0x4) Debug Unit Mode Register --------
#  define Sf2M3_US_PAR          ((unsigned int) 0x7 <<  9)	// (DBGU) Parity type
#  define 	Sf2M3_US_PAR_EVEN                 ((unsigned int) 0x0 <<  9)	// (DBGU) Even Parity
#  define 	Sf2M3_US_PAR_ODD                  ((unsigned int) 0x1 <<  9)	// (DBGU) Odd Parity
#  define 	Sf2M3_US_PAR_SPACE                ((unsigned int) 0x2 <<  9)	// (DBGU) Parity forced to 0 (Space)
#  define 	Sf2M3_US_PAR_MARK                 ((unsigned int) 0x3 <<  9)	// (DBGU) Parity forced to 1 (Mark)
#  define 	Sf2M3_US_PAR_NONE                 ((unsigned int) 0x4 <<  9)	// (DBGU) No Parity
#  define 	Sf2M3_US_PAR_MULTI_DROP           ((unsigned int) 0x6 <<  9)	// (DBGU) Multi-drop mode
#  define Sf2M3_US_CHMODE       ((unsigned int) 0x3 << 14)	// (DBGU) Channel Mode
#  define 	Sf2M3_US_CHMODE_NORMAL               ((unsigned int) 0x0 << 14)	// (DBGU) Normal Mode: The USART channel operates as an RX/TX USART.
#  define 	Sf2M3_US_CHMODE_AUTO                 ((unsigned int) 0x1 << 14)	// (DBGU) Automatic Echo: Receiver Data Input is connected to the TXD pin.
#  define 	Sf2M3_US_CHMODE_LOCAL                ((unsigned int) 0x2 << 14)	// (DBGU) Local Loopback: Transmitter Output Signal is connected to Receiver Input Signal.
#  define 	Sf2M3_US_CHMODE_REMOTE               ((unsigned int) 0x3 << 14)	// (DBGU) Remote Loopback: RXD pin is internally connected to TXD pin.
// -------- DBGU_IER : (DBGU Offset: 0x8) Debug Unit Interrupt Enable Register --------
#  define Sf2M3_US_RXRDY        ((unsigned int) 0x1 <<  0)	// (DBGU) RXRDY Interrupt
#  define Sf2M3_US_TXRDY        ((unsigned int) 0x1 <<  1)	// (DBGU) TXRDY Interrupt
#  define Sf2M3_US_ENDRX        ((unsigned int) 0x1 <<  3)	// (DBGU) End of Receive Transfer Interrupt
#  define Sf2M3_US_ENDTX        ((unsigned int) 0x1 <<  4)	// (DBGU) End of Transmit Interrupt
#  define Sf2M3_US_OVRE         ((unsigned int) 0x1 <<  5)	// (DBGU) Overrun Interrupt
#  define Sf2M3_US_FRAME        ((unsigned int) 0x1 <<  6)	// (DBGU) Framing Error Interrupt
#  define Sf2M3_US_PARE         ((unsigned int) 0x1 <<  7)	// (DBGU) Parity Error Interrupt
#  define Sf2M3_US_TXEMPTY      ((unsigned int) 0x1 <<  9)	// (DBGU) TXEMPTY Interrupt
#  define Sf2M3_US_TXBUFE       ((unsigned int) 0x1 << 11)	// (DBGU) TXBUFE Interrupt
#  define Sf2M3_US_RXBUFF       ((unsigned int) 0x1 << 12)	// (DBGU) RXBUFF Interrupt
#  define Sf2M3_US_COMM_TX      ((unsigned int) 0x1 << 30)	// (DBGU) COMM_TX Interrupt
#  define Sf2M3_US_COMM_RX      ((unsigned int) 0x1 << 31)	// (DBGU) COMM_RX Interrupt
// -------- DBGU_IDR : (DBGU Offset: 0xc) Debug Unit Interrupt Disable Register --------
// -------- DBGU_IMR : (DBGU Offset: 0x10) Debug Unit Interrupt Mask Register --------
// -------- DBGU_CSR : (DBGU Offset: 0x14) Debug Unit Channel Status Register --------
// -------- DBGU_FNTR : (DBGU Offset: 0x48) Debug Unit FORCE_NTRST Register --------
#  define Sf2M3_US_FORCE_NTRST  ((unsigned int) 0x1 <<  0)	// (DBGU) Force NTRST in JTAG

// *****************************************************************************
//              SOFTWARE API DEFINITION  FOR Peripheral Data Controller
// *****************************************************************************
typedef struct _Sf2M3S_PDC {
  Sf2M3_REG PDC_RPR;		// Receive Pointer Register
  Sf2M3_REG PDC_RCR;		// Receive Counter Register
  Sf2M3_REG PDC_TPR;		// Transmit Pointer Register
  Sf2M3_REG PDC_TCR;		// Transmit Counter Register
  Sf2M3_REG PDC_RNPR;		// Receive Next Pointer Register
  Sf2M3_REG PDC_RNCR;		// Receive Next Counter Register
  Sf2M3_REG PDC_TNPR;		// Transmit Next Pointer Register
  Sf2M3_REG PDC_TNCR;		// Transmit Next Counter Register
  Sf2M3_REG PDC_PTCR;		// PDC Transfer Control Register
  Sf2M3_REG PDC_PTSR;		// PDC Transfer Status Register
} Sf2M3S_PDC, *Sf2M3PS_PDC;

// -------- PDC_PTCR : (PDC Offset: 0x20) PDC Transfer Control Register --------
#  define Sf2M3_PDC_RXTEN       ((unsigned int) 0x1 <<  0)	// (PDC) Receiver Transfer Enable
#  define Sf2M3_PDC_RXTDIS      ((unsigned int) 0x1 <<  1)	// (PDC) Receiver Transfer Disable
#  define Sf2M3_PDC_TXTEN       ((unsigned int) 0x1 <<  8)	// (PDC) Transmitter Transfer Enable
#  define Sf2M3_PDC_TXTDIS      ((unsigned int) 0x1 <<  9)	// (PDC) Transmitter Transfer Disable
// -------- PDC_PTSR : (PDC Offset: 0x24) PDC Transfer Status Register --------

// *****************************************************************************
//              SOFTWARE API DEFINITION  FOR Parallel Input Output Controler
// *****************************************************************************
typedef struct _Sf2M3S_PIO {
  Sf2M3_REG PIO_PER;		// PIO Enable Register
  Sf2M3_REG PIO_PDR;		// PIO Disable Register
  Sf2M3_REG PIO_PSR;		// PIO Status Register
  Sf2M3_REG Reserved0[1];	//
  Sf2M3_REG PIO_OER;		// Output Enable Register
  Sf2M3_REG PIO_ODR;		// Output Disable Register
  Sf2M3_REG PIO_OSR;		// Output Status Register
  Sf2M3_REG Reserved1[1];	//
  Sf2M3_REG PIO_IFER;		// Input Filter Enable Register
  Sf2M3_REG PIO_IFDR;		// Input Filter Disable Register
  Sf2M3_REG PIO_IFSR;		// Input Filter Status Register
  Sf2M3_REG Reserved2[1];	//
  Sf2M3_REG PIO_SODR;		// Set Output Data Register
  Sf2M3_REG PIO_CODR;		// Clear Output Data Register
  Sf2M3_REG PIO_ODSR;		// Output Data Status Register
  Sf2M3_REG PIO_PDSR;		// Pin Data Status Register
  Sf2M3_REG PIO_IER;		// Interrupt Enable Register
  Sf2M3_REG PIO_IDR;		// Interrupt Disable Register
  Sf2M3_REG PIO_IMR;		// Interrupt Mask Register
  Sf2M3_REG PIO_ISR;		// Interrupt Status Register
  Sf2M3_REG PIO_MDER;		// Multi-driver Enable Register
  Sf2M3_REG PIO_MDDR;		// Multi-driver Disable Register
  Sf2M3_REG PIO_MDSR;		// Multi-driver Status Register
  Sf2M3_REG Reserved3[1];	//
  Sf2M3_REG PIO_PPUDR;		// Pull-up Disable Register
  Sf2M3_REG PIO_PPUER;		// Pull-up Enable Register
  Sf2M3_REG PIO_PPUSR;		// Pad Pull-up Status Register
  Sf2M3_REG Reserved4[1];	//
  Sf2M3_REG PIO_ASR;		// Select A Register
  Sf2M3_REG PIO_BSR;		// Select B Register
  Sf2M3_REG PIO_ABSR;		// AB Select Status Register
  Sf2M3_REG Reserved5[9];	//
  Sf2M3_REG PIO_OWER;		// Output Write Enable Register
  Sf2M3_REG PIO_OWDR;		// Output Write Disable Register
  Sf2M3_REG PIO_OWSR;		// Output Write Status Register
} Sf2M3S_PIO, *Sf2M3PS_PIO;


// *****************************************************************************
//              SOFTWARE API DEFINITION  FOR Clock Generator Controler
// *****************************************************************************
typedef struct _Sf2M3S_CKGR {
  Sf2M3_REG CKGR_MOR;		// Main Oscillator Register
  Sf2M3_REG CKGR_MCFR;		// Main Clock  Frequency Register
  Sf2M3_REG Reserved0[1];	//
  Sf2M3_REG CKGR_PLLR;		// PLL Register
} Sf2M3S_CKGR, *Sf2M3PS_CKGR;

// -------- CKGR_MOR : (CKGR Offset: 0x0) Main Oscillator Register --------
#  define Sf2M3_CKGR_MOSCEN     ((unsigned int) 0x1 <<  0)	// (CKGR) Main Oscillator Enable
#  define Sf2M3_CKGR_OSCBYPASS  ((unsigned int) 0x1 <<  1)	// (CKGR) Main Oscillator Bypass
#  define Sf2M3_CKGR_OSCOUNT    ((unsigned int) 0xFF <<  8)	// (CKGR) Main Oscillator Start-up Time
// -------- CKGR_MCFR : (CKGR Offset: 0x4) Main Clock Frequency Register --------
#  define Sf2M3_CKGR_MAINF      ((unsigned int) 0xFFFF <<  0)	// (CKGR) Main Clock Frequency
#  define Sf2M3_CKGR_MAINRDY    ((unsigned int) 0x1 << 16)	// (CKGR) Main Clock Ready
// -------- CKGR_PLLR : (CKGR Offset: 0xc) PLL B Register --------
#  define Sf2M3_CKGR_DIV        ((unsigned int) 0xFF <<  0)	// (CKGR) Divider Selected
#  define 	Sf2M3_CKGR_DIV_0                    ((unsigned int) 0x0)	// (CKGR) Divider output is 0
#  define 	Sf2M3_CKGR_DIV_BYPASS               ((unsigned int) 0x1)	// (CKGR) Divider is bypassed
#  define Sf2M3_CKGR_PLLCOUNT   ((unsigned int) 0x3F <<  8)	// (CKGR) PLL Counter
#  define Sf2M3_CKGR_OUT        ((unsigned int) 0x3 << 14)	// (CKGR) PLL Output Frequency Range
#  define 	Sf2M3_CKGR_OUT_0                    ((unsigned int) 0x0 << 14)	// (CKGR) Please refer to the PLL datasheet
#  define 	Sf2M3_CKGR_OUT_1                    ((unsigned int) 0x1 << 14)	// (CKGR) Please refer to the PLL datasheet
#  define 	Sf2M3_CKGR_OUT_2                    ((unsigned int) 0x2 << 14)	// (CKGR) Please refer to the PLL datasheet
#  define 	Sf2M3_CKGR_OUT_3                    ((unsigned int) 0x3 << 14)	// (CKGR) Please refer to the PLL datasheet
#  define Sf2M3_CKGR_MUL        ((unsigned int) 0x7FF << 16)	// (CKGR) PLL Multiplier
#  define Sf2M3_CKGR_USBDIV     ((unsigned int) 0x3 << 28)	// (CKGR) Divider for USB Clocks
#  define 	Sf2M3_CKGR_USBDIV_0                    ((unsigned int) 0x0 << 28)	// (CKGR) Divider output is PLL clock output
#  define 	Sf2M3_CKGR_USBDIV_1                    ((unsigned int) 0x1 << 28)	// (CKGR) Divider output is PLL clock output divided by 2
#  define 	Sf2M3_CKGR_USBDIV_2                    ((unsigned int) 0x2 << 28)	// (CKGR) Divider output is PLL clock output divided by 4

// *****************************************************************************
//              SOFTWARE API DEFINITION  FOR Power Management Controler
// *****************************************************************************
typedef struct _Sf2M3S_PMC {
  Sf2M3_REG PMC_SCER;		// System Clock Enable Register
  Sf2M3_REG PMC_SCDR;		// System Clock Disable Register
  Sf2M3_REG PMC_SCSR;		// System Clock Status Register
  Sf2M3_REG Reserved0[1];	//
  Sf2M3_REG PMC_PCER;		// Peripheral Clock Enable Register
  Sf2M3_REG PMC_PCDR;		// Peripheral Clock Disable Register
  Sf2M3_REG PMC_PCSR;		// Peripheral Clock Status Register
  Sf2M3_REG Reserved1[1];	//
  Sf2M3_REG PMC_MOR;		// Main Oscillator Register
  Sf2M3_REG PMC_MCFR;		// Main Clock  Frequency Register
  Sf2M3_REG Reserved2[1];	//
  Sf2M3_REG PMC_PLLR;		// PLL Register
  Sf2M3_REG PMC_MCKR;		// Master Clock Register
  Sf2M3_REG Reserved3[3];	//
  Sf2M3_REG PMC_PCKR[8];		// Programmable Clock Register
  Sf2M3_REG PMC_IER;		// Interrupt Enable Register
  Sf2M3_REG PMC_IDR;		// Interrupt Disable Register
  Sf2M3_REG PMC_SR;		// Status Register
  Sf2M3_REG PMC_IMR;		// Interrupt Mask Register
} Sf2M3S_PMC, *Sf2M3PS_PMC;

// -------- PMC_SCER : (PMC Offset: 0x0) System Clock Enable Register --------
#  define Sf2M3_PMC_PCK         ((unsigned int) 0x1 <<  0)	// (PMC) Processor Clock
#  define Sf2M3_PMC_UDP         ((unsigned int) 0x1 <<  7)	// (PMC) USB Device Port Clock
#  define Sf2M3_PMC_PCK0        ((unsigned int) 0x1 <<  8)	// (PMC) Programmable Clock Output
#  define Sf2M3_PMC_PCK1        ((unsigned int) 0x1 <<  9)	// (PMC) Programmable Clock Output
#  define Sf2M3_PMC_PCK2        ((unsigned int) 0x1 << 10)	// (PMC) Programmable Clock Output
#  define Sf2M3_PMC_PCK3        ((unsigned int) 0x1 << 11)	// (PMC) Programmable Clock Output
// -------- PMC_SCDR : (PMC Offset: 0x4) System Clock Disable Register --------
// -------- PMC_SCSR : (PMC Offset: 0x8) System Clock Status Register --------
// -------- CKGR_MOR : (PMC Offset: 0x20) Main Oscillator Register --------
// -------- CKGR_MCFR : (PMC Offset: 0x24) Main Clock Frequency Register --------
// -------- CKGR_PLLR : (PMC Offset: 0x2c) PLL B Register --------
// -------- PMC_MCKR : (PMC Offset: 0x30) Master Clock Register --------
#  define Sf2M3_PMC_CSS         ((unsigned int) 0x3 <<  0)	// (PMC) Programmable Clock Selection
#  define 	Sf2M3_PMC_CSS_SLOW_CLK             ((unsigned int) 0x0)	// (PMC) Slow Clock is selected
#  define 	Sf2M3_PMC_CSS_MAIN_CLK             ((unsigned int) 0x1)	// (PMC) Main Clock is selected
#  define 	Sf2M3_PMC_CSS_PLL_CLK              ((unsigned int) 0x3)	// (PMC) Clock from PLL is selected
#  define Sf2M3_PMC_PRES        ((unsigned int) 0x7 <<  2)	// (PMC) Programmable Clock Prescaler
#  define 	Sf2M3_PMC_PRES_CLK                  ((unsigned int) 0x0 <<  2)	// (PMC) Selected clock
#  define 	Sf2M3_PMC_PRES_CLK_2                ((unsigned int) 0x1 <<  2)	// (PMC) Selected clock divided by 2
#  define 	Sf2M3_PMC_PRES_CLK_4                ((unsigned int) 0x2 <<  2)	// (PMC) Selected clock divided by 4
#  define 	Sf2M3_PMC_PRES_CLK_8                ((unsigned int) 0x3 <<  2)	// (PMC) Selected clock divided by 8
#  define 	Sf2M3_PMC_PRES_CLK_16               ((unsigned int) 0x4 <<  2)	// (PMC) Selected clock divided by 16
#  define 	Sf2M3_PMC_PRES_CLK_32               ((unsigned int) 0x5 <<  2)	// (PMC) Selected clock divided by 32
#  define 	Sf2M3_PMC_PRES_CLK_64               ((unsigned int) 0x6 <<  2)	// (PMC) Selected clock divided by 64
// -------- PMC_PCKR : (PMC Offset: 0x40) Programmable Clock Register --------
// -------- PMC_IER : (PMC Offset: 0x60) PMC Interrupt Enable Register --------
#  define Sf2M3_PMC_MOSCS       ((unsigned int) 0x1 <<  0)	// (PMC) MOSC Status/Enable/Disable/Mask
#  define Sf2M3_PMC_LOCK        ((unsigned int) 0x1 <<  2)	// (PMC) PLL Status/Enable/Disable/Mask
#  define Sf2M3_PMC_MCKRDY      ((unsigned int) 0x1 <<  3)	// (PMC) MCK_RDY Status/Enable/Disable/Mask
#  define Sf2M3_PMC_PCK0RDY     ((unsigned int) 0x1 <<  8)	// (PMC) PCK0_RDY Status/Enable/Disable/Mask
#  define Sf2M3_PMC_PCK1RDY     ((unsigned int) 0x1 <<  9)	// (PMC) PCK1_RDY Status/Enable/Disable/Mask
#  define Sf2M3_PMC_PCK2RDY     ((unsigned int) 0x1 << 10)	// (PMC) PCK2_RDY Status/Enable/Disable/Mask
#  define Sf2M3_PMC_PCK3RDY     ((unsigned int) 0x1 << 11)	// (PMC) PCK3_RDY Status/Enable/Disable/Mask
// -------- PMC_IDR : (PMC Offset: 0x64) PMC Interrupt Disable Register --------
// -------- PMC_SR : (PMC Offset: 0x68) PMC Status Register --------
// -------- PMC_IMR : (PMC Offset: 0x6c) PMC Interrupt Mask Register --------

// *****************************************************************************
//              SOFTWARE API DEFINITION  FOR Reset Controller Interface
// *****************************************************************************
typedef struct _Sf2M3S_RSTC {
  Sf2M3_REG RSTC_RCR;		// Reset Control Register
  Sf2M3_REG RSTC_RSR;		// Reset Status Register
  Sf2M3_REG RSTC_RMR;		// Reset Mode Register
} Sf2M3S_RSTC, *Sf2M3PS_RSTC;

// -------- SYSC_RCR : (RSTC Offset: 0x0) Reset Control Register --------
#  define Sf2M3_SYSC_PROCRST    ((unsigned int) 0x1 <<  0)	// (RSTC) Processor Reset
#  define Sf2M3_SYSC_ICERST     ((unsigned int) 0x1 <<  1)	// (RSTC) ICE Interface Reset
#  define Sf2M3_SYSC_PERRST     ((unsigned int) 0x1 <<  2)	// (RSTC) Peripheral Reset
#  define Sf2M3_SYSC_EXTRST     ((unsigned int) 0x1 <<  3)	// (RSTC) External Reset
#  define Sf2M3_SYSC_KEY        ((unsigned int) 0xFF << 24)	// (RSTC) Password
// -------- SYSC_RSR : (RSTC Offset: 0x4) Reset Status Register --------
#  define Sf2M3_SYSC_URSTS      ((unsigned int) 0x1 <<  0)	// (RSTC) User Reset Status
#  define Sf2M3_SYSC_BODSTS     ((unsigned int) 0x1 <<  1)	// (RSTC) Brown-out Detection Status
#  define Sf2M3_SYSC_RSTTYP     ((unsigned int) 0x7 <<  8)	// (RSTC) Reset Type
#  define 	Sf2M3_SYSC_RSTTYP_POWERUP              ((unsigned int) 0x0 <<  8)	// (RSTC) Power-up Reset. VDDCORE rising.
#  define 	Sf2M3_SYSC_RSTTYP_WATCHDOG             ((unsigned int) 0x2 <<  8)	// (RSTC) Watchdog Reset. Watchdog overflow occured.
#  define 	Sf2M3_SYSC_RSTTYP_SOFTWARE             ((unsigned int) 0x3 <<  8)	// (RSTC) Software Reset. Processor reset required by the software.
#  define 	Sf2M3_SYSC_RSTTYP_USER                 ((unsigned int) 0x4 <<  8)	// (RSTC) User Reset. NRST pin detected low.
#  define 	Sf2M3_SYSC_RSTTYP_BROWNOUT             ((unsigned int) 0x5 <<  8)	// (RSTC) Brown-out Reset.
#  define Sf2M3_SYSC_NRSTL      ((unsigned int) 0x1 << 16)	// (RSTC) NRST pin level
#  define Sf2M3_SYSC_SRCMP      ((unsigned int) 0x1 << 17)	// (RSTC) Software Reset Command in Progress.
// -------- SYSC_RMR : (RSTC Offset: 0x8) Reset Mode Register --------
#  define Sf2M3_SYSC_URSTEN     ((unsigned int) 0x1 <<  0)	// (RSTC) User Reset Enable
#  define Sf2M3_SYSC_URSTIEN    ((unsigned int) 0x1 <<  4)	// (RSTC) User Reset Interrupt Enable
#  define Sf2M3_SYSC_ERSTL      ((unsigned int) 0xF <<  8)	// (RSTC) User Reset Enable
#  define Sf2M3_SYSC_BODIEN     ((unsigned int) 0x1 << 16)	// (RSTC) Brown-out Detection Interrupt Enable

// *****************************************************************************
//              SOFTWARE API DEFINITION  FOR Real Time Timer Controller Interface
// *****************************************************************************
typedef struct _Sf2M3S_RTTC {
  Sf2M3_REG RTTC_RTMR;		// Real-time Mode Register
  Sf2M3_REG RTTC_RTAR;		// Real-time Alarm Register
  Sf2M3_REG RTTC_RTVR;		// Real-time Value Register
  Sf2M3_REG RTTC_RTSR;		// Real-time Status Register
} Sf2M3S_RTTC, *Sf2M3PS_RTTC;

// -------- SYSC_RTMR : (RTTC Offset: 0x0) Real-time Mode Register --------
#  define Sf2M3_SYSC_RTPRES     ((unsigned int) 0xFFFF <<  0)	// (RTTC) Real-time Timer Prescaler Value
#  define Sf2M3_SYSC_ALMIEN     ((unsigned int) 0x1 << 16)	// (RTTC) Alarm Interrupt Enable
#  define Sf2M3_SYSC_RTTINCIEN  ((unsigned int) 0x1 << 17)	// (RTTC) Real Time Timer Increment Interrupt Enable
#  define Sf2M3_SYSC_RTTRST     ((unsigned int) 0x1 << 18)	// (RTTC) Real Time Timer Restart
// -------- SYSC_RTAR : (RTTC Offset: 0x4) Real-time Alarm Register --------
#  define Sf2M3_SYSC_ALMV       ((unsigned int) 0x0 <<  0)	// (RTTC) Alarm Value
// -------- SYSC_RTVR : (RTTC Offset: 0x8) Current Real-time Value Register --------
#  define Sf2M3_SYSC_CRTV       ((unsigned int) 0x0 <<  0)	// (RTTC) Current Real-time Value
// -------- SYSC_RTSR : (RTTC Offset: 0xc) Real-time Status Register --------
#  define Sf2M3_SYSC_ALMS       ((unsigned int) 0x1 <<  0)	// (RTTC) Real-time Alarm Status
#  define Sf2M3_SYSC_RTTINC     ((unsigned int) 0x1 <<  1)	// (RTTC) Real-time Timer Increment

// *****************************************************************************
//              SOFTWARE API DEFINITION  FOR Periodic Interval Timer Controller Interface
// *****************************************************************************
typedef struct _Sf2M3S_PITC {
  Sf2M3_REG PITC_PIMR;		// Period Interval Mode Register
  Sf2M3_REG PITC_PISR;		// Period Interval Status Register
  Sf2M3_REG PITC_PIVR;		// Period Interval Value Register
  Sf2M3_REG PITC_PIIR;		// Period Interval Image Register
} Sf2M3S_PITC, *Sf2M3PS_PITC;

// -------- SYSC_PIMR : (PITC Offset: 0x0) Periodic Interval Mode Register --------
#  define Sf2M3_SYSC_PIV        ((unsigned int) 0xFFFFF <<  0)	// (PITC) Periodic Interval Value
#  define Sf2M3_SYSC_PITEN      ((unsigned int) 0x1 << 24)	// (PITC) Periodic Interval Timer Enabled
#  define Sf2M3_SYSC_PITIEN     ((unsigned int) 0x1 << 25)	// (PITC) Periodic Interval Timer Interrupt Enable
// -------- SYSC_PISR : (PITC Offset: 0x4) Periodic Interval Status Register --------
#  define Sf2M3_SYSC_PITS       ((unsigned int) 0x1 <<  0)	// (PITC) Periodic Interval Timer Status
// -------- SYSC_PIVR : (PITC Offset: 0x8) Periodic Interval Value Register --------
#  define Sf2M3_SYSC_CPIV       ((unsigned int) 0xFFFFF <<  0)	// (PITC) Current Periodic Interval Value
#  define Sf2M3_SYSC_PICNT      ((unsigned int) 0xFFF << 20)	// (PITC) Periodic Interval Counter
// -------- SYSC_PIIR : (PITC Offset: 0xc) Periodic Interval Image Register --------

// *****************************************************************************
//              SOFTWARE API DEFINITION  FOR Watchdog Timer Controller Interface
// *****************************************************************************
typedef struct _Sf2M3S_WDTC {
  Sf2M3_REG WDTC_WDCR;		// Watchdog Control Register
  Sf2M3_REG WDTC_WDMR;		// Watchdog Mode Register
  Sf2M3_REG WDTC_WDSR;		// Watchdog Status Register
} Sf2M3S_WDTC, *Sf2M3PS_WDTC;

// -------- SYSC_WDCR : (WDTC Offset: 0x0) Periodic Interval Image Register --------
#  define Sf2M3_SYSC_WDRSTT     ((unsigned int) 0x1 <<  0)	// (WDTC) Watchdog Restart
// -------- SYSC_WDMR : (WDTC Offset: 0x4) Watchdog Mode Register --------
#  define Sf2M3_SYSC_WDV        ((unsigned int) 0xFFF <<  0)	// (WDTC) Watchdog Timer Restart
#  define Sf2M3_SYSC_WDFIEN     ((unsigned int) 0x1 << 12)	// (WDTC) Watchdog Fault Interrupt Enable
#  define Sf2M3_SYSC_WDRSTEN    ((unsigned int) 0x1 << 13)	// (WDTC) Watchdog Reset Enable
#  define Sf2M3_SYSC_WDRPROC    ((unsigned int) 0x1 << 14)	// (WDTC) Watchdog Timer Restart
#  define Sf2M3_SYSC_WDDIS      ((unsigned int) 0x1 << 15)	// (WDTC) Watchdog Disable
#  define Sf2M3_SYSC_WDD        ((unsigned int) 0xFFF << 16)	// (WDTC) Watchdog Delta Value
#  define Sf2M3_SYSC_WDDBGHLT   ((unsigned int) 0x1 << 28)	// (WDTC) Watchdog Debug Halt
#  define Sf2M3_SYSC_WDIDLEHLT  ((unsigned int) 0x1 << 29)	// (WDTC) Watchdog Idle Halt
// -------- SYSC_WDSR : (WDTC Offset: 0x8) Watchdog Status Register --------
#  define Sf2M3_SYSC_WDUNF      ((unsigned int) 0x1 <<  0)	// (WDTC) Watchdog Underflow
#  define Sf2M3_SYSC_WDERR      ((unsigned int) 0x1 <<  1)	// (WDTC) Watchdog Error

// *****************************************************************************
//              SOFTWARE API DEFINITION  FOR Memory Controller Interface
// *****************************************************************************
typedef struct _Sf2M3S_MC {
  Sf2M3_REG MC_RCR;		// MC Remap Control Register
  Sf2M3_REG MC_ASR;		// MC Abort Status Register
  Sf2M3_REG MC_AASR;		// MC Abort Address Status Register
  Sf2M3_REG Reserved0[21];	//
  Sf2M3_REG MC_FMR;		// MC Flash Mode Register
  Sf2M3_REG MC_FCR;		// MC Flash Command Register
  Sf2M3_REG MC_FSR;		// MC Flash Status Register
} Sf2M3S_MC, *Sf2M3PS_MC;

// -------- MC_RCR : (MC Offset: 0x0) MC Remap Control Register --------
#  define Sf2M3_MC_RCB          ((unsigned int) 0x1 <<  0)	// (MC) Remap Command Bit
// -------- MC_ASR : (MC Offset: 0x4) MC Abort Status Register --------
#  define Sf2M3_MC_UNDADD       ((unsigned int) 0x1 <<  0)	// (MC) Undefined Addess Abort Status
#  define Sf2M3_MC_MISADD       ((unsigned int) 0x1 <<  1)	// (MC) Misaligned Addess Abort Status
#  define Sf2M3_MC_ABTSZ        ((unsigned int) 0x3 <<  8)	// (MC) Abort Size Status
#  define 	Sf2M3_MC_ABTSZ_BYTE                 ((unsigned int) 0x0 <<  8)	// (MC) Byte
#  define 	Sf2M3_MC_ABTSZ_HWORD                ((unsigned int) 0x1 <<  8)	// (MC) Half-word
#  define 	Sf2M3_MC_ABTSZ_WORD                 ((unsigned int) 0x2 <<  8)	// (MC) Word
#  define Sf2M3_MC_ABTTYP       ((unsigned int) 0x3 << 10)	// (MC) Abort Type Status
#  define 	Sf2M3_MC_ABTTYP_DATAR                ((unsigned int) 0x0 << 10)	// (MC) Data Read
#  define 	Sf2M3_MC_ABTTYP_DATAW                ((unsigned int) 0x1 << 10)	// (MC) Data Write
#  define 	Sf2M3_MC_ABTTYP_FETCH                ((unsigned int) 0x2 << 10)	// (MC) Code Fetch
#  define Sf2M3_MC_MST0         ((unsigned int) 0x1 << 16)	// (MC) Master 0 Abort Source
#  define Sf2M3_MC_MST1         ((unsigned int) 0x1 << 17)	// (MC) Master 1 Abort Source
#  define Sf2M3_MC_SVMST0       ((unsigned int) 0x1 << 24)	// (MC) Saved Master 0 Abort Source
#  define Sf2M3_MC_SVMST1       ((unsigned int) 0x1 << 25)	// (MC) Saved Master 1 Abort Source
// -------- MC_FMR : (MC Offset: 0x60) MC Flash Mode Register --------
#  define Sf2M3_MC_FRDY         ((unsigned int) 0x1 <<  0)	// (MC) Flash Ready
#  define Sf2M3_MC_LOCKE        ((unsigned int) 0x1 <<  2)	// (MC) Lock Error
#  define Sf2M3_MC_PROGE        ((unsigned int) 0x1 <<  3)	// (MC) Programming Error
#  define Sf2M3_MC_NEBP         ((unsigned int) 0x1 <<  7)	// (MC) No Erase Before Programming
#  define Sf2M3_MC_FWS          ((unsigned int) 0x3 <<  8)	// (MC) Flash Wait State
#  define 	Sf2M3_MC_FWS_0FWS                 ((unsigned int) 0x0 <<  8)	// (MC) 1 cycle for Read, 2 for Write operations
#  define 	Sf2M3_MC_FWS_1FWS                 ((unsigned int) 0x1 <<  8)	// (MC) 2 cycles for Read, 3 for Write operations
#  define 	Sf2M3_MC_FWS_2FWS                 ((unsigned int) 0x2 <<  8)	// (MC) 3 cycles for Read, 4 for Write operations
#  define 	Sf2M3_MC_FWS_3FWS                 ((unsigned int) 0x3 <<  8)	// (MC) 4 cycles for Read, 4 for Write operations
#  define Sf2M3_MC_FMCN         ((unsigned int) 0xFF << 16)	// (MC) Flash Microsecond Cycle Number
// -------- MC_FCR : (MC Offset: 0x64) MC Flash Command Register --------
#  define Sf2M3_MC_FCMD         ((unsigned int) 0xF <<  0)	// (MC) Flash Command
#  define 	Sf2M3_MC_FCMD_START_PROG           ((unsigned int) 0x1)	// (MC) Starts the programming of th epage specified by PAGEN.
#  define 	Sf2M3_MC_FCMD_LOCK                 ((unsigned int) 0x2)	// (MC) Starts a lock sequence of the sector defined by the bits 4 to 7 of the field PAGEN.
#  define 	Sf2M3_MC_FCMD_PROG_AND_LOCK        ((unsigned int) 0x3)	// (MC) The lock sequence automatically happens after the programming sequence is completed.
#  define 	Sf2M3_MC_FCMD_UNLOCK               ((unsigned int) 0x4)	// (MC) Starts an unlock sequence of the sector defined by the bits 4 to 7 of the field PAGEN.
#  define 	Sf2M3_MC_FCMD_ERASE_ALL            ((unsigned int) 0x8)	// (MC) Starts the erase of the entire flash.If at least a page is locked, the command is cancelled.
#  define 	Sf2M3_MC_FCMD_SET_GP_NVM           ((unsigned int) 0xB)	// (MC) Set General Purpose NVM bits.
#  define 	Sf2M3_MC_FCMD_CLR_GP_NVM           ((unsigned int) 0xD)	// (MC) Clear General Purpose NVM bits.
#  define 	Sf2M3_MC_FCMD_SET_SECURITY         ((unsigned int) 0xF)	// (MC) Set Security Bit.
#  define Sf2M3_MC_PAGEN        ((unsigned int) 0x3FF <<  8)	// (MC) Page Number
#  define Sf2M3_MC_KEY          ((unsigned int) 0xFF << 24)	// (MC) Writing Protect Key
// -------- MC_FSR : (MC Offset: 0x68) MC Flash Command Register --------
#  define Sf2M3_MC_SECURITY     ((unsigned int) 0x1 <<  4)	// (MC) Security Bit Status
#  define Sf2M3_MC_GPNVM0       ((unsigned int) 0x1 <<  8)	// (MC) Sector 0 Lock Status
#  define Sf2M3_MC_GPNVM1       ((unsigned int) 0x1 <<  9)	// (MC) Sector 1 Lock Status
#  define Sf2M3_MC_GPNVM2       ((unsigned int) 0x1 << 10)	// (MC) Sector 2 Lock Status
#  define Sf2M3_MC_GPNVM3       ((unsigned int) 0x1 << 11)	// (MC) Sector 3 Lock Status
#  define Sf2M3_MC_GPNVM4       ((unsigned int) 0x1 << 12)	// (MC) Sector 4 Lock Status
#  define Sf2M3_MC_GPNVM5       ((unsigned int) 0x1 << 13)	// (MC) Sector 5 Lock Status
#  define Sf2M3_MC_GPNVM6       ((unsigned int) 0x1 << 14)	// (MC) Sector 6 Lock Status
#  define Sf2M3_MC_GPNVM7       ((unsigned int) 0x1 << 15)	// (MC) Sector 7 Lock Status
#  define Sf2M3_MC_LOCKS0       ((unsigned int) 0x1 << 16)	// (MC) Sector 0 Lock Status
#  define Sf2M3_MC_LOCKS1       ((unsigned int) 0x1 << 17)	// (MC) Sector 1 Lock Status
#  define Sf2M3_MC_LOCKS2       ((unsigned int) 0x1 << 18)	// (MC) Sector 2 Lock Status
#  define Sf2M3_MC_LOCKS3       ((unsigned int) 0x1 << 19)	// (MC) Sector 3 Lock Status
#  define Sf2M3_MC_LOCKS4       ((unsigned int) 0x1 << 20)	// (MC) Sector 4 Lock Status
#  define Sf2M3_MC_LOCKS5       ((unsigned int) 0x1 << 21)	// (MC) Sector 5 Lock Status
#  define Sf2M3_MC_LOCKS6       ((unsigned int) 0x1 << 22)	// (MC) Sector 6 Lock Status
#  define Sf2M3_MC_LOCKS7       ((unsigned int) 0x1 << 23)	// (MC) Sector 7 Lock Status
#  define Sf2M3_MC_LOCKS8       ((unsigned int) 0x1 << 24)	// (MC) Sector 8 Lock Status
#  define Sf2M3_MC_LOCKS9       ((unsigned int) 0x1 << 25)	// (MC) Sector 9 Lock Status
#  define Sf2M3_MC_LOCKS10      ((unsigned int) 0x1 << 26)	// (MC) Sector 10 Lock Status
#  define Sf2M3_MC_LOCKS11      ((unsigned int) 0x1 << 27)	// (MC) Sector 11 Lock Status
#  define Sf2M3_MC_LOCKS12      ((unsigned int) 0x1 << 28)	// (MC) Sector 12 Lock Status
#  define Sf2M3_MC_LOCKS13      ((unsigned int) 0x1 << 29)	// (MC) Sector 13 Lock Status
#  define Sf2M3_MC_LOCKS14      ((unsigned int) 0x1 << 30)	// (MC) Sector 14 Lock Status
#  define Sf2M3_MC_LOCKS15      ((unsigned int) 0x1 << 31)	// (MC) Sector 15 Lock Status

// *****************************************************************************
//              SOFTWARE API DEFINITION  FOR Serial Parallel Interface
// *****************************************************************************
typedef struct _Sf2M3S_SPI {
  Sf2M3_REG SPI_CR;		// Control Register
  Sf2M3_REG SPI_MR;		// Mode Register
  Sf2M3_REG SPI_RDR;		// Receive Data Register
  Sf2M3_REG SPI_TDR;		// Transmit Data Register
  Sf2M3_REG SPI_SR;		// Status Register
  Sf2M3_REG SPI_IER;		// Interrupt Enable Register
  Sf2M3_REG SPI_IDR;		// Interrupt Disable Register
  Sf2M3_REG SPI_IMR;		// Interrupt Mask Register
  Sf2M3_REG Reserved0[4];	//
  Sf2M3_REG SPI_CSR[4];		// Chip Select Register
  Sf2M3_REG Reserved1[48];	//
  Sf2M3_REG SPI_RPR;		// Receive Pointer Register
  Sf2M3_REG SPI_RCR;		// Receive Counter Register
  Sf2M3_REG SPI_TPR;		// Transmit Pointer Register
  Sf2M3_REG SPI_TCR;		// Transmit Counter Register
  Sf2M3_REG SPI_RNPR;		// Receive Next Pointer Register
  Sf2M3_REG SPI_RNCR;		// Receive Next Counter Register
  Sf2M3_REG SPI_TNPR;		// Transmit Next Pointer Register
  Sf2M3_REG SPI_TNCR;		// Transmit Next Counter Register
  Sf2M3_REG SPI_PTCR;		// PDC Transfer Control Register
  Sf2M3_REG SPI_PTSR;		// PDC Transfer Status Register
} Sf2M3S_SPI, *Sf2M3PS_SPI;

// -------- SPI_CR : (SPI Offset: 0x0) SPI Control Register --------
#  define Sf2M3_SPI_SPIEN       ((unsigned int) 0x1 <<  0)	// (SPI) SPI Enable
#  define Sf2M3_SPI_SPIDIS      ((unsigned int) 0x1 <<  1)	// (SPI) SPI Disable
#  define Sf2M3_SPI_SWRST       ((unsigned int) 0x1 <<  7)	// (SPI) SPI Software reset
#  define Sf2M3_SPI_LASTXFER    ((unsigned int) 0x1 << 24)	// (SPI) SPI Last Transfer
// -------- SPI_MR : (SPI Offset: 0x4) SPI Mode Register --------
#  define Sf2M3_SPI_MSTR        ((unsigned int) 0x1 <<  0)	// (SPI) Master/Slave Mode
#  define Sf2M3_SPI_PS          ((unsigned int) 0x1 <<  1)	// (SPI) Peripheral Select
#  define 	Sf2M3_SPI_PS_FIXED                ((unsigned int) 0x0 <<  1)	// (SPI) Fixed Peripheral Select
#  define 	Sf2M3_SPI_PS_VARIABLE             ((unsigned int) 0x1 <<  1)	// (SPI) Variable Peripheral Select
#  define Sf2M3_SPI_PCSDEC      ((unsigned int) 0x1 <<  2)	// (SPI) Chip Select Decode
#  define Sf2M3_SPI_FDIV        ((unsigned int) 0x1 <<  3)	// (SPI) Clock Selection
#  define Sf2M3_SPI_MODFDIS     ((unsigned int) 0x1 <<  4)	// (SPI) Mode Fault Detection
#  define Sf2M3_SPI_LLB         ((unsigned int) 0x1 <<  7)	// (SPI) Clock Selection
#  define Sf2M3_SPI_PCS         ((unsigned int) 0xF << 16)	// (SPI) Peripheral Chip Select
#  define Sf2M3_SPI_DLYBCS      ((unsigned int) 0xFF << 24)	// (SPI) Delay Between Chip Selects
// -------- SPI_RDR : (SPI Offset: 0x8) Receive Data Register --------
#  define Sf2M3_SPI_RD          ((unsigned int) 0xFFFF <<  0)	// (SPI) Receive Data
#  define Sf2M3_SPI_RPCS        ((unsigned int) 0xF << 16)	// (SPI) Peripheral Chip Select Status
// -------- SPI_TDR : (SPI Offset: 0xc) Transmit Data Register --------
#  define Sf2M3_SPI_TD          ((unsigned int) 0xFFFF <<  0)	// (SPI) Transmit Data
#  define Sf2M3_SPI_TPCS        ((unsigned int) 0xF << 16)	// (SPI) Peripheral Chip Select Status
// -------- SPI_SR : (SPI Offset: 0x10) Status Register --------
#  define Sf2M3_SPI_RDRF        ((unsigned int) 0x1 <<  0)	// (SPI) Receive Data Register Full
#  define Sf2M3_SPI_TDRE        ((unsigned int) 0x1 <<  1)	// (SPI) Transmit Data Register Empty
#  define Sf2M3_SPI_MODF        ((unsigned int) 0x1 <<  2)	// (SPI) Mode Fault Error
#  define Sf2M3_SPI_OVRES       ((unsigned int) 0x1 <<  3)	// (SPI) Overrun Error Status
#  define Sf2M3_SPI_ENDRX       ((unsigned int) 0x1 <<  4)	// (SPI) End of Receiver Transfer
#  define Sf2M3_SPI_ENDTX       ((unsigned int) 0x1 <<  5)	// (SPI) End of Receiver Transfer
#  define Sf2M3_SPI_RXBUFF      ((unsigned int) 0x1 <<  6)	// (SPI) RXBUFF Interrupt
#  define Sf2M3_SPI_TXBUFE      ((unsigned int) 0x1 <<  7)	// (SPI) TXBUFE Interrupt
#  define Sf2M3_SPI_NSSR        ((unsigned int) 0x1 <<  8)	// (SPI) NSSR Interrupt
#  define Sf2M3_SPI_TXEMPTY     ((unsigned int) 0x1 <<  9)	// (SPI) TXEMPTY Interrupt
#  define Sf2M3_SPI_SPIENS      ((unsigned int) 0x1 << 16)	// (SPI) Enable Status
// -------- SPI_IER : (SPI Offset: 0x14) Interrupt Enable Register --------
// -------- SPI_IDR : (SPI Offset: 0x18) Interrupt Disable Register --------
// -------- SPI_IMR : (SPI Offset: 0x1c) Interrupt Mask Register --------
// -------- SPI_CSR : (SPI Offset: 0x30) Chip Select Register --------
#  define Sf2M3_SPI_CPOL        ((unsigned int) 0x1 <<  0)	// (SPI) Clock Polarity
#  define Sf2M3_SPI_NCPHA       ((unsigned int) 0x1 <<  1)	// (SPI) Clock Phase
#  define Sf2M3_SPI_CSAAT       ((unsigned int) 0x1 <<  2)	// (SPI) Chip Select Active After Transfer
#  define Sf2M3_SPI_BITS        ((unsigned int) 0xF <<  4)	// (SPI) Bits Per Transfer
#  define 	Sf2M3_SPI_BITS_8                    ((unsigned int) 0x0 <<  4)	// (SPI) 8 Bits Per transfer
#  define 	Sf2M3_SPI_BITS_9                    ((unsigned int) 0x1 <<  4)	// (SPI) 9 Bits Per transfer
#  define 	Sf2M3_SPI_BITS_10                   ((unsigned int) 0x2 <<  4)	// (SPI) 10 Bits Per transfer
#  define 	Sf2M3_SPI_BITS_11                   ((unsigned int) 0x3 <<  4)	// (SPI) 11 Bits Per transfer
#  define 	Sf2M3_SPI_BITS_12                   ((unsigned int) 0x4 <<  4)	// (SPI) 12 Bits Per transfer
#  define 	Sf2M3_SPI_BITS_13                   ((unsigned int) 0x5 <<  4)	// (SPI) 13 Bits Per transfer
#  define 	Sf2M3_SPI_BITS_14                   ((unsigned int) 0x6 <<  4)	// (SPI) 14 Bits Per transfer
#  define 	Sf2M3_SPI_BITS_15                   ((unsigned int) 0x7 <<  4)	// (SPI) 15 Bits Per transfer
#  define 	Sf2M3_SPI_BITS_16                   ((unsigned int) 0x8 <<  4)	// (SPI) 16 Bits Per transfer
#  define Sf2M3_SPI_SCBR        ((unsigned int) 0xFF <<  8)	// (SPI) Serial Clock Baud Rate
#  define Sf2M3_SPI_DLYBS       ((unsigned int) 0xFF << 16)	// (SPI) Serial Clock Baud Rate
#  define Sf2M3_SPI_DLYBCT      ((unsigned int) 0xFF << 24)	// (SPI) Delay Between Consecutive Transfers

// *****************************************************************************
//              SOFTWARE API DEFINITION  FOR Analog to Digital Convertor
// *****************************************************************************
typedef struct _Sf2M3S_ADC {
  Sf2M3_REG ADC_CR;		// ADC Control Register
  Sf2M3_REG ADC_MR;		// ADC Mode Register
  Sf2M3_REG Reserved0[2];	//
  Sf2M3_REG ADC_CHER;		// ADC Channel Enable Register
  Sf2M3_REG ADC_CHDR;		// ADC Channel Disable Register
  Sf2M3_REG ADC_CHSR;		// ADC Channel Status Register
  Sf2M3_REG ADC_SR;		// ADC Status Register
  Sf2M3_REG ADC_LCDR;		// ADC Last Converted Data Register
  Sf2M3_REG ADC_IER;		// ADC Interrupt Enable Register
  Sf2M3_REG ADC_IDR;		// ADC Interrupt Disable Register
  Sf2M3_REG ADC_IMR;		// ADC Interrupt Mask Register
  Sf2M3_REG ADC_CDR0;		// ADC Channel Data Register 0
  Sf2M3_REG ADC_CDR1;		// ADC Channel Data Register 1
  Sf2M3_REG ADC_CDR2;		// ADC Channel Data Register 2
  Sf2M3_REG ADC_CDR3;		// ADC Channel Data Register 3
  Sf2M3_REG ADC_CDR4;		// ADC Channel Data Register 4
  Sf2M3_REG ADC_CDR5;		// ADC Channel Data Register 5
  Sf2M3_REG ADC_CDR6;		// ADC Channel Data Register 6
  Sf2M3_REG ADC_CDR7;		// ADC Channel Data Register 7
  Sf2M3_REG Reserved1[44];	//
  Sf2M3_REG ADC_RPR;		// Receive Pointer Register
  Sf2M3_REG ADC_RCR;		// Receive Counter Register
  Sf2M3_REG ADC_TPR;		// Transmit Pointer Register
  Sf2M3_REG ADC_TCR;		// Transmit Counter Register
  Sf2M3_REG ADC_RNPR;		// Receive Next Pointer Register
  Sf2M3_REG ADC_RNCR;		// Receive Next Counter Register
  Sf2M3_REG ADC_TNPR;		// Transmit Next Pointer Register
  Sf2M3_REG ADC_TNCR;		// Transmit Next Counter Register
  Sf2M3_REG ADC_PTCR;		// PDC Transfer Control Register
  Sf2M3_REG ADC_PTSR;		// PDC Transfer Status Register
} Sf2M3S_ADC, *Sf2M3PS_ADC;

// -------- ADC_CR : (ADC Offset: 0x0) ADC Control Register --------
#  define Sf2M3_ADC_SWRST       ((unsigned int) 0x1 <<  0)	// (ADC) Software Reset
#  define Sf2M3_ADC_START       ((unsigned int) 0x1 <<  1)	// (ADC) Start Conversion
// -------- ADC_MR : (ADC Offset: 0x4) ADC Mode Register --------
#  define Sf2M3_ADC_TRGEN       ((unsigned int) 0x1 <<  0)	// (ADC) Trigger Enable
#  define 	Sf2M3_ADC_TRGEN_DIS                  ((unsigned int) 0x0)	// (ADC) Hradware triggers are disabled. Starting a conversion is only possible by software
#  define 	Sf2M3_ADC_TRGEN_EN                   ((unsigned int) 0x1)	// (ADC) Hardware trigger selected by TRGSEL field is enabled.
#  define Sf2M3_ADC_TRGSEL      ((unsigned int) 0x7 <<  1)	// (ADC) Trigger Selection
#  define 	Sf2M3_ADC_TRGSEL_TIOA0                ((unsigned int) 0x0 <<  1)	// (ADC) Selected TRGSEL = TIAO0
#  define 	Sf2M3_ADC_TRGSEL_TIOA1                ((unsigned int) 0x1 <<  1)	// (ADC) Selected TRGSEL = TIAO1
#  define 	Sf2M3_ADC_TRGSEL_TIOA2                ((unsigned int) 0x2 <<  1)	// (ADC) Selected TRGSEL = TIAO2
#  define 	Sf2M3_ADC_TRGSEL_TIOA3                ((unsigned int) 0x3 <<  1)	// (ADC) Selected TRGSEL = TIAO3
#  define 	Sf2M3_ADC_TRGSEL_TIOA4                ((unsigned int) 0x4 <<  1)	// (ADC) Selected TRGSEL = TIAO4
#  define 	Sf2M3_ADC_TRGSEL_TIOA5                ((unsigned int) 0x5 <<  1)	// (ADC) Selected TRGSEL = TIAO5
#  define 	Sf2M3_ADC_TRGSEL_EXT                  ((unsigned int) 0x6 <<  1)	// (ADC) Selected TRGSEL = External Trigger
#  define Sf2M3_ADC_LOWRES      ((unsigned int) 0x1 <<  4)	// (ADC) Resolution.
#  define 	Sf2M3_ADC_LOWRES_10_BIT               ((unsigned int) 0x0 <<  4)	// (ADC) 10-bit resolution
#  define 	Sf2M3_ADC_LOWRES_8_BIT                ((unsigned int) 0x1 <<  4)	// (ADC) 8-bit resolution
#  define Sf2M3_ADC_SLEEP       ((unsigned int) 0x1 <<  5)	// (ADC) Sleep Mode
#  define 	Sf2M3_ADC_SLEEP_NORMAL_MODE          ((unsigned int) 0x0 <<  5)	// (ADC) Normal Mode
#  define 	Sf2M3_ADC_SLEEP_MODE                 ((unsigned int) 0x1 <<  5)	// (ADC) Sleep Mode
#  define Sf2M3_ADC_PRESCAL     ((unsigned int) 0x3F <<  8)	// (ADC) Prescaler rate selection
#  define Sf2M3_ADC_STARTUP     ((unsigned int) 0x1F << 16)	// (ADC) Startup Time
#  define Sf2M3_ADC_SHTIM       ((unsigned int) 0xF << 24)	// (ADC) Sample & Hold Time
// --------     ADC_CHER : (ADC Offset: 0x10) ADC Channel Enable Register --------
#  define Sf2M3_ADC_CH0         ((unsigned int) 0x1 <<  0)	// (ADC) Channel 0
#  define Sf2M3_ADC_CH1         ((unsigned int) 0x1 <<  1)	// (ADC) Channel 1
#  define Sf2M3_ADC_CH2         ((unsigned int) 0x1 <<  2)	// (ADC) Channel 2
#  define Sf2M3_ADC_CH3         ((unsigned int) 0x1 <<  3)	// (ADC) Channel 3
#  define Sf2M3_ADC_CH4         ((unsigned int) 0x1 <<  4)	// (ADC) Channel 4
#  define Sf2M3_ADC_CH5         ((unsigned int) 0x1 <<  5)	// (ADC) Channel 5
#  define Sf2M3_ADC_CH6         ((unsigned int) 0x1 <<  6)	// (ADC) Channel 6
#  define Sf2M3_ADC_CH7         ((unsigned int) 0x1 <<  7)	// (ADC) Channel 7
// --------     ADC_CHDR : (ADC Offset: 0x14) ADC Channel Disable Register --------
// --------     ADC_CHSR : (ADC Offset: 0x18) ADC Channel Status Register --------
// -------- ADC_SR : (ADC Offset: 0x1c) ADC Status Register --------
#  define Sf2M3_ADC_EOC0        ((unsigned int) 0x1 <<  0)	// (ADC) End of Conversion
#  define Sf2M3_ADC_EOC1        ((unsigned int) 0x1 <<  1)	// (ADC) End of Conversion
#  define Sf2M3_ADC_EOC2        ((unsigned int) 0x1 <<  2)	// (ADC) End of Conversion
#  define Sf2M3_ADC_EOC3        ((unsigned int) 0x1 <<  3)	// (ADC) End of Conversion
#  define Sf2M3_ADC_EOC4        ((unsigned int) 0x1 <<  4)	// (ADC) End of Conversion
#  define Sf2M3_ADC_EOC5        ((unsigned int) 0x1 <<  5)	// (ADC) End of Conversion
#  define Sf2M3_ADC_EOC6        ((unsigned int) 0x1 <<  6)	// (ADC) End of Conversion
#  define Sf2M3_ADC_EOC7        ((unsigned int) 0x1 <<  7)	// (ADC) End of Conversion
#  define Sf2M3_ADC_OVRE0       ((unsigned int) 0x1 <<  8)	// (ADC) Overrun Error
#  define Sf2M3_ADC_OVRE1       ((unsigned int) 0x1 <<  9)	// (ADC) Overrun Error
#  define Sf2M3_ADC_OVRE2       ((unsigned int) 0x1 << 10)	// (ADC) Overrun Error
#  define Sf2M3_ADC_OVRE3       ((unsigned int) 0x1 << 11)	// (ADC) Overrun Error
#  define Sf2M3_ADC_OVRE4       ((unsigned int) 0x1 << 12)	// (ADC) Overrun Error
#  define Sf2M3_ADC_OVRE5       ((unsigned int) 0x1 << 13)	// (ADC) Overrun Error
#  define Sf2M3_ADC_OVRE6       ((unsigned int) 0x1 << 14)	// (ADC) Overrun Error
#  define Sf2M3_ADC_OVRE7       ((unsigned int) 0x1 << 15)	// (ADC) Overrun Error
#  define Sf2M3_ADC_DRDY        ((unsigned int) 0x1 << 16)	// (ADC) Data Ready
#  define Sf2M3_ADC_GOVRE       ((unsigned int) 0x1 << 17)	// (ADC) General Overrun
#  define Sf2M3_ADC_ENDRX       ((unsigned int) 0x1 << 18)	// (ADC) End of Receiver Transfer
#  define Sf2M3_ADC_RXBUFF      ((unsigned int) 0x1 << 19)	// (ADC) RXBUFF Interrupt
// -------- ADC_LCDR : (ADC Offset: 0x20) ADC Last Converted Data Register --------
#  define Sf2M3_ADC_LDATA       ((unsigned int) 0x3FF <<  0)	// (ADC) Last Data Converted
// -------- ADC_IER : (ADC Offset: 0x24) ADC Interrupt Enable Register --------
// -------- ADC_IDR : (ADC Offset: 0x28) ADC Interrupt Disable Register --------
// -------- ADC_IMR : (ADC Offset: 0x2c) ADC Interrupt Mask Register --------
// -------- ADC_CDR0 : (ADC Offset: 0x30) ADC Channel Data Register 0 --------
#  define Sf2M3_ADC_DATA        ((unsigned int) 0x3FF <<  0)	// (ADC) Converted Data
// -------- ADC_CDR1 : (ADC Offset: 0x34) ADC Channel Data Register 1 --------
// -------- ADC_CDR2 : (ADC Offset: 0x38) ADC Channel Data Register 2 --------
// -------- ADC_CDR3 : (ADC Offset: 0x3c) ADC Channel Data Register 3 --------
// -------- ADC_CDR4 : (ADC Offset: 0x40) ADC Channel Data Register 4 --------
// -------- ADC_CDR5 : (ADC Offset: 0x44) ADC Channel Data Register 5 --------
// -------- ADC_CDR6 : (ADC Offset: 0x48) ADC Channel Data Register 6 --------
// -------- ADC_CDR7 : (ADC Offset: 0x4c) ADC Channel Data Register 7 --------

// *****************************************************************************
//              SOFTWARE API DEFINITION  FOR Synchronous Serial Controller Interface
// *****************************************************************************
typedef struct _Sf2M3S_SSC {
  Sf2M3_REG SSC_CR;		// Control Register
  Sf2M3_REG SSC_CMR;		// Clock Mode Register
  Sf2M3_REG Reserved0[2];	//
  Sf2M3_REG SSC_RCMR;		// Receive Clock ModeRegister
  Sf2M3_REG SSC_RFMR;		// Receive Frame Mode Register
  Sf2M3_REG SSC_TCMR;		// Transmit Clock Mode Register
  Sf2M3_REG SSC_TFMR;		// Transmit Frame Mode Register
  Sf2M3_REG SSC_RHR;		// Receive Holding Register
  Sf2M3_REG SSC_THR;		// Transmit Holding Register
  Sf2M3_REG Reserved1[2];	//
  Sf2M3_REG SSC_RSHR;		// Receive Sync Holding Register
  Sf2M3_REG SSC_TSHR;		// Transmit Sync Holding Register
  Sf2M3_REG SSC_RC0R;		// Receive Compare 0 Register
  Sf2M3_REG SSC_RC1R;		// Receive Compare 1 Register
  Sf2M3_REG SSC_SR;		// Status Register
  Sf2M3_REG SSC_IER;		// Interrupt Enable Register
  Sf2M3_REG SSC_IDR;		// Interrupt Disable Register
  Sf2M3_REG SSC_IMR;		// Interrupt Mask Register
  Sf2M3_REG Reserved2[44];	//
  Sf2M3_REG SSC_RPR;		// Receive Pointer Register
  Sf2M3_REG SSC_RCR;		// Receive Counter Register
  Sf2M3_REG SSC_TPR;		// Transmit Pointer Register
  Sf2M3_REG SSC_TCR;		// Transmit Counter Register
  Sf2M3_REG SSC_RNPR;		// Receive Next Pointer Register
  Sf2M3_REG SSC_RNCR;		// Receive Next Counter Register
  Sf2M3_REG SSC_TNPR;		// Transmit Next Pointer Register
  Sf2M3_REG SSC_TNCR;		// Transmit Next Counter Register
  Sf2M3_REG SSC_PTCR;		// PDC Transfer Control Register
  Sf2M3_REG SSC_PTSR;		// PDC Transfer Status Register
} Sf2M3S_SSC, *Sf2M3PS_SSC;

// -------- SSC_CR : (SSC Offset: 0x0) SSC Control Register --------
#  define Sf2M3_SSC_RXEN        ((unsigned int) 0x1 <<  0)	// (SSC) Receive Enable
#  define Sf2M3_SSC_RXDIS       ((unsigned int) 0x1 <<  1)	// (SSC) Receive Disable
#  define Sf2M3_SSC_TXEN        ((unsigned int) 0x1 <<  8)	// (SSC) Transmit Enable
#  define Sf2M3_SSC_TXDIS       ((unsigned int) 0x1 <<  9)	// (SSC) Transmit Disable
#  define Sf2M3_SSC_SWRST       ((unsigned int) 0x1 << 15)	// (SSC) Software Reset
// -------- SSC_RCMR : (SSC Offset: 0x10) SSC Receive Clock Mode Register --------
#  define Sf2M3_SSC_CKS         ((unsigned int) 0x3 <<  0)	// (SSC) Receive/Transmit Clock Selection
#  define 	Sf2M3_SSC_CKS_DIV                  ((unsigned int) 0x0)	// (SSC) Divided Clock
#  define 	Sf2M3_SSC_CKS_TK                   ((unsigned int) 0x1)	// (SSC) TK Clock signal
#  define 	Sf2M3_SSC_CKS_RK                   ((unsigned int) 0x2)	// (SSC) RK pin
#  define Sf2M3_SSC_CKO         ((unsigned int) 0x7 <<  2)	// (SSC) Receive/Transmit Clock Output Mode Selection
#  define 	Sf2M3_SSC_CKO_NONE                 ((unsigned int) 0x0 <<  2)	// (SSC) Receive/Transmit Clock Output Mode: None RK pin: Input-only
#  define 	Sf2M3_SSC_CKO_CONTINOUS            ((unsigned int) 0x1 <<  2)	// (SSC) Continuous Receive/Transmit Clock RK pin: Output
#  define 	Sf2M3_SSC_CKO_DATA_TX              ((unsigned int) 0x2 <<  2)	// (SSC) Receive/Transmit Clock only during data transfers RK pin: Output
#  define Sf2M3_SSC_CKI         ((unsigned int) 0x1 <<  5)	// (SSC) Receive/Transmit Clock Inversion
#  define Sf2M3_SSC_CKG         ((unsigned int) 0x3 <<  6)	// (SSC) Receive/Transmit Clock Gating Selection
#  define 	Sf2M3_SSC_CKG_NONE                 ((unsigned int) 0x0 <<  6)	// (SSC) Receive/Transmit Clock Gating: None, continuous clock
#  define 	Sf2M3_SSC_CKG_LOW                  ((unsigned int) 0x1 <<  6)	// (SSC) Receive/Transmit Clock enabled only if RF Low
#  define 	Sf2M3_SSC_CKG_HIGH                 ((unsigned int) 0x2 <<  6)	// (SSC) Receive/Transmit Clock enabled only if RF High
#  define Sf2M3_SSC_START       ((unsigned int) 0xF <<  8)	// (SSC) Receive/Transmit Start Selection
#  define 	Sf2M3_SSC_START_CONTINOUS            ((unsigned int) 0x0 <<  8)	// (SSC) Continuous, as soon as the receiver is enabled, and immediately after the end of transfer of the previous data.
#  define 	Sf2M3_SSC_START_TX                   ((unsigned int) 0x1 <<  8)	// (SSC) Transmit/Receive start
#  define 	Sf2M3_SSC_START_LOW_RF               ((unsigned int) 0x2 <<  8)	// (SSC) Detection of a low level on RF input
#  define 	Sf2M3_SSC_START_HIGH_RF              ((unsigned int) 0x3 <<  8)	// (SSC) Detection of a high level on RF input
#  define 	Sf2M3_SSC_START_FALL_RF              ((unsigned int) 0x4 <<  8)	// (SSC) Detection of a falling edge on RF input
#  define 	Sf2M3_SSC_START_RISE_RF              ((unsigned int) 0x5 <<  8)	// (SSC) Detection of a rising edge on RF input
#  define 	Sf2M3_SSC_START_LEVEL_RF             ((unsigned int) 0x6 <<  8)	// (SSC) Detection of any level change on RF input
#  define 	Sf2M3_SSC_START_EDGE_RF              ((unsigned int) 0x7 <<  8)	// (SSC) Detection of any edge on RF input
#  define 	Sf2M3_SSC_START_0                    ((unsigned int) 0x8 <<  8)	// (SSC) Compare 0
#  define Sf2M3_SSC_STOP        ((unsigned int) 0x1 << 12)	// (SSC) Receive Stop Selection
#  define Sf2M3_SSC_STTOUT      ((unsigned int) 0x1 << 15)	// (SSC) Receive/Transmit Start Output Selection
#  define Sf2M3_SSC_STTDLY      ((unsigned int) 0xFF << 16)	// (SSC) Receive/Transmit Start Delay
#  define Sf2M3_SSC_PERIOD      ((unsigned int) 0xFF << 24)	// (SSC) Receive/Transmit Period Divider Selection
// -------- SSC_RFMR : (SSC Offset: 0x14) SSC Receive Frame Mode Register --------
#  define Sf2M3_SSC_DATLEN      ((unsigned int) 0x1F <<  0)	// (SSC) Data Length
#  define Sf2M3_SSC_LOOP        ((unsigned int) 0x1 <<  5)	// (SSC) Loop Mode
#  define Sf2M3_SSC_MSBF        ((unsigned int) 0x1 <<  7)	// (SSC) Most Significant Bit First
#  define Sf2M3_SSC_DATNB       ((unsigned int) 0xF <<  8)	// (SSC) Data Number per Frame
#  define Sf2M3_SSC_FSLEN       ((unsigned int) 0xF << 16)	// (SSC) Receive/Transmit Frame Sync length
#  define Sf2M3_SSC_FSOS        ((unsigned int) 0x7 << 20)	// (SSC) Receive/Transmit Frame Sync Output Selection
#  define 	Sf2M3_SSC_FSOS_NONE                 ((unsigned int) 0x0 << 20)	// (SSC) Selected Receive/Transmit Frame Sync Signal: None RK pin Input-only
#  define 	Sf2M3_SSC_FSOS_NEGATIVE             ((unsigned int) 0x1 << 20)	// (SSC) Selected Receive/Transmit Frame Sync Signal: Negative Pulse
#  define 	Sf2M3_SSC_FSOS_POSITIVE             ((unsigned int) 0x2 << 20)	// (SSC) Selected Receive/Transmit Frame Sync Signal: Positive Pulse
#  define 	Sf2M3_SSC_FSOS_LOW                  ((unsigned int) 0x3 << 20)	// (SSC) Selected Receive/Transmit Frame Sync Signal: Driver Low during data transfer
#  define 	Sf2M3_SSC_FSOS_HIGH                 ((unsigned int) 0x4 << 20)	// (SSC) Selected Receive/Transmit Frame Sync Signal: Driver High during data transfer
#  define 	Sf2M3_SSC_FSOS_TOGGLE               ((unsigned int) 0x5 << 20)	// (SSC) Selected Receive/Transmit Frame Sync Signal: Toggling at each start of data transfer
#  define Sf2M3_SSC_FSEDGE      ((unsigned int) 0x1 << 24)	// (SSC) Frame Sync Edge Detection
// -------- SSC_TCMR : (SSC Offset: 0x18) SSC Transmit Clock Mode Register --------
// -------- SSC_TFMR : (SSC Offset: 0x1c) SSC Transmit Frame Mode Register --------
#  define Sf2M3_SSC_DATDEF      ((unsigned int) 0x1 <<  5)	// (SSC) Data Default Value
#  define Sf2M3_SSC_FSDEN       ((unsigned int) 0x1 << 23)	// (SSC) Frame Sync Data Enable
// -------- SSC_SR : (SSC Offset: 0x40) SSC Status Register --------
#  define Sf2M3_SSC_TXRDY       ((unsigned int) 0x1 <<  0)	// (SSC) Transmit Ready
#  define Sf2M3_SSC_TXEMPTY     ((unsigned int) 0x1 <<  1)	// (SSC) Transmit Empty
#  define Sf2M3_SSC_ENDTX       ((unsigned int) 0x1 <<  2)	// (SSC) End Of Transmission
#  define Sf2M3_SSC_TXBUFE      ((unsigned int) 0x1 <<  3)	// (SSC) Transmit Buffer Empty
#  define Sf2M3_SSC_RXRDY       ((unsigned int) 0x1 <<  4)	// (SSC) Receive Ready
#  define Sf2M3_SSC_OVRUN       ((unsigned int) 0x1 <<  5)	// (SSC) Receive Overrun
#  define Sf2M3_SSC_ENDRX       ((unsigned int) 0x1 <<  6)	// (SSC) End of Reception
#  define Sf2M3_SSC_RXBUFF      ((unsigned int) 0x1 <<  7)	// (SSC) Receive Buffer Full
#  define Sf2M3_SSC_CP0         ((unsigned int) 0x1 <<  8)	// (SSC) Compare 0
#  define Sf2M3_SSC_CP1         ((unsigned int) 0x1 <<  9)	// (SSC) Compare 1
#  define Sf2M3_SSC_TXSYN       ((unsigned int) 0x1 << 10)	// (SSC) Transmit Sync
#  define Sf2M3_SSC_RXSYN       ((unsigned int) 0x1 << 11)	// (SSC) Receive Sync
#  define Sf2M3_SSC_TXENA       ((unsigned int) 0x1 << 16)	// (SSC) Transmit Enable
#  define Sf2M3_SSC_RXENA       ((unsigned int) 0x1 << 17)	// (SSC) Receive Enable
// -------- SSC_IER : (SSC Offset: 0x44) SSC Interrupt Enable Register --------
// -------- SSC_IDR : (SSC Offset: 0x48) SSC Interrupt Disable Register --------
// -------- SSC_IMR : (SSC Offset: 0x4c) SSC Interrupt Mask Register --------

// *****************************************************************************
//              SOFTWARE API DEFINITION  FOR Usart
// *****************************************************************************
typedef struct _Sf2M3S_USART {
  Sf2M3_REG US_CR;		// Control Register
  Sf2M3_REG US_MR;		// Mode Register
  Sf2M3_REG US_IER;		// Interrupt Enable Register
  Sf2M3_REG US_IDR;		// Interrupt Disable Register
  Sf2M3_REG US_IMR;		// Interrupt Mask Register
  Sf2M3_REG US_CSR;		// Channel Status Register
  Sf2M3_REG US_RHR;		// Receiver Holding Register
  Sf2M3_REG US_THR;		// Transmitter Holding Register
  Sf2M3_REG US_BRGR;		// Baud Rate Generator Register
  Sf2M3_REG US_RTOR;		// Receiver Time-out Register
  Sf2M3_REG US_TTGR;		// Transmitter Time-guard Register
  Sf2M3_REG Reserved0[5];	//
  Sf2M3_REG US_FIDI;		// FI_DI_Ratio Register
  Sf2M3_REG US_NER;		// Nb Errors Register
  Sf2M3_REG US_XXR;		// XON_XOFF Register
  Sf2M3_REG US_IF;		// IRDA_FILTER Register
  Sf2M3_REG Reserved1[44];	//
  Sf2M3_REG US_RPR;		// Receive Pointer Register
  Sf2M3_REG US_RCR;		// Receive Counter Register
  Sf2M3_REG US_TPR;		// Transmit Pointer Register
  Sf2M3_REG US_TCR;		// Transmit Counter Register
  Sf2M3_REG US_RNPR;		// Receive Next Pointer Register
  Sf2M3_REG US_RNCR;		// Receive Next Counter Register
  Sf2M3_REG US_TNPR;		// Transmit Next Pointer Register
  Sf2M3_REG US_TNCR;		// Transmit Next Counter Register
  Sf2M3_REG US_PTCR;		// PDC Transfer Control Register
  Sf2M3_REG US_PTSR;		// PDC Transfer Status Register
} Sf2M3S_USART, *Sf2M3PS_USART;

// -------- US_CR : (USART Offset: 0x0) Debug Unit Control Register --------
#  define Sf2M3_US_RSTSTA       ((unsigned int) 0x1 <<  8)	// (USART) Reset Status Bits
#  define Sf2M3_US_STTBRK       ((unsigned int) 0x1 <<  9)	// (USART) Start Break
#  define Sf2M3_US_STPBRK       ((unsigned int) 0x1 << 10)	// (USART) Stop Break
#  define Sf2M3_US_STTTO        ((unsigned int) 0x1 << 11)	// (USART) Start Time-out
#  define Sf2M3_US_SENDA        ((unsigned int) 0x1 << 12)	// (USART) Send Address
#  define Sf2M3_US_RSTIT        ((unsigned int) 0x1 << 13)	// (USART) Reset Iterations
#  define Sf2M3_US_RSTNACK      ((unsigned int) 0x1 << 14)	// (USART) Reset Non Acknowledge
#  define Sf2M3_US_RETTO        ((unsigned int) 0x1 << 15)	// (USART) Rearm Time-out
#  define Sf2M3_US_DTREN        ((unsigned int) 0x1 << 16)	// (USART) Data Terminal ready Enable
#  define Sf2M3_US_DTRDIS       ((unsigned int) 0x1 << 17)	// (USART) Data Terminal ready Disable
#  define Sf2M3_US_RTSEN        ((unsigned int) 0x1 << 18)	// (USART) Request to Send enable
#  define Sf2M3_US_RTSDIS       ((unsigned int) 0x1 << 19)	// (USART) Request to Send Disable
// -------- US_MR : (USART Offset: 0x4) Debug Unit Mode Register --------
#  define Sf2M3_US_USMODE       ((unsigned int) 0xF <<  0)	// (USART) Usart mode
#  define 	Sf2M3_US_USMODE_NORMAL               ((unsigned int) 0x0)	// (USART) Normal
#  define 	Sf2M3_US_USMODE_RS485                ((unsigned int) 0x1)	// (USART) RS485
#  define 	Sf2M3_US_USMODE_HWHSH                ((unsigned int) 0x2)	// (USART) Hardware Handshaking
#  define 	Sf2M3_US_USMODE_MODEM                ((unsigned int) 0x3)	// (USART) Modem
#  define 	Sf2M3_US_USMODE_ISO7816_0            ((unsigned int) 0x4)	// (USART) ISO7816 protocol: T = 0
#  define 	Sf2M3_US_USMODE_ISO7816_1            ((unsigned int) 0x6)	// (USART) ISO7816 protocol: T = 1
#  define 	Sf2M3_US_USMODE_IRDA                 ((unsigned int) 0x8)	// (USART) IrDA
#  define 	Sf2M3_US_USMODE_SWHSH                ((unsigned int) 0xC)	// (USART) Software Handshaking
#  define Sf2M3_US_CLKS         ((unsigned int) 0x3 <<  4)	// (USART) Clock Selection (Baud Rate generator Input Clock
#  define 	Sf2M3_US_CLKS_CLOCK                ((unsigned int) 0x0 <<  4)	// (USART) Clock
#  define 	Sf2M3_US_CLKS_FDIV1                ((unsigned int) 0x1 <<  4)	// (USART) fdiv1
#  define 	Sf2M3_US_CLKS_SLOW                 ((unsigned int) 0x2 <<  4)	// (USART) slow_clock (ARM)
#  define 	Sf2M3_US_CLKS_EXT                  ((unsigned int) 0x3 <<  4)	// (USART) External (SCK)
#  define Sf2M3_US_CHRL         ((unsigned int) 0x3 <<  6)	// (USART) Clock Selection (Baud Rate generator Input Clock
#  define 	Sf2M3_US_CHRL_5_BITS               ((unsigned int) 0x0 <<  6)	// (USART) Character Length: 5 bits
#  define 	Sf2M3_US_CHRL_6_BITS               ((unsigned int) 0x1 <<  6)	// (USART) Character Length: 6 bits
#  define 	Sf2M3_US_CHRL_7_BITS               ((unsigned int) 0x2 <<  6)	// (USART) Character Length: 7 bits
#  define 	Sf2M3_US_CHRL_8_BITS               ((unsigned int) 0x3 <<  6)	// (USART) Character Length: 8 bits
#  define Sf2M3_US_SYNC         ((unsigned int) 0x1 <<  8)	// (USART) Synchronous Mode Select
#  define Sf2M3_US_NBSTOP       ((unsigned int) 0x3 << 12)	// (USART) Number of Stop bits
#  define 	Sf2M3_US_NBSTOP_1_BIT                ((unsigned int) 0x0 << 12)	// (USART) 1 stop bit
#  define 	Sf2M3_US_NBSTOP_15_BIT               ((unsigned int) 0x1 << 12)	// (USART) Asynchronous (SYNC=0) 2 stop bits Synchronous (SYNC=1) 2 stop bits
#  define 	Sf2M3_US_NBSTOP_2_BIT                ((unsigned int) 0x2 << 12)	// (USART) 2 stop bits
#  define Sf2M3_US_MSBF         ((unsigned int) 0x1 << 16)	// (USART) Bit Order
#  define Sf2M3_US_MODE9        ((unsigned int) 0x1 << 17)	// (USART) 9-bit Character length
#  define Sf2M3_US_CKLO         ((unsigned int) 0x1 << 18)	// (USART) Clock Output Select
#  define Sf2M3_US_OVER         ((unsigned int) 0x1 << 19)	// (USART) Over Sampling Mode
#  define Sf2M3_US_INACK        ((unsigned int) 0x1 << 20)	// (USART) Inhibit Non Acknowledge
#  define Sf2M3_US_DSNACK       ((unsigned int) 0x1 << 21)	// (USART) Disable Successive NACK
#  define Sf2M3_US_MAX_ITER     ((unsigned int) 0x1 << 24)	// (USART) Number of Repetitions
#  define Sf2M3_US_FILTER       ((unsigned int) 0x1 << 28)	// (USART) Receive Line Filter
// -------- US_IER : (USART Offset: 0x8) Debug Unit Interrupt Enable Register --------
#  define Sf2M3_US_RXBRK        ((unsigned int) 0x1 <<  2)	// (USART) Break Received/End of Break
#  define Sf2M3_US_TIMEOUT      ((unsigned int) 0x1 <<  8)	// (USART) Receiver Time-out
#  define Sf2M3_US_ITERATION    ((unsigned int) 0x1 << 10)	// (USART) Max number of Repetitions Reached
#  define Sf2M3_US_NACK         ((unsigned int) 0x1 << 13)	// (USART) Non Acknowledge
#  define Sf2M3_US_RIIC         ((unsigned int) 0x1 << 16)	// (USART) Ring INdicator Input Change Flag
#  define Sf2M3_US_DSRIC        ((unsigned int) 0x1 << 17)	// (USART) Data Set Ready Input Change Flag
#  define Sf2M3_US_DCDIC        ((unsigned int) 0x1 << 18)	// (USART) Data Carrier Flag
#  define Sf2M3_US_CTSIC        ((unsigned int) 0x1 << 19)	// (USART) Clear To Send Input Change Flag
// -------- US_IDR : (USART Offset: 0xc) Debug Unit Interrupt Disable Register --------
// -------- US_IMR : (USART Offset: 0x10) Debug Unit Interrupt Mask Register --------
// -------- US_CSR : (USART Offset: 0x14) Debug Unit Channel Status Register --------
#  define Sf2M3_US_RI           ((unsigned int) 0x1 << 20)	// (USART) Image of RI Input
#  define Sf2M3_US_DSR          ((unsigned int) 0x1 << 21)	// (USART) Image of DSR Input
#  define Sf2M3_US_DCD          ((unsigned int) 0x1 << 22)	// (USART) Image of DCD Input
#  define Sf2M3_US_CTS          ((unsigned int) 0x1 << 23)	// (USART) Image of CTS Input

// *****************************************************************************
//              SOFTWARE API DEFINITION  FOR Two-wire Interface
// *****************************************************************************
typedef struct _Sf2M3S_TWI {
  Sf2M3_REG TWI_CR;		// Control Register
  Sf2M3_REG TWI_MMR;		// Master Mode Register
  Sf2M3_REG TWI_SMR;		// Slave Mode Register
  Sf2M3_REG TWI_IADR;		// Internal Address Register
  Sf2M3_REG TWI_CWGR;		// Clock Waveform Generator Register
  Sf2M3_REG Reserved0[3];	//
  Sf2M3_REG TWI_SR;		// Status Register
  Sf2M3_REG TWI_IER;		// Interrupt Enable Register
  Sf2M3_REG TWI_IDR;		// Interrupt Disable Register
  Sf2M3_REG TWI_IMR;		// Interrupt Mask Register
  Sf2M3_REG TWI_RHR;		// Receive Holding Register
  Sf2M3_REG TWI_THR;		// Transmit Holding Register
} Sf2M3S_TWI, *Sf2M3PS_TWI;

// -------- TWI_CR : (TWI Offset: 0x0) TWI Control Register --------
#  define Sf2M3_TWI_START       ((unsigned int) 0x1 <<  0)	// (TWI) Send a START Condition
#  define Sf2M3_TWI_STOP        ((unsigned int) 0x1 <<  1)	// (TWI) Send a STOP Condition
#  define Sf2M3_TWI_MSEN        ((unsigned int) 0x1 <<  2)	// (TWI) TWI Master Transfer Enabled
#  define Sf2M3_TWI_MSDIS       ((unsigned int) 0x1 <<  3)	// (TWI) TWI Master Transfer Disabled
#  define Sf2M3_TWI_SVEN        ((unsigned int) 0x1 <<  4)	// (TWI) TWI Slave Transfer Enabled
#  define Sf2M3_TWI_SVDIS       ((unsigned int) 0x1 <<  5)	// (TWI) TWI Slave Transfer Disabled
#  define Sf2M3_TWI_SWRST       ((unsigned int) 0x1 <<  7)	// (TWI) Software Reset
// -------- TWI_MMR : (TWI Offset: 0x4) TWI Master Mode Register --------
#  define Sf2M3_TWI_IADRSZ      ((unsigned int) 0x3 <<  8)	// (TWI) Internal Device Address Size
#  define 	Sf2M3_TWI_IADRSZ_NO                   ((unsigned int) 0x0 <<  8)	// (TWI) No internal device address
#  define 	Sf2M3_TWI_IADRSZ_1_BYTE               ((unsigned int) 0x1 <<  8)	// (TWI) One-byte internal device address
#  define 	Sf2M3_TWI_IADRSZ_2_BYTE               ((unsigned int) 0x2 <<  8)	// (TWI) Two-byte internal device address
#  define 	Sf2M3_TWI_IADRSZ_3_BYTE               ((unsigned int) 0x3 <<  8)	// (TWI) Three-byte internal device address
#  define Sf2M3_TWI_MREAD       ((unsigned int) 0x1 << 12)	// (TWI) Master Read Direction
#  define Sf2M3_TWI_DADR        ((unsigned int) 0x7F << 16)	// (TWI) Device Address
// -------- TWI_SMR : (TWI Offset: 0x8) TWI Slave Mode Register --------
#  define Sf2M3_TWI_SADR        ((unsigned int) 0x7F << 16)	// (TWI) Slave Device Address
// -------- TWI_CWGR : (TWI Offset: 0x10) TWI Clock Waveform Generator Register --------
#  define Sf2M3_TWI_CLDIV       ((unsigned int) 0xFF <<  0)	// (TWI) Clock Low Divider
#  define Sf2M3_TWI_CHDIV       ((unsigned int) 0xFF <<  8)	// (TWI) Clock High Divider
#  define Sf2M3_TWI_CKDIV       ((unsigned int) 0x7 << 16)	// (TWI) Clock Divider
// -------- TWI_SR : (TWI Offset: 0x20) TWI Status Register --------
#  define Sf2M3_TWI_TXCOMP      ((unsigned int) 0x1 <<  0)	// (TWI) Transmission Completed
#  define Sf2M3_TWI_RXRDY       ((unsigned int) 0x1 <<  1)	// (TWI) Receive holding register ReaDY
#  define Sf2M3_TWI_TXRDY       ((unsigned int) 0x1 <<  2)	// (TWI) Transmit holding register ReaDY
#  define Sf2M3_TWI_SVREAD      ((unsigned int) 0x1 <<  3)	// (TWI) Slave Read
#  define Sf2M3_TWI_SVACC       ((unsigned int) 0x1 <<  4)	// (TWI) Slave Access
#  define Sf2M3_TWI_GCACC       ((unsigned int) 0x1 <<  5)	// (TWI) General Call Access
#  define Sf2M3_TWI_OVRE        ((unsigned int) 0x1 <<  6)	// (TWI) Overrun Error
#  define Sf2M3_TWI_UNRE        ((unsigned int) 0x1 <<  7)	// (TWI) Underrun Error
#  define Sf2M3_TWI_NACK        ((unsigned int) 0x1 <<  8)	// (TWI) Not Acknowledged
#  define Sf2M3_TWI_ARBLST      ((unsigned int) 0x1 <<  9)	// (TWI) Arbitration Lost
// -------- TWI_IER : (TWI Offset: 0x24) TWI Interrupt Enable Register --------
// -------- TWI_IDR : (TWI Offset: 0x28) TWI Interrupt Disable Register --------
// -------- TWI_IMR : (TWI Offset: 0x2c) TWI Interrupt Mask Register --------

// *****************************************************************************
//              SOFTWARE API DEFINITION  FOR Timer Counter Channel Interface
// *****************************************************************************
typedef struct _Sf2M3S_TC {
  Sf2M3_REG TC_CCR;		// Channel Control Register
  Sf2M3_REG TC_CMR;		// Channel Mode Register (Capture Mode / Waveform Mode)
  Sf2M3_REG Reserved0[2];	//
  Sf2M3_REG TC_CV;		// Counter Value
  Sf2M3_REG TC_RA;		// Register A
  Sf2M3_REG TC_RB;		// Register B
  Sf2M3_REG TC_RC;		// Register C
  Sf2M3_REG TC_SR;		// Status Register
  Sf2M3_REG TC_IER;		// Interrupt Enable Register
  Sf2M3_REG TC_IDR;		// Interrupt Disable Register
  Sf2M3_REG TC_IMR;		// Interrupt Mask Register
} Sf2M3S_TC, *Sf2M3PS_TC;

// -------- TC_CCR : (TC Offset: 0x0) TC Channel Control Register --------
#  define Sf2M3_TC_CLKEN        ((unsigned int) 0x1 <<  0)	// (TC) Counter Clock Enable Command
#  define Sf2M3_TC_CLKDIS       ((unsigned int) 0x1 <<  1)	// (TC) Counter Clock Disable Command
#  define Sf2M3_TC_SWTRG        ((unsigned int) 0x1 <<  2)	// (TC) Software Trigger Command
// -------- TC_CMR : (TC Offset: 0x4) TC Channel Mode Register: Capture Mode / Waveform Mode --------
#  define Sf2M3_TC_CLKS         ((unsigned int) 0x7 <<  0)	// (TC) Clock Selection
#  define 	Sf2M3_TC_CLKS_TIMER_DIV1_CLOCK     ((unsigned int) 0x0)	// (TC) Clock selected: TIMER_DIV1_CLOCK
#  define 	Sf2M3_TC_CLKS_TIMER_DIV2_CLOCK     ((unsigned int) 0x1)	// (TC) Clock selected: TIMER_DIV2_CLOCK
#  define 	Sf2M3_TC_CLKS_TIMER_DIV3_CLOCK     ((unsigned int) 0x2)	// (TC) Clock selected: TIMER_DIV3_CLOCK
#  define 	Sf2M3_TC_CLKS_TIMER_DIV4_CLOCK     ((unsigned int) 0x3)	// (TC) Clock selected: TIMER_DIV4_CLOCK
#  define 	Sf2M3_TC_CLKS_TIMER_DIV5_CLOCK     ((unsigned int) 0x4)	// (TC) Clock selected: TIMER_DIV5_CLOCK
#  define 	Sf2M3_TC_CLKS_XC0                  ((unsigned int) 0x5)	// (TC) Clock selected: XC0
#  define 	Sf2M3_TC_CLKS_XC1                  ((unsigned int) 0x6)	// (TC) Clock selected: XC1
#  define 	Sf2M3_TC_CLKS_XC2                  ((unsigned int) 0x7)	// (TC) Clock selected: XC2
#  define Sf2M3_TC_CLKI         ((unsigned int) 0x1 <<  3)	// (TC) Clock Invert
#  define Sf2M3_TC_BURST        ((unsigned int) 0x3 <<  4)	// (TC) Burst Signal Selection
#  define 	Sf2M3_TC_BURST_NONE                 ((unsigned int) 0x0 <<  4)	// (TC) The clock is not gated by an external signal
#  define 	Sf2M3_TC_BURST_XC0                  ((unsigned int) 0x1 <<  4)	// (TC) XC0 is ANDed with the selected clock
#  define 	Sf2M3_TC_BURST_XC1                  ((unsigned int) 0x2 <<  4)	// (TC) XC1 is ANDed with the selected clock
#  define 	Sf2M3_TC_BURST_XC2                  ((unsigned int) 0x3 <<  4)	// (TC) XC2 is ANDed with the selected clock
#  define Sf2M3_TC_CPCSTOP      ((unsigned int) 0x1 <<  6)	// (TC) Counter Clock Stopped with RC Compare
#  define Sf2M3_TC_LDBSTOP      ((unsigned int) 0x1 <<  6)	// (TC) Counter Clock Stopped with RB Loading
#  define Sf2M3_TC_LDBDIS       ((unsigned int) 0x1 <<  7)	// (TC) Counter Clock Disabled with RB Loading
#  define Sf2M3_TC_CPCDIS       ((unsigned int) 0x1 <<  7)	// (TC) Counter Clock Disable with RC Compare
#  define Sf2M3_TC_ETRGEDG      ((unsigned int) 0x3 <<  8)	// (TC) External Trigger Edge Selection
#  define 	Sf2M3_TC_ETRGEDG_NONE                 ((unsigned int) 0x0 <<  8)	// (TC) Edge: None
#  define 	Sf2M3_TC_ETRGEDG_RISING               ((unsigned int) 0x1 <<  8)	// (TC) Edge: rising edge
#  define 	Sf2M3_TC_ETRGEDG_FALLING              ((unsigned int) 0x2 <<  8)	// (TC) Edge: falling edge
#  define 	Sf2M3_TC_ETRGEDG_BOTH                 ((unsigned int) 0x3 <<  8)	// (TC) Edge: each edge
#  define Sf2M3_TC_EEVTEDG      ((unsigned int) 0x3 <<  8)	// (TC) External Event Edge Selection
#  define 	Sf2M3_TC_EEVTEDG_NONE                 ((unsigned int) 0x0 <<  8)	// (TC) Edge: None
#  define 	Sf2M3_TC_EEVTEDG_RISING               ((unsigned int) 0x1 <<  8)	// (TC) Edge: rising edge
#  define 	Sf2M3_TC_EEVTEDG_FALLING              ((unsigned int) 0x2 <<  8)	// (TC) Edge: falling edge
#  define 	Sf2M3_TC_EEVTEDG_BOTH                 ((unsigned int) 0x3 <<  8)	// (TC) Edge: each edge
#  define Sf2M3_TC_ABETRG       ((unsigned int) 0x1 << 10)	// (TC) TIOA or TIOB External Trigger Selection
#  define Sf2M3_TC_EEVT         ((unsigned int) 0x3 << 10)	// (TC) External Event  Selection
#  define 	Sf2M3_TC_EEVT_NONE                 ((unsigned int) 0x0 << 10)	// (TC) Signal selected as external event: TIOB TIOB direction: input
#  define 	Sf2M3_TC_EEVT_RISING               ((unsigned int) 0x1 << 10)	// (TC) Signal selected as external event: XC0 TIOB direction: output
#  define 	Sf2M3_TC_EEVT_FALLING              ((unsigned int) 0x2 << 10)	// (TC) Signal selected as external event: XC1 TIOB direction: output
#  define 	Sf2M3_TC_EEVT_BOTH                 ((unsigned int) 0x3 << 10)	// (TC) Signal selected as external event: XC2 TIOB direction: output
#  define Sf2M3_TC_ENETRG       ((unsigned int) 0x1 << 12)	// (TC) External Event Trigger enable
#  define Sf2M3_TC_WAVESEL      ((unsigned int) 0x3 << 13)	// (TC) Waveform  Selection
#  define 	Sf2M3_TC_WAVESEL_UP                   ((unsigned int) 0x0 << 13)	// (TC) UP mode without atomatic trigger on RC Compare
#  define 	Sf2M3_TC_WAVESEL_UPDOWN               ((unsigned int) 0x1 << 13)	// (TC) UPDOWN mode without automatic trigger on RC Compare
#  define 	Sf2M3_TC_WAVESEL_UP_AUTO              ((unsigned int) 0x2 << 13)	// (TC) UP mode with automatic trigger on RC Compare
#  define 	Sf2M3_TC_WAVESEL_UPDOWN_AUTO          ((unsigned int) 0x3 << 13)	// (TC) UPDOWN mode with automatic trigger on RC Compare
#  define Sf2M3_TC_CPCTRG       ((unsigned int) 0x1 << 14)	// (TC) RC Compare Trigger Enable
#  define Sf2M3_TC_WAVE         ((unsigned int) 0x1 << 15)	// (TC)
#  define Sf2M3_TC_LDRA         ((unsigned int) 0x3 << 16)	// (TC) RA Loading Selection
#  define 	Sf2M3_TC_LDRA_NONE                 ((unsigned int) 0x0 << 16)	// (TC) Edge: None
#  define 	Sf2M3_TC_LDRA_RISING               ((unsigned int) 0x1 << 16)	// (TC) Edge: rising edge of TIOA
#  define 	Sf2M3_TC_LDRA_FALLING              ((unsigned int) 0x2 << 16)	// (TC) Edge: falling edge of TIOA
#  define 	Sf2M3_TC_LDRA_BOTH                 ((unsigned int) 0x3 << 16)	// (TC) Edge: each edge of TIOA
#  define Sf2M3_TC_ACPA         ((unsigned int) 0x3 << 16)	// (TC) RA Compare Effect on TIOA
#  define 	Sf2M3_TC_ACPA_NONE                 ((unsigned int) 0x0 << 16)	// (TC) Effect: none
#  define 	Sf2M3_TC_ACPA_SET                  ((unsigned int) 0x1 << 16)	// (TC) Effect: set
#  define 	Sf2M3_TC_ACPA_CLEAR                ((unsigned int) 0x2 << 16)	// (TC) Effect: clear
#  define 	Sf2M3_TC_ACPA_TOGGLE               ((unsigned int) 0x3 << 16)	// (TC) Effect: toggle
#  define Sf2M3_TC_LDRB         ((unsigned int) 0x3 << 18)	// (TC) RB Loading Selection
#  define 	Sf2M3_TC_LDRB_NONE                 ((unsigned int) 0x0 << 18)	// (TC) Edge: None
#  define 	Sf2M3_TC_LDRB_RISING               ((unsigned int) 0x1 << 18)	// (TC) Edge: rising edge of TIOA
#  define 	Sf2M3_TC_LDRB_FALLING              ((unsigned int) 0x2 << 18)	// (TC) Edge: falling edge of TIOA
#  define 	Sf2M3_TC_LDRB_BOTH                 ((unsigned int) 0x3 << 18)	// (TC) Edge: each edge of TIOA
#  define Sf2M3_TC_ACPC         ((unsigned int) 0x3 << 18)	// (TC) RC Compare Effect on TIOA
#  define 	Sf2M3_TC_ACPC_NONE                 ((unsigned int) 0x0 << 18)	// (TC) Effect: none
#  define 	Sf2M3_TC_ACPC_SET                  ((unsigned int) 0x1 << 18)	// (TC) Effect: set
#  define 	Sf2M3_TC_ACPC_CLEAR                ((unsigned int) 0x2 << 18)	// (TC) Effect: clear
#  define 	Sf2M3_TC_ACPC_TOGGLE               ((unsigned int) 0x3 << 18)	// (TC) Effect: toggle
#  define Sf2M3_TC_AEEVT        ((unsigned int) 0x3 << 20)	// (TC) External Event Effect on TIOA
#  define 	Sf2M3_TC_AEEVT_NONE                 ((unsigned int) 0x0 << 20)	// (TC) Effect: none
#  define 	Sf2M3_TC_AEEVT_SET                  ((unsigned int) 0x1 << 20)	// (TC) Effect: set
#  define 	Sf2M3_TC_AEEVT_CLEAR                ((unsigned int) 0x2 << 20)	// (TC) Effect: clear
#  define 	Sf2M3_TC_AEEVT_TOGGLE               ((unsigned int) 0x3 << 20)	// (TC) Effect: toggle
#  define Sf2M3_TC_ASWTRG       ((unsigned int) 0x3 << 22)	// (TC) Software Trigger Effect on TIOA
#  define 	Sf2M3_TC_ASWTRG_NONE                 ((unsigned int) 0x0 << 22)	// (TC) Effect: none
#  define 	Sf2M3_TC_ASWTRG_SET                  ((unsigned int) 0x1 << 22)	// (TC) Effect: set
#  define 	Sf2M3_TC_ASWTRG_CLEAR                ((unsigned int) 0x2 << 22)	// (TC) Effect: clear
#  define 	Sf2M3_TC_ASWTRG_TOGGLE               ((unsigned int) 0x3 << 22)	// (TC) Effect: toggle
#  define Sf2M3_TC_BCPB         ((unsigned int) 0x3 << 24)	// (TC) RB Compare Effect on TIOB
#  define 	Sf2M3_TC_BCPB_NONE                 ((unsigned int) 0x0 << 24)	// (TC) Effect: none
#  define 	Sf2M3_TC_BCPB_SET                  ((unsigned int) 0x1 << 24)	// (TC) Effect: set
#  define 	Sf2M3_TC_BCPB_CLEAR                ((unsigned int) 0x2 << 24)	// (TC) Effect: clear
#  define 	Sf2M3_TC_BCPB_TOGGLE               ((unsigned int) 0x3 << 24)	// (TC) Effect: toggle
#  define Sf2M3_TC_BCPC         ((unsigned int) 0x3 << 26)	// (TC) RC Compare Effect on TIOB
#  define 	Sf2M3_TC_BCPC_NONE                 ((unsigned int) 0x0 << 26)	// (TC) Effect: none
#  define 	Sf2M3_TC_BCPC_SET                  ((unsigned int) 0x1 << 26)	// (TC) Effect: set
#  define 	Sf2M3_TC_BCPC_CLEAR                ((unsigned int) 0x2 << 26)	// (TC) Effect: clear
#  define 	Sf2M3_TC_BCPC_TOGGLE               ((unsigned int) 0x3 << 26)	// (TC) Effect: toggle
#  define Sf2M3_TC_BEEVT        ((unsigned int) 0x3 << 28)	// (TC) External Event Effect on TIOB
#  define 	Sf2M3_TC_BEEVT_NONE                 ((unsigned int) 0x0 << 28)	// (TC) Effect: none
#  define 	Sf2M3_TC_BEEVT_SET                  ((unsigned int) 0x1 << 28)	// (TC) Effect: set
#  define 	Sf2M3_TC_BEEVT_CLEAR                ((unsigned int) 0x2 << 28)	// (TC) Effect: clear
#  define 	Sf2M3_TC_BEEVT_TOGGLE               ((unsigned int) 0x3 << 28)	// (TC) Effect: toggle
#  define Sf2M3_TC_BSWTRG       ((unsigned int) 0x3 << 30)	// (TC) Software Trigger Effect on TIOB
#  define 	Sf2M3_TC_BSWTRG_NONE                 ((unsigned int) 0x0 << 30)	// (TC) Effect: none
#  define 	Sf2M3_TC_BSWTRG_SET                  ((unsigned int) 0x1 << 30)	// (TC) Effect: set
#  define 	Sf2M3_TC_BSWTRG_CLEAR                ((unsigned int) 0x2 << 30)	// (TC) Effect: clear
#  define 	Sf2M3_TC_BSWTRG_TOGGLE               ((unsigned int) 0x3 << 30)	// (TC) Effect: toggle
// -------- TC_SR : (TC Offset: 0x20) TC Channel Status Register --------
#  define Sf2M3_TC_COVFS        ((unsigned int) 0x1 <<  0)	// (TC) Counter Overflow
#  define Sf2M3_TC_LOVRS        ((unsigned int) 0x1 <<  1)	// (TC) Load Overrun
#  define Sf2M3_TC_CPAS         ((unsigned int) 0x1 <<  2)	// (TC) RA Compare
#  define Sf2M3_TC_CPBS         ((unsigned int) 0x1 <<  3)	// (TC) RB Compare
#  define Sf2M3_TC_CPCS         ((unsigned int) 0x1 <<  4)	// (TC) RC Compare
#  define Sf2M3_TC_LDRAS        ((unsigned int) 0x1 <<  5)	// (TC) RA Loading
#  define Sf2M3_TC_LDRBS        ((unsigned int) 0x1 <<  6)	// (TC) RB Loading
#  define Sf2M3_TC_ETRCS        ((unsigned int) 0x1 <<  7)	// (TC) External Trigger
#  define Sf2M3_TC_ETRGS        ((unsigned int) 0x1 << 16)	// (TC) Clock Enabling
#  define Sf2M3_TC_MTIOA        ((unsigned int) 0x1 << 17)	// (TC) TIOA Mirror
#  define Sf2M3_TC_MTIOB        ((unsigned int) 0x1 << 18)	// (TC) TIOA Mirror
// -------- TC_IER : (TC Offset: 0x24) TC Channel Interrupt Enable Register --------
// -------- TC_IDR : (TC Offset: 0x28) TC Channel Interrupt Disable Register --------
// -------- TC_IMR : (TC Offset: 0x2c) TC Channel Interrupt Mask Register --------

// *****************************************************************************
//              SOFTWARE API DEFINITION  FOR Timer Counter Interface
// *****************************************************************************
typedef struct _Sf2M3S_TCB {
  Sf2M3S_TC TCB_TC0;		// TC Channel 0
  Sf2M3_REG Reserved0[4];	//
  Sf2M3S_TC TCB_TC1;		// TC Channel 1
  Sf2M3_REG Reserved1[4];	//
  Sf2M3S_TC TCB_TC2;		// TC Channel 2
  Sf2M3_REG Reserved2[4];	//
  Sf2M3_REG TCB_BCR;		// TC Block Control Register
  Sf2M3_REG TCB_BMR;		// TC Block Mode Register
} Sf2M3S_TCB, *Sf2M3PS_TCB;

// -------- TCB_BCR : (TCB Offset: 0xc0) TC Block Control Register --------
#  define Sf2M3_TCB_SYNC        ((unsigned int) 0x1 <<  0)	// (TCB) Synchro Command
// -------- TCB_BMR : (TCB Offset: 0xc4) TC Block Mode Register --------
#  define Sf2M3_TCB_TC0XC0S     ((unsigned int) 0x1 <<  0)	// (TCB) External Clock Signal 0 Selection
#  define 	Sf2M3_TCB_TC0XC0S_TCLK0                ((unsigned int) 0x0)	// (TCB) TCLK0 connected to XC0
#  define 	Sf2M3_TCB_TC0XC0S_NONE                 ((unsigned int) 0x1)	// (TCB) None signal connected to XC0
#  define 	Sf2M3_TCB_TC0XC0S_TIOA1                ((unsigned int) 0x2)	// (TCB) TIOA1 connected to XC0
#  define 	Sf2M3_TCB_TC0XC0S_TIOA2                ((unsigned int) 0x3)	// (TCB) TIOA2 connected to XC0
#  define Sf2M3_TCB_TC1XC1S     ((unsigned int) 0x1 <<  2)	// (TCB) External Clock Signal 1 Selection
#  define 	Sf2M3_TCB_TC1XC1S_TCLK1                ((unsigned int) 0x0 <<  2)	// (TCB) TCLK1 connected to XC1
#  define 	Sf2M3_TCB_TC1XC1S_NONE                 ((unsigned int) 0x1 <<  2)	// (TCB) None signal connected to XC1
#  define 	Sf2M3_TCB_TC1XC1S_TIOA0                ((unsigned int) 0x2 <<  2)	// (TCB) TIOA0 connected to XC1
#  define 	Sf2M3_TCB_TC1XC1S_TIOA2                ((unsigned int) 0x3 <<  2)	// (TCB) TIOA2 connected to XC1
#  define Sf2M3_TCB_TC2XC2S     ((unsigned int) 0x1 <<  4)	// (TCB) External Clock Signal 2 Selection
#  define 	Sf2M3_TCB_TC2XC2S_TCLK2                ((unsigned int) 0x0 <<  4)	// (TCB) TCLK2 connected to XC2
#  define 	Sf2M3_TCB_TC2XC2S_NONE                 ((unsigned int) 0x1 <<  4)	// (TCB) None signal connected to XC2
#  define 	Sf2M3_TCB_TC2XC2S_TIOA0                ((unsigned int) 0x2 <<  4)	// (TCB) TIOA0 connected to XC2
#  define 	Sf2M3_TCB_TC2XC2S_TIOA2                ((unsigned int) 0x3 <<  4)	// (TCB) TIOA2 connected to XC2

// *****************************************************************************
//              SOFTWARE API DEFINITION  FOR PWMC Channel Interface
// *****************************************************************************
typedef struct _Sf2M3S_PWMC_CH {
  Sf2M3_REG PWMC_CMR;		// Channel Mode Register
  Sf2M3_REG PWMC_CDTYR;		// Channel Duty Cycle Register
  Sf2M3_REG PWMC_CPRDR;		// Channel Period Register
  Sf2M3_REG PWMC_CCNTR;		// Channel Counter Register
  Sf2M3_REG PWMC_CUPDR;		// Channel Update Register
  Sf2M3_REG PWMC_Reserved[3];	// Reserved
} Sf2M3S_PWMC_CH, *Sf2M3PS_PWMC_CH;

// -------- PWMC_CMR : (PWMC_CH Offset: 0x0) PWMC Channel Mode Register --------
#  define Sf2M3_PWMC_CPRE       ((unsigned int) 0xF <<  0)	// (PWMC_CH) Channel Pre-scaler : PWMC_CLKx
#  define 	Sf2M3_PWMC_CPRE_MCK                  ((unsigned int) 0x0)	// (PWMC_CH)
#  define 	Sf2M3_PWMC_CPRE_MCKA                 ((unsigned int) 0xB)	// (PWMC_CH)
#  define 	Sf2M3_PWMC_CPRE_MCKB                 ((unsigned int) 0xC)	// (PWMC_CH)
#  define Sf2M3_PWMC_CALG       ((unsigned int) 0x1 <<  8)	// (PWMC_CH) Channel Alignment
#  define Sf2M3_PWMC_CPOL       ((unsigned int) 0x1 <<  9)	// (PWMC_CH) Channel Polarity
#  define Sf2M3_PWMC_CPD        ((unsigned int) 0x1 << 10)	// (PWMC_CH) Channel Update Period
// -------- PWMC_CDTYR : (PWMC_CH Offset: 0x4) PWMC Channel Duty Cycle Register --------
#  define Sf2M3_PWMC_CDTY       ((unsigned int) 0x0 <<  0)	// (PWMC_CH) Channel Duty Cycle
// -------- PWMC_CPRDR : (PWMC_CH Offset: 0x8) PWMC Channel Period Register --------
#  define Sf2M3_PWMC_CPRD       ((unsigned int) 0x0 <<  0)	// (PWMC_CH) Channel Period
// -------- PWMC_CCNTR : (PWMC_CH Offset: 0xc) PWMC Channel Counter Register --------
#  define Sf2M3_PWMC_CCNT       ((unsigned int) 0x0 <<  0)	// (PWMC_CH) Channel Counter
// -------- PWMC_CUPDR : (PWMC_CH Offset: 0x10) PWMC Channel Update Register --------
#  define Sf2M3_PWMC_CUPD       ((unsigned int) 0x0 <<  0)	// (PWMC_CH) Channel Update

// *****************************************************************************
//              SOFTWARE API DEFINITION  FOR Pulse Width Modulation Controller Interface
// *****************************************************************************
typedef struct _Sf2M3S_PWMC {
  Sf2M3_REG PWMC_MR;		// PWMC Mode Register
  Sf2M3_REG PWMC_ENA;		// PWMC Enable Register
  Sf2M3_REG PWMC_DIS;		// PWMC Disable Register
  Sf2M3_REG PWMC_SR;		// PWMC Status Register
  Sf2M3_REG PWMC_IER;		// PWMC Interrupt Enable Register
  Sf2M3_REG PWMC_IDR;		// PWMC Interrupt Disable Register
  Sf2M3_REG PWMC_IMR;		// PWMC Interrupt Mask Register
  Sf2M3_REG PWMC_ISR;		// PWMC Interrupt Status Register
  Sf2M3_REG Reserved0[55];	//
  Sf2M3_REG PWMC_VR;		// PWMC Version Register
  Sf2M3_REG Reserved1[64];	//
  Sf2M3S_PWMC_CH PWMC_CH[32];	// PWMC Channel 0
} Sf2M3S_PWMC, *Sf2M3PS_PWMC;

// -------- PWMC_MR : (PWMC Offset: 0x0) PWMC Mode Register --------
#  define Sf2M3_PWMC_DIVA       ((unsigned int) 0xFF <<  0)	// (PWMC) CLKA divide factor.
#  define Sf2M3_PWMC_PREA       ((unsigned int) 0xF <<  8)	// (PWMC) Divider Input Clock Prescaler A
#  define 	Sf2M3_PWMC_PREA_MCK                  ((unsigned int) 0x0 <<  8)	// (PWMC)
#  define Sf2M3_PWMC_DIVB       ((unsigned int) 0xFF << 16)	// (PWMC) CLKB divide factor.
#  define Sf2M3_PWMC_PREB       ((unsigned int) 0xF << 24)	// (PWMC) Divider Input Clock Prescaler B
#  define 	Sf2M3_PWMC_PREB_MCK                  ((unsigned int) 0x0 << 24)	// (PWMC)
// -------- PWMC_ENA : (PWMC Offset: 0x4) PWMC Enable Register --------
#  define Sf2M3_PWMC_CHID0      ((unsigned int) 0x1 <<  0)	// (PWMC) Channel ID 0
#  define Sf2M3_PWMC_CHID1      ((unsigned int) 0x1 <<  1)	// (PWMC) Channel ID 1
#  define Sf2M3_PWMC_CHID2      ((unsigned int) 0x1 <<  2)	// (PWMC) Channel ID 2
#  define Sf2M3_PWMC_CHID3      ((unsigned int) 0x1 <<  3)	// (PWMC) Channel ID 3
#  define Sf2M3_PWMC_CHID4      ((unsigned int) 0x1 <<  4)	// (PWMC) Channel ID 4
#  define Sf2M3_PWMC_CHID5      ((unsigned int) 0x1 <<  5)	// (PWMC) Channel ID 5
#  define Sf2M3_PWMC_CHID6      ((unsigned int) 0x1 <<  6)	// (PWMC) Channel ID 6
#  define Sf2M3_PWMC_CHID7      ((unsigned int) 0x1 <<  7)	// (PWMC) Channel ID 7
// -------- PWMC_DIS : (PWMC Offset: 0x8) PWMC Disable Register --------
// -------- PWMC_SR : (PWMC Offset: 0xc) PWMC Status Register --------
// -------- PWMC_IER : (PWMC Offset: 0x10) PWMC Interrupt Enable Register --------
// -------- PWMC_IDR : (PWMC Offset: 0x14) PWMC Interrupt Disable Register --------
// -------- PWMC_IMR : (PWMC Offset: 0x18) PWMC Interrupt Mask Register --------
// -------- PWMC_ISR : (PWMC Offset: 0x1c) PWMC Interrupt Status Register --------

// *****************************************************************************
//              SOFTWARE API DEFINITION  FOR USB Device Interface
// *****************************************************************************
typedef struct _Sf2M3S_UDP {
  Sf2M3_REG UDP_NUM;		// Frame Number Register
  Sf2M3_REG UDP_GLBSTATE;	// Global State Register
  Sf2M3_REG UDP_FADDR;		// Function Address Register
  Sf2M3_REG Reserved0[1];	//
  Sf2M3_REG UDP_IER;		// Interrupt Enable Register
  Sf2M3_REG UDP_IDR;		// Interrupt Disable Register
  Sf2M3_REG UDP_IMR;		// Interrupt Mask Register
  Sf2M3_REG UDP_ISR;		// Interrupt Status Register
  Sf2M3_REG UDP_ICR;		// Interrupt Clear Register
  Sf2M3_REG Reserved1[1];	//
  Sf2M3_REG UDP_RSTEP;		// Reset Endpoint Register
  Sf2M3_REG Reserved2[1];	//
  Sf2M3_REG UDP_CSR[8];		// Endpoint Control and Status Register
  Sf2M3_REG UDP_FDR[8];		// Endpoint FIFO Data Register
} Sf2M3S_UDP, *Sf2M3PS_UDP;

// -------- UDP_FRM_NUM : (UDP Offset: 0x0) USB Frame Number Register --------
#  define Sf2M3_UDP_FRM_NUM     ((unsigned int) 0x7FF <<  0)	// (UDP) Frame Number as Defined in the Packet Field Formats
#  define Sf2M3_UDP_FRM_ERR     ((unsigned int) 0x1 << 16)	// (UDP) Frame Error
#  define Sf2M3_UDP_FRM_OK      ((unsigned int) 0x1 << 17)	// (UDP) Frame OK
// -------- UDP_GLB_STATE : (UDP Offset: 0x4) USB Global State Register --------
#  define Sf2M3_UDP_FADDEN      ((unsigned int) 0x1 <<  0)	// (UDP) Function Address Enable
#  define Sf2M3_UDP_CONFG       ((unsigned int) 0x1 <<  1)	// (UDP) Configured
#  define Sf2M3_UDP_RMWUPE      ((unsigned int) 0x1 <<  2)	// (UDP) Remote Wake Up Enable
#  define Sf2M3_UDP_RSMINPR     ((unsigned int) 0x1 <<  3)	// (UDP) A Resume Has Been Sent to the Host
// -------- UDP_FADDR : (UDP Offset: 0x8) USB Function Address Register --------
#  define Sf2M3_UDP_FADD        ((unsigned int) 0xFF <<  0)	// (UDP) Function Address Value
#  define Sf2M3_UDP_FEN         ((unsigned int) 0x1 <<  8)	// (UDP) Function Enable
// -------- UDP_IER : (UDP Offset: 0x10) USB Interrupt Enable Register --------
#  define Sf2M3_UDP_EPINT0      ((unsigned int) 0x1 <<  0)	// (UDP) Endpoint 0 Interrupt
#  define Sf2M3_UDP_EPINT1      ((unsigned int) 0x1 <<  1)	// (UDP) Endpoint 0 Interrupt
#  define Sf2M3_UDP_EPINT2      ((unsigned int) 0x1 <<  2)	// (UDP) Endpoint 2 Interrupt
#  define Sf2M3_UDP_EPINT3      ((unsigned int) 0x1 <<  3)	// (UDP) Endpoint 3 Interrupt
#  define Sf2M3_UDP_EPINT4      ((unsigned int) 0x1 <<  4)	// (UDP) Endpoint 4 Interrupt
#  define Sf2M3_UDP_EPINT5      ((unsigned int) 0x1 <<  5)	// (UDP) Endpoint 5 Interrupt
#  define Sf2M3_UDP_EPINT6      ((unsigned int) 0x1 <<  6)	// (UDP) Endpoint 6 Interrupt
#  define Sf2M3_UDP_EPINT7      ((unsigned int) 0x1 <<  7)	// (UDP) Endpoint 7 Interrupt
#  define Sf2M3_UDP_RXSUSP      ((unsigned int) 0x1 <<  8)	// (UDP) USB Suspend Interrupt
#  define Sf2M3_UDP_RXRSM       ((unsigned int) 0x1 <<  9)	// (UDP) USB Resume Interrupt
#  define Sf2M3_UDP_EXTRSM      ((unsigned int) 0x1 << 10)	// (UDP) USB External Resume Interrupt
#  define Sf2M3_UDP_SOFINT      ((unsigned int) 0x1 << 11)	// (UDP) USB Start Of frame Interrupt
#  define Sf2M3_UDP_WAKEUP      ((unsigned int) 0x1 << 13)	// (UDP) USB Resume Interrupt
// -------- UDP_IDR : (UDP Offset: 0x14) USB Interrupt Disable Register --------
// -------- UDP_IMR : (UDP Offset: 0x18) USB Interrupt Mask Register --------
// -------- UDP_ISR : (UDP Offset: 0x1c) USB Interrupt Status Register --------
#  define Sf2M3_UDP_ENDBUSRES   ((unsigned int) 0x1 << 12)	// (UDP) USB End Of Bus Reset Interrupt
// -------- UDP_ICR : (UDP Offset: 0x20) USB Interrupt Clear Register --------
// -------- UDP_RST_EP : (UDP Offset: 0x28) USB Reset Endpoint Register --------
#  define Sf2M3_UDP_EP0         ((unsigned int) 0x1 <<  0)	// (UDP) Reset Endpoint 0
#  define Sf2M3_UDP_EP1         ((unsigned int) 0x1 <<  1)	// (UDP) Reset Endpoint 1
#  define Sf2M3_UDP_EP2         ((unsigned int) 0x1 <<  2)	// (UDP) Reset Endpoint 2
#  define Sf2M3_UDP_EP3         ((unsigned int) 0x1 <<  3)	// (UDP) Reset Endpoint 3
#  define Sf2M3_UDP_EP4         ((unsigned int) 0x1 <<  4)	// (UDP) Reset Endpoint 4
#  define Sf2M3_UDP_EP5         ((unsigned int) 0x1 <<  5)	// (UDP) Reset Endpoint 5
#  define Sf2M3_UDP_EP6         ((unsigned int) 0x1 <<  6)	// (UDP) Reset Endpoint 6
#  define Sf2M3_UDP_EP7         ((unsigned int) 0x1 <<  7)	// (UDP) Reset Endpoint 7
// -------- UDP_CSR : (UDP Offset: 0x30) USB Endpoint Control and Status Register --------
#  define Sf2M3_UDP_TXCOMP      ((unsigned int) 0x1 <<  0)	// (UDP) Generates an IN packet with data previously written in the DPR
#  define Sf2M3_UDP_RX_DATA_BK0 ((unsigned int) 0x1 <<  1)	// (UDP) Receive Data Bank 0
#  define Sf2M3_UDP_RXSETUP     ((unsigned int) 0x1 <<  2)	// (UDP) Sends STALL to the Host (Control endpoints)
#  define Sf2M3_UDP_ISOERROR    ((unsigned int) 0x1 <<  3)	// (UDP) Isochronous error (Isochronous endpoints)
#  define Sf2M3_UDP_TXPKTRDY    ((unsigned int) 0x1 <<  4)	// (UDP) Transmit Packet Ready
#  define Sf2M3_UDP_FORCESTALL  ((unsigned int) 0x1 <<  5)	// (UDP) Force Stall (used by Control, Bulk and Isochronous endpoints).
#  define Sf2M3_UDP_RX_DATA_BK1 ((unsigned int) 0x1 <<  6)	// (UDP) Receive Data Bank 1 (only used by endpoints with ping-pong attributes).
#  define Sf2M3_UDP_DIR         ((unsigned int) 0x1 <<  7)	// (UDP) Transfer Direction
#  define Sf2M3_UDP_EPTYPE      ((unsigned int) 0x7 <<  8)	// (UDP) Endpoint type
#  define 	Sf2M3_UDP_EPTYPE_CTRL                 ((unsigned int) 0x0 <<  8)	// (UDP) Control
#  define 	Sf2M3_UDP_EPTYPE_ISO_OUT              ((unsigned int) 0x1 <<  8)	// (UDP) Isochronous OUT
#  define 	Sf2M3_UDP_EPTYPE_BULK_OUT             ((unsigned int) 0x2 <<  8)	// (UDP) Bulk OUT
#  define 	Sf2M3_UDP_EPTYPE_INT_OUT              ((unsigned int) 0x3 <<  8)	// (UDP) Interrupt OUT
#  define 	Sf2M3_UDP_EPTYPE_ISO_IN               ((unsigned int) 0x5 <<  8)	// (UDP) Isochronous IN
#  define 	Sf2M3_UDP_EPTYPE_BULK_IN              ((unsigned int) 0x6 <<  8)	// (UDP) Bulk IN
#  define 	Sf2M3_UDP_EPTYPE_INT_IN               ((unsigned int) 0x7 <<  8)	// (UDP) Interrupt IN
#  define Sf2M3_UDP_DTGLE       ((unsigned int) 0x1 << 11)	// (UDP) Data Toggle
#  define Sf2M3_UDP_EPEDS       ((unsigned int) 0x1 << 15)	// (UDP) Endpoint Enable Disable
#  define Sf2M3_UDP_RXBYTECNT   ((unsigned int) 0x7FF << 16)	// (UDP) Number Of Bytes Available in the FIFO

// *****************************************************************************
//               REGISTER ADDRESS DEFINITION FOR Sf2M3SAM7S64
// *****************************************************************************
// ========== Register definition for SYSC peripheral ==========
#  define Sf2M3_SYSC_SYSC_VREG ((Sf2M3_REG *) 	0xFFFFFD60)	// (SYSC) Voltage Regulator Mode Register
// ========== Register definition for AIC peripheral ==========
#  define Sf2M3_AIC_ICCR  ((Sf2M3_REG *) 	0xFFFFF128)
							// (AIC) Interrupt Clear Command Register
#  define Sf2M3_AIC_IECR  ((Sf2M3_REG *) 	0xFFFFF120)
							// (AIC) Interrupt Enable Command Register
#  define Sf2M3_AIC_SMR   ((Sf2M3_REG *) 	0xFFFFF000)
							// (AIC) Source Mode Register
#  define Sf2M3_AIC_ISCR  ((Sf2M3_REG *) 	0xFFFFF12C)
							// (AIC) Interrupt Set Command Register
#  define Sf2M3_AIC_EOICR ((Sf2M3_REG *) 	0xFFFFF130)
							// (AIC) End of Interrupt Command Register
#  define Sf2M3_AIC_DCR   ((Sf2M3_REG *) 	0xFFFFF138)
							// (AIC) Debug Control Register (Protect)
#  define Sf2M3_AIC_FFER  ((Sf2M3_REG *) 	0xFFFFF140)
							// (AIC) Fast Forcing Enable Register
#  define Sf2M3_AIC_SVR   ((Sf2M3_REG *) 	0xFFFFF080)
							// (AIC) Source Vector Register
#  define Sf2M3_AIC_SPU   ((Sf2M3_REG *) 	0xFFFFF134)
							// (AIC) Spurious Vector Register
#  define Sf2M3_AIC_FFDR  ((Sf2M3_REG *) 	0xFFFFF144)
							// (AIC) Fast Forcing Disable Register
#  define Sf2M3_AIC_FVR   ((Sf2M3_REG *) 	0xFFFFF104)
							// (AIC) FIQ Vector Register
#  define Sf2M3_AIC_FFSR  ((Sf2M3_REG *) 	0xFFFFF148)
							// (AIC) Fast Forcing Status Register
#  define Sf2M3_AIC_IMR   ((Sf2M3_REG *) 	0xFFFFF110)
							// (AIC) Interrupt Mask Register
#  define Sf2M3_AIC_ISR   ((Sf2M3_REG *) 	0xFFFFF108)
							// (AIC) Interrupt Status Register
#  define Sf2M3_AIC_IVR   ((Sf2M3_REG *) 	0xFFFFF100)
							// (AIC) IRQ Vector Register
#  define Sf2M3_AIC_IDCR  ((Sf2M3_REG *) 	0xFFFFF124)
							// (AIC) Interrupt Disable Command Register
#  define Sf2M3_AIC_CISR  ((Sf2M3_REG *) 	0xFFFFF114)
							// (AIC) Core Interrupt Status Register
#  define Sf2M3_AIC_IPR   ((Sf2M3_REG *) 	0xFFFFF10C)
							// (AIC) Interrupt Pending Register
// ========== Register definition for DBGU peripheral ==========
#  define Sf2M3_DBGU_C2R  ((Sf2M3_REG *) 	0xFFFFF244)
							// (DBGU) Chip ID2 Register
#  define Sf2M3_DBGU_THR  ((Sf2M3_REG *) 	0xFFFFF21C)
							// (DBGU) Transmitter Holding Register
#  define Sf2M3_DBGU_CSR  ((Sf2M3_REG *) 	0xFFFFF214)
							// (DBGU) Channel Status Register
#  define Sf2M3_DBGU_IDR  ((Sf2M3_REG *) 	0xFFFFF20C)
							// (DBGU) Interrupt Disable Register
#  define Sf2M3_DBGU_MR   ((Sf2M3_REG *) 	0xFFFFF204)
							// (DBGU) Mode Register
#  define Sf2M3_DBGU_FNTR ((Sf2M3_REG *) 	0xFFFFF248)
							// (DBGU) Force NTRST Register
#  define Sf2M3_DBGU_C1R  ((Sf2M3_REG *) 	0xFFFFF240)
							// (DBGU) Chip ID1 Register
#  define Sf2M3_DBGU_BRGR ((Sf2M3_REG *) 	0xFFFFF220)
							// (DBGU) Baud Rate Generator Register
#  define Sf2M3_DBGU_RHR  ((Sf2M3_REG *) 	0xFFFFF218)
							// (DBGU) Receiver Holding Register
#  define Sf2M3_DBGU_IMR  ((Sf2M3_REG *) 	0xFFFFF210)
							// (DBGU) Interrupt Mask Register
#  define Sf2M3_DBGU_IER  ((Sf2M3_REG *) 	0xFFFFF208)
							// (DBGU) Interrupt Enable Register
#  define Sf2M3_DBGU_CR   ((Sf2M3_REG *) 	0xFFFFF200)
							// (DBGU) Control Register
// ========== Register definition for PDC_DBGU peripheral ==========
#  define Sf2M3_DBGU_TNCR ((Sf2M3_REG *) 	0xFFFFF31C)
							// (PDC_DBGU) Transmit Next Counter Register
#  define Sf2M3_DBGU_RNCR ((Sf2M3_REG *) 	0xFFFFF314)
							// (PDC_DBGU) Receive Next Counter Register
#  define Sf2M3_DBGU_PTCR ((Sf2M3_REG *) 	0xFFFFF320)
							// (PDC_DBGU) PDC Transfer Control Register
#  define Sf2M3_DBGU_PTSR ((Sf2M3_REG *) 	0xFFFFF324)
							// (PDC_DBGU) PDC Transfer Status Register
#  define Sf2M3_DBGU_RCR  ((Sf2M3_REG *) 	0xFFFFF304)
							// (PDC_DBGU) Receive Counter Register
#  define Sf2M3_DBGU_TCR  ((Sf2M3_REG *) 	0xFFFFF30C)
							// (PDC_DBGU) Transmit Counter Register
#  define Sf2M3_DBGU_RPR  ((Sf2M3_REG *) 	0xFFFFF300)
							// (PDC_DBGU) Receive Pointer Register
#  define Sf2M3_DBGU_TPR  ((Sf2M3_REG *) 	0xFFFFF308)
							// (PDC_DBGU) Transmit Pointer Register
#  define Sf2M3_DBGU_RNPR ((Sf2M3_REG *) 	0xFFFFF310)
							// (PDC_DBGU) Receive Next Pointer Register
#  define Sf2M3_DBGU_TNPR ((Sf2M3_REG *) 	0xFFFFF318)
							// (PDC_DBGU) Transmit Next Pointer Register
// ========== Register definition for PIOA peripheral ==========
#  define Sf2M3_PIOA_IMR  ((Sf2M3_REG *) 	0xFFFFF448)
							// (PIOA) Interrupt Mask Register
#  define Sf2M3_PIOA_IER  ((Sf2M3_REG *) 	0xFFFFF440)
							// (PIOA) Interrupt Enable Register
#  define Sf2M3_PIOA_OWDR ((Sf2M3_REG *) 	0xFFFFF4A4)
							// (PIOA) Output Write Disable Register
#  define Sf2M3_PIOA_ISR  ((Sf2M3_REG *) 	0xFFFFF44C)
							// (PIOA) Interrupt Status Register
#  define Sf2M3_PIOA_PPUDR ((Sf2M3_REG *) 	0xFFFFF460)
							// (PIOA) Pull-up Disable Register
#  define Sf2M3_PIOA_MDSR ((Sf2M3_REG *) 	0xFFFFF458)
							// (PIOA) Multi-driver Status Register
#  define Sf2M3_PIOA_MDER ((Sf2M3_REG *) 	0xFFFFF450)
							// (PIOA) Multi-driver Enable Register
#  define Sf2M3_PIOA_PER  ((Sf2M3_REG *) 	0xFFFFF400)
							// (PIOA) PIO Enable Register
#  define Sf2M3_PIOA_PSR  ((Sf2M3_REG *) 	0xFFFFF408)
							// (PIOA) PIO Status Register
#  define Sf2M3_PIOA_OER  ((Sf2M3_REG *) 	0xFFFFF410)
							// (PIOA) Output Enable Register
#  define Sf2M3_PIOA_BSR  ((Sf2M3_REG *) 	0xFFFFF474)
							// (PIOA) Select B Register
#  define Sf2M3_PIOA_PPUER ((Sf2M3_REG *) 	0xFFFFF464)
							// (PIOA) Pull-up Enable Register
#  define Sf2M3_PIOA_MDDR ((Sf2M3_REG *) 	0xFFFFF454)
							// (PIOA) Multi-driver Disable Register
#  define Sf2M3_PIOA_PDR  ((Sf2M3_REG *) 	0xFFFFF404)
							// (PIOA) PIO Disable Register
#  define Sf2M3_PIOA_ODR  ((Sf2M3_REG *) 	0xFFFFF414)
							// (PIOA) Output Disable Register
#  define Sf2M3_PIOA_IFDR ((Sf2M3_REG *) 	0xFFFFF424)
							// (PIOA) Input Filter Disable Register
#  define Sf2M3_PIOA_ABSR ((Sf2M3_REG *) 	0xFFFFF478)
							// (PIOA) AB Select Status Register
#  define Sf2M3_PIOA_ASR  ((Sf2M3_REG *) 	0xFFFFF470)
							// (PIOA) Select A Register
#  define Sf2M3_PIOA_PPUSR ((Sf2M3_REG *) 	0xFFFFF468)
							// (PIOA) Pad Pull-up Status Register
#  define Sf2M3_PIOA_ODSR ((Sf2M3_REG *) 	0xFFFFF438)
							// (PIOA) Output Data Status Register
#  define Sf2M3_PIOA_SODR ((Sf2M3_REG *) 	0xFFFFF430)
							// (PIOA) Set Output Data Register
#  define Sf2M3_PIOA_IFSR ((Sf2M3_REG *) 	0xFFFFF428)
							// (PIOA) Input Filter Status Register
#  define Sf2M3_PIOA_IFER ((Sf2M3_REG *) 	0xFFFFF420)
							// (PIOA) Input Filter Enable Register
#  define Sf2M3_PIOA_OSR  ((Sf2M3_REG *) 	0xFFFFF418)
							// (PIOA) Output Status Register
#  define Sf2M3_PIOA_IDR  ((Sf2M3_REG *) 	0xFFFFF444)
							// (PIOA) Interrupt Disable Register
#  define Sf2M3_PIOA_PDSR ((Sf2M3_REG *) 	0xFFFFF43C)
							// (PIOA) Pin Data Status Register
#  define Sf2M3_PIOA_CODR ((Sf2M3_REG *) 	0xFFFFF434)
							// (PIOA) Clear Output Data Register
#  define Sf2M3_PIOA_OWSR ((Sf2M3_REG *) 	0xFFFFF4A8)
							// (PIOA) Output Write Status Register
#  define Sf2M3_PIOA_OWER ((Sf2M3_REG *) 	0xFFFFF4A0)
							// (PIOA) Output Write Enable Register
// ========== Register definition for CKGR peripheral ==========
#  define Sf2M3_CKGR_PLLR ((Sf2M3_REG *) 	0xFFFFFC2C)
							// (CKGR) PLL Register
#  define Sf2M3_CKGR_MCFR ((Sf2M3_REG *) 	0xFFFFFC24)
							// (CKGR) Main Clock  Frequency Register
#  define Sf2M3_CKGR_MOR  ((Sf2M3_REG *) 	0xFFFFFC20)
							// (CKGR) Main Oscillator Register
// ========== Register definition for PMC peripheral ==========
#  define Sf2M3_PMC_SCSR  ((Sf2M3_REG *) 	0xFFFFFC08)
							// (PMC) System Clock Status Register
#  define Sf2M3_PMC_SCER  ((Sf2M3_REG *) 	0xFFFFFC00)
							// (PMC) System Clock Enable Register
#  define Sf2M3_PMC_IMR   ((Sf2M3_REG *) 	0xFFFFFC6C)
							// (PMC) Interrupt Mask Register
#  define Sf2M3_PMC_IDR   ((Sf2M3_REG *) 	0xFFFFFC64)
							// (PMC) Interrupt Disable Register
#  define Sf2M3_PMC_PCDR  ((Sf2M3_REG *) 	0xFFFFFC14)
							// (PMC) Peripheral Clock Disable Register
#  define Sf2M3_PMC_SCDR  ((Sf2M3_REG *) 	0xFFFFFC04)
							// (PMC) System Clock Disable Register
#  define Sf2M3_PMC_SR    ((Sf2M3_REG *) 	0xFFFFFC68)
							// (PMC) Status Register
#  define Sf2M3_PMC_IER   ((Sf2M3_REG *) 	0xFFFFFC60)
							// (PMC) Interrupt Enable Register
#  define Sf2M3_PMC_MCKR  ((Sf2M3_REG *) 	0xFFFFFC30)
							// (PMC) Master Clock Register
#  define Sf2M3_PMC_MOR   ((Sf2M3_REG *) 	0xFFFFFC20)
							// (PMC) Main Oscillator Register
#  define Sf2M3_PMC_PCER  ((Sf2M3_REG *) 	0xFFFFFC10)
							// (PMC) Peripheral Clock Enable Register
#  define Sf2M3_PMC_PCSR  ((Sf2M3_REG *) 	0xFFFFFC18)
							// (PMC) Peripheral Clock Status Register
#  define Sf2M3_PMC_PLLR  ((Sf2M3_REG *) 	0xFFFFFC2C)
							// (PMC) PLL Register
#  define Sf2M3_PMC_MCFR  ((Sf2M3_REG *) 	0xFFFFFC24)
							// (PMC) Main Clock  Frequency Register
#  define Sf2M3_PMC_PCKR  ((Sf2M3_REG *) 	0xFFFFFC40)
							// (PMC) Programmable Clock Register
// ========== Register definition for RSTC peripheral ==========
#  define Sf2M3_RSTC_RSR  ((Sf2M3_REG *) 	0xFFFFFD04)
							// (RSTC) Reset Status Register
#  define Sf2M3_RSTC_RMR  ((Sf2M3_REG *) 	0xFFFFFD08)
							// (RSTC) Reset Mode Register
#  define Sf2M3_RSTC_RCR  ((Sf2M3_REG *) 	0xFFFFFD00)
							// (RSTC) Reset Control Register
// ========== Register definition for RTTC peripheral ==========
#  define Sf2M3_RTTC_RTSR ((Sf2M3_REG *) 	0xFFFFFD2C)
							// (RTTC) Real-time Status Register
#  define Sf2M3_RTTC_RTAR ((Sf2M3_REG *) 	0xFFFFFD24)
							// (RTTC) Real-time Alarm Register
#  define Sf2M3_RTTC_RTVR ((Sf2M3_REG *) 	0xFFFFFD28)
							// (RTTC) Real-time Value Register
#  define Sf2M3_RTTC_RTMR ((Sf2M3_REG *) 	0xFFFFFD20)
							// (RTTC) Real-time Mode Register
// ========== Register definition for PITC peripheral ==========
#  define Sf2M3_PITC_PIIR ((Sf2M3_REG *) 	0xFFFFFD3C)
							// (PITC) Period Interval Image Register
#  define Sf2M3_PITC_PISR ((Sf2M3_REG *) 	0xFFFFFD34)
							// (PITC) Period Interval Status Register
#  define Sf2M3_PITC_PIVR ((Sf2M3_REG *) 	0xFFFFFD38)
							// (PITC) Period Interval Value Register
#  define Sf2M3_PITC_PIMR ((Sf2M3_REG *) 	0xFFFFFD30)
							// (PITC) Period Interval Mode Register
// ========== Register definition for WDTC peripheral ==========
#  define Sf2M3_WDTC_WDMR ((Sf2M3_REG *) 	0xFFFFFD44)
							// (WDTC) Watchdog Mode Register
#  define Sf2M3_WDTC_WDSR ((Sf2M3_REG *) 	0xFFFFFD48)
							// (WDTC) Watchdog Status Register
#  define Sf2M3_WDTC_WDCR ((Sf2M3_REG *) 	0xFFFFFD40)
							// (WDTC) Watchdog Control Register
// ========== Register definition for MC peripheral ==========
#  define Sf2M3_MC_FCR    ((Sf2M3_REG *) 	0xFFFFFF64)
							// (MC) MC Flash Command Register
#  define Sf2M3_MC_ASR    ((Sf2M3_REG *) 	0xFFFFFF04)
							// (MC) MC Abort Status Register
#  define Sf2M3_MC_FSR    ((Sf2M3_REG *) 	0xFFFFFF68)
							// (MC) MC Flash Status Register
#  define Sf2M3_MC_FMR    ((Sf2M3_REG *) 	0xFFFFFF60)
							// (MC) MC Flash Mode Register
#  define Sf2M3_MC_AASR   ((Sf2M3_REG *) 	0xFFFFFF08)
							// (MC) MC Abort Address Status Register
#  define Sf2M3_MC_RCR    ((Sf2M3_REG *) 	0xFFFFFF00)
							// (MC) MC Remap Control Register
// ========== Register definition for PDC_SPI peripheral ==========
#  define Sf2M3_SPI_PTCR  ((Sf2M3_REG *) 	0xFFFE0120)
							// (PDC_SPI) PDC Transfer Control Register
#  define Sf2M3_SPI_TNPR  ((Sf2M3_REG *) 	0xFFFE0118)
							// (PDC_SPI) Transmit Next Pointer Register
#  define Sf2M3_SPI_RNPR  ((Sf2M3_REG *) 	0xFFFE0110)
							// (PDC_SPI) Receive Next Pointer Register
#  define Sf2M3_SPI_TPR   ((Sf2M3_REG *) 	0xFFFE0108)
							// (PDC_SPI) Transmit Pointer Register
#  define Sf2M3_SPI_RPR   ((Sf2M3_REG *) 	0xFFFE0100)
							// (PDC_SPI) Receive Pointer Register
#  define Sf2M3_SPI_PTSR  ((Sf2M3_REG *) 	0xFFFE0124)
							// (PDC_SPI) PDC Transfer Status Register
#  define Sf2M3_SPI_TNCR  ((Sf2M3_REG *) 	0xFFFE011C)
							// (PDC_SPI) Transmit Next Counter Register
#  define Sf2M3_SPI_RNCR  ((Sf2M3_REG *) 	0xFFFE0114)
							// (PDC_SPI) Receive Next Counter Register
#  define Sf2M3_SPI_TCR   ((Sf2M3_REG *) 	0xFFFE010C)
							// (PDC_SPI) Transmit Counter Register
#  define Sf2M3_SPI_RCR   ((Sf2M3_REG *) 	0xFFFE0104)
							// (PDC_SPI) Receive Counter Register
// ========== Register definition for SPI peripheral ==========
#  define Sf2M3_SPI_CSR   ((Sf2M3_REG *) 	0xFFFE0030)
							// (SPI) Chip Select Register
#  define Sf2M3_SPI_IDR   ((Sf2M3_REG *) 	0xFFFE0018)
							// (SPI) Interrupt Disable Register
#  define Sf2M3_SPI_SR    ((Sf2M3_REG *) 	0xFFFE0010)
							// (SPI) Status Register
#  define Sf2M3_SPI_RDR   ((Sf2M3_REG *) 	0xFFFE0008)
							// (SPI) Receive Data Register
#  define Sf2M3_SPI_CR    ((Sf2M3_REG *) 	0xFFFE0000)
							// (SPI) Control Register
#  define Sf2M3_SPI_IMR   ((Sf2M3_REG *) 	0xFFFE001C)
							// (SPI) Interrupt Mask Register
#  define Sf2M3_SPI_IER   ((Sf2M3_REG *) 	0xFFFE0014)
							// (SPI) Interrupt Enable Register
#  define Sf2M3_SPI_TDR   ((Sf2M3_REG *) 	0xFFFE000C)
							// (SPI) Transmit Data Register
#  define Sf2M3_SPI_MR    ((Sf2M3_REG *) 	0xFFFE0004)
							// (SPI) Mode Register
// ========== Register definition for PDC_ADC peripheral ==========
#  define Sf2M3_ADC_PTCR  ((Sf2M3_REG *) 	0xFFFD8120)
							// (PDC_ADC) PDC Transfer Control Register
#  define Sf2M3_ADC_TNPR  ((Sf2M3_REG *) 	0xFFFD8118)
							// (PDC_ADC) Transmit Next Pointer Register
#  define Sf2M3_ADC_RNPR  ((Sf2M3_REG *) 	0xFFFD8110)
							// (PDC_ADC) Receive Next Pointer Register
#  define Sf2M3_ADC_TPR   ((Sf2M3_REG *) 	0xFFFD8108)
							// (PDC_ADC) Transmit Pointer Register
#  define Sf2M3_ADC_RPR   ((Sf2M3_REG *) 	0xFFFD8100)
							// (PDC_ADC) Receive Pointer Register
#  define Sf2M3_ADC_PTSR  ((Sf2M3_REG *) 	0xFFFD8124)
							// (PDC_ADC) PDC Transfer Status Register
#  define Sf2M3_ADC_TNCR  ((Sf2M3_REG *) 	0xFFFD811C)
							// (PDC_ADC) Transmit Next Counter Register
#  define Sf2M3_ADC_RNCR  ((Sf2M3_REG *) 	0xFFFD8114)
							// (PDC_ADC) Receive Next Counter Register
#  define Sf2M3_ADC_TCR   ((Sf2M3_REG *) 	0xFFFD810C)
							// (PDC_ADC) Transmit Counter Register
#  define Sf2M3_ADC_RCR   ((Sf2M3_REG *) 	0xFFFD8104)
							// (PDC_ADC) Receive Counter Register
// ========== Register definition for ADC peripheral ==========
#  define Sf2M3_ADC_IMR   ((Sf2M3_REG *) 	0xFFFD802C)
							// (ADC) ADC Interrupt Mask Register
#  define Sf2M3_ADC_CDR4  ((Sf2M3_REG *) 	0xFFFD8040)
							// (ADC) ADC Channel Data Register 4
#  define Sf2M3_ADC_CDR2  ((Sf2M3_REG *) 	0xFFFD8038)
							// (ADC) ADC Channel Data Register 2
#  define Sf2M3_ADC_CDR0  ((Sf2M3_REG *) 	0xFFFD8030)
							// (ADC) ADC Channel Data Register 0
#  define Sf2M3_ADC_CDR7  ((Sf2M3_REG *) 	0xFFFD804C)
							// (ADC) ADC Channel Data Register 7
#  define Sf2M3_ADC_CDR1  ((Sf2M3_REG *) 	0xFFFD8034)
							// (ADC) ADC Channel Data Register 1
#  define Sf2M3_ADC_CDR3  ((Sf2M3_REG *) 	0xFFFD803C)
							// (ADC) ADC Channel Data Register 3
#  define Sf2M3_ADC_CDR5  ((Sf2M3_REG *) 	0xFFFD8044)
							// (ADC) ADC Channel Data Register 5
#  define Sf2M3_ADC_MR    ((Sf2M3_REG *) 	0xFFFD8004)
							// (ADC) ADC Mode Register
#  define Sf2M3_ADC_CDR6  ((Sf2M3_REG *) 	0xFFFD8048)
							// (ADC) ADC Channel Data Register 6
#  define Sf2M3_ADC_CR    ((Sf2M3_REG *) 	0xFFFD8000)
							// (ADC) ADC Control Register
#  define Sf2M3_ADC_CHER  ((Sf2M3_REG *) 	0xFFFD8010)
							// (ADC) ADC Channel Enable Register
#  define Sf2M3_ADC_CHSR  ((Sf2M3_REG *) 	0xFFFD8018)
							// (ADC) ADC Channel Status Register
#  define Sf2M3_ADC_IER   ((Sf2M3_REG *) 	0xFFFD8024)
							// (ADC) ADC Interrupt Enable Register
#  define Sf2M3_ADC_SR    ((Sf2M3_REG *) 	0xFFFD801C)
							// (ADC) ADC Status Register
#  define Sf2M3_ADC_CHDR  ((Sf2M3_REG *) 	0xFFFD8014)
							// (ADC) ADC Channel Disable Register
#  define Sf2M3_ADC_IDR   ((Sf2M3_REG *) 	0xFFFD8028)
							// (ADC) ADC Interrupt Disable Register
#  define Sf2M3_ADC_LCDR  ((Sf2M3_REG *) 	0xFFFD8020)
							// (ADC) ADC Last Converted Data Register
// ========== Register definition for PDC_SSC peripheral ==========
#  define Sf2M3_SSC_PTCR  ((Sf2M3_REG *) 	0xFFFD4120)
							// (PDC_SSC) PDC Transfer Control Register
#  define Sf2M3_SSC_TNPR  ((Sf2M3_REG *) 	0xFFFD4118)
							// (PDC_SSC) Transmit Next Pointer Register
#  define Sf2M3_SSC_RNPR  ((Sf2M3_REG *) 	0xFFFD4110)
							// (PDC_SSC) Receive Next Pointer Register
#  define Sf2M3_SSC_TPR   ((Sf2M3_REG *) 	0xFFFD4108)
							// (PDC_SSC) Transmit Pointer Register
#  define Sf2M3_SSC_RPR   ((Sf2M3_REG *) 	0xFFFD4100)
							// (PDC_SSC) Receive Pointer Register
#  define Sf2M3_SSC_PTSR  ((Sf2M3_REG *) 	0xFFFD4124)
							// (PDC_SSC) PDC Transfer Status Register
#  define Sf2M3_SSC_TNCR  ((Sf2M3_REG *) 	0xFFFD411C)
							// (PDC_SSC) Transmit Next Counter Register
#  define Sf2M3_SSC_RNCR  ((Sf2M3_REG *) 	0xFFFD4114)
							// (PDC_SSC) Receive Next Counter Register
#  define Sf2M3_SSC_TCR   ((Sf2M3_REG *) 	0xFFFD410C)
							// (PDC_SSC) Transmit Counter Register
#  define Sf2M3_SSC_RCR   ((Sf2M3_REG *) 	0xFFFD4104)
							// (PDC_SSC) Receive Counter Register
// ========== Register definition for SSC peripheral ==========
#  define Sf2M3_SSC_RFMR  ((Sf2M3_REG *) 	0xFFFD4014)
							// (SSC) Receive Frame Mode Register
#  define Sf2M3_SSC_CMR   ((Sf2M3_REG *) 	0xFFFD4004)
							// (SSC) Clock Mode Register
#  define Sf2M3_SSC_IDR   ((Sf2M3_REG *) 	0xFFFD4048)
							// (SSC) Interrupt Disable Register
#  define Sf2M3_SSC_SR    ((Sf2M3_REG *) 	0xFFFD4040)
							// (SSC) Status Register
#  define Sf2M3_SSC_RC0R  ((Sf2M3_REG *) 	0xFFFD4038)
							// (SSC) Receive Compare 0 Register
#  define Sf2M3_SSC_RSHR  ((Sf2M3_REG *) 	0xFFFD4030)
							// (SSC) Receive Sync Holding Register
#  define Sf2M3_SSC_RHR   ((Sf2M3_REG *) 	0xFFFD4020)
							// (SSC) Receive Holding Register
#  define Sf2M3_SSC_TCMR  ((Sf2M3_REG *) 	0xFFFD4018)
							// (SSC) Transmit Clock Mode Register
#  define Sf2M3_SSC_RCMR  ((Sf2M3_REG *) 	0xFFFD4010)
							// (SSC) Receive Clock ModeRegister
#  define Sf2M3_SSC_CR    ((Sf2M3_REG *) 	0xFFFD4000)
							// (SSC) Control Register
#  define Sf2M3_SSC_IMR   ((Sf2M3_REG *) 	0xFFFD404C)
							// (SSC) Interrupt Mask Register
#  define Sf2M3_SSC_IER   ((Sf2M3_REG *) 	0xFFFD4044)
							// (SSC) Interrupt Enable Register
#  define Sf2M3_SSC_RC1R  ((Sf2M3_REG *) 	0xFFFD403C)
							// (SSC) Receive Compare 1 Register
#  define Sf2M3_SSC_TSHR  ((Sf2M3_REG *) 	0xFFFD4034)
							// (SSC) Transmit Sync Holding Register
#  define Sf2M3_SSC_THR   ((Sf2M3_REG *) 	0xFFFD4024)
							// (SSC) Transmit Holding Register
#  define Sf2M3_SSC_TFMR  ((Sf2M3_REG *) 	0xFFFD401C)
							// (SSC) Transmit Frame Mode Register
// ========== Register definition for PDC_US1 peripheral ==========
#  define Sf2M3_US1_PTSR  ((Sf2M3_REG *) 	0xFFFC4124)
							// (PDC_US1) PDC Transfer Status Register
#  define Sf2M3_US1_TNCR  ((Sf2M3_REG *) 	0xFFFC411C)
							// (PDC_US1) Transmit Next Counter Register
#  define Sf2M3_US1_RNCR  ((Sf2M3_REG *) 	0xFFFC4114)
							// (PDC_US1) Receive Next Counter Register
#  define Sf2M3_US1_TCR   ((Sf2M3_REG *) 	0xFFFC410C)
							// (PDC_US1) Transmit Counter Register
#  define Sf2M3_US1_RCR   ((Sf2M3_REG *) 	0xFFFC4104)
							// (PDC_US1) Receive Counter Register
#  define Sf2M3_US1_PTCR  ((Sf2M3_REG *) 	0xFFFC4120)
							// (PDC_US1) PDC Transfer Control Register
#  define Sf2M3_US1_TNPR  ((Sf2M3_REG *) 	0xFFFC4118)
							// (PDC_US1) Transmit Next Pointer Register
#  define Sf2M3_US1_RNPR  ((Sf2M3_REG *) 	0xFFFC4110)
							// (PDC_US1) Receive Next Pointer Register
#  define Sf2M3_US1_TPR   ((Sf2M3_REG *) 	0xFFFC4108)
							// (PDC_US1) Transmit Pointer Register
#  define Sf2M3_US1_RPR   ((Sf2M3_REG *) 	0xFFFC4100)
							// (PDC_US1) Receive Pointer Register
// ========== Register definition for US1 peripheral ==========
#  define Sf2M3_US1_XXR   ((Sf2M3_REG *) 	0xFFFC4048)
							// (US1) XON_XOFF Register
#  define Sf2M3_US1_RHR   ((Sf2M3_REG *) 	0xFFFC4018)
							// (US1) Receiver Holding Register
#  define Sf2M3_US1_IMR   ((Sf2M3_REG *) 	0xFFFC4010)
							// (US1) Interrupt Mask Register
#  define Sf2M3_US1_IER   ((Sf2M3_REG *) 	0xFFFC4008)
							// (US1) Interrupt Enable Register
#  define Sf2M3_US1_CR    ((Sf2M3_REG *) 	0xFFFC4000)
							// (US1) Control Register
#  define Sf2M3_US1_RTOR  ((Sf2M3_REG *) 	0xFFFC4024)
							// (US1) Receiver Time-out Register
#  define Sf2M3_US1_THR   ((Sf2M3_REG *) 	0xFFFC401C)
							// (US1) Transmitter Holding Register
#  define Sf2M3_US1_CSR   ((Sf2M3_REG *) 	0xFFFC4014)
							// (US1) Channel Status Register
#  define Sf2M3_US1_IDR   ((Sf2M3_REG *) 	0xFFFC400C)
							// (US1) Interrupt Disable Register
#  define Sf2M3_US1_FIDI  ((Sf2M3_REG *) 	0xFFFC4040)
							// (US1) FI_DI_Ratio Register
#  define Sf2M3_US1_BRGR  ((Sf2M3_REG *) 	0xFFFC4020)
							// (US1) Baud Rate Generator Register
#  define Sf2M3_US1_TTGR  ((Sf2M3_REG *) 	0xFFFC4028)
							// (US1) Transmitter Time-guard Register
#  define Sf2M3_US1_IF    ((Sf2M3_REG *) 	0xFFFC404C)
							// (US1) IRDA_FILTER Register
#  define Sf2M3_US1_NER   ((Sf2M3_REG *) 	0xFFFC4044)
							// (US1) Nb Errors Register
#  define Sf2M3_US1_MR    ((Sf2M3_REG *) 	0xFFFC4004)
							// (US1) Mode Register
// ========== Register definition for PDC_US0 peripheral ==========
#  define Sf2M3_US0_PTCR  ((Sf2M3_REG *) 	0xFFFC0120)
							// (PDC_US0) PDC Transfer Control Register
#  define Sf2M3_US0_TNPR  ((Sf2M3_REG *) 	0xFFFC0118)
							// (PDC_US0) Transmit Next Pointer Register
#  define Sf2M3_US0_RNPR  ((Sf2M3_REG *) 	0xFFFC0110)
							// (PDC_US0) Receive Next Pointer Register
#  define Sf2M3_US0_TPR   ((Sf2M3_REG *) 	0xFFFC0108)
							// (PDC_US0) Transmit Pointer Register
#  define Sf2M3_US0_RPR   ((Sf2M3_REG *) 	0xFFFC0100)
							// (PDC_US0) Receive Pointer Register
#  define Sf2M3_US0_PTSR  ((Sf2M3_REG *) 	0xFFFC0124)
							// (PDC_US0) PDC Transfer Status Register
#  define Sf2M3_US0_TNCR  ((Sf2M3_REG *) 	0xFFFC011C)
							// (PDC_US0) Transmit Next Counter Register
#  define Sf2M3_US0_RNCR  ((Sf2M3_REG *) 	0xFFFC0114)
							// (PDC_US0) Receive Next Counter Register
#  define Sf2M3_US0_TCR   ((Sf2M3_REG *) 	0xFFFC010C)
							// (PDC_US0) Transmit Counter Register
#  define Sf2M3_US0_RCR   ((Sf2M3_REG *) 	0xFFFC0104)
							// (PDC_US0) Receive Counter Register
// ========== Register definition for US0 peripheral ==========
#  define Sf2M3_US0_TTGR  ((Sf2M3_REG *) 	0xFFFC0028)
							// (US0) Transmitter Time-guard Register
#  define Sf2M3_US0_BRGR  ((Sf2M3_REG *) 	0xFFFC0020)
							// (US0) Baud Rate Generator Register
#  define Sf2M3_US0_RHR   ((Sf2M3_REG *) 	0xFFFC0018)
							// (US0) Receiver Holding Register
#  define Sf2M3_US0_IMR   ((Sf2M3_REG *) 	0xFFFC0010)
							// (US0) Interrupt Mask Register
#  define Sf2M3_US0_NER   ((Sf2M3_REG *) 	0xFFFC0044)
							// (US0) Nb Errors Register
#  define Sf2M3_US0_RTOR  ((Sf2M3_REG *) 	0xFFFC0024)
							// (US0) Receiver Time-out Register
#  define Sf2M3_US0_XXR   ((Sf2M3_REG *) 	0xFFFC0048)
							// (US0) XON_XOFF Register
#  define Sf2M3_US0_FIDI  ((Sf2M3_REG *) 	0xFFFC0040)
							// (US0) FI_DI_Ratio Register
#  define Sf2M3_US0_CR    ((Sf2M3_REG *) 	0xFFFC0000)
							// (US0) Control Register
#  define Sf2M3_US0_IER   ((Sf2M3_REG *) 	0xFFFC0008)
							// (US0) Interrupt Enable Register
#  define Sf2M3_US0_IF    ((Sf2M3_REG *) 	0xFFFC004C)
							// (US0) IRDA_FILTER Register
#  define Sf2M3_US0_MR    ((Sf2M3_REG *) 	0xFFFC0004)
							// (US0) Mode Register
#  define Sf2M3_US0_IDR   ((Sf2M3_REG *) 	0xFFFC000C)
							// (US0) Interrupt Disable Register
#  define Sf2M3_US0_CSR   ((Sf2M3_REG *) 	0xFFFC0014)
							// (US0) Channel Status Register
#  define Sf2M3_US0_THR   ((Sf2M3_REG *) 	0xFFFC001C)
							// (US0) Transmitter Holding Register
// ========== Register definition for TWI peripheral ==========
#  define Sf2M3_TWI_RHR   ((Sf2M3_REG *) 	0xFFFB8030)
							// (TWI) Receive Holding Register
#  define Sf2M3_TWI_IDR   ((Sf2M3_REG *) 	0xFFFB8028)
							// (TWI) Interrupt Disable Register
#  define Sf2M3_TWI_SR    ((Sf2M3_REG *) 	0xFFFB8020)
							// (TWI) Status Register
#  define Sf2M3_TWI_CWGR  ((Sf2M3_REG *) 	0xFFFB8010)
							// (TWI) Clock Waveform Generator Register
#  define Sf2M3_TWI_SMR   ((Sf2M3_REG *) 	0xFFFB8008)
							// (TWI) Slave Mode Register
#  define Sf2M3_TWI_CR    ((Sf2M3_REG *) 	0xFFFB8000)
							// (TWI) Control Register
#  define Sf2M3_TWI_THR   ((Sf2M3_REG *) 	0xFFFB8034)
							// (TWI) Transmit Holding Register
#  define Sf2M3_TWI_IMR   ((Sf2M3_REG *) 	0xFFFB802C)
							// (TWI) Interrupt Mask Register
#  define Sf2M3_TWI_IER   ((Sf2M3_REG *) 	0xFFFB8024)
							// (TWI) Interrupt Enable Register
#  define Sf2M3_TWI_IADR  ((Sf2M3_REG *) 	0xFFFB800C)
							// (TWI) Internal Address Register
#  define Sf2M3_TWI_MMR   ((Sf2M3_REG *) 	0xFFFB8004)
							// (TWI) Master Mode Register
// ========== Register definition for TC2 peripheral ==========
#  define Sf2M3_TC2_IMR   ((Sf2M3_REG *) 	0xFFFA00AC)
							// (TC2) Interrupt Mask Register
#  define Sf2M3_TC2_IER   ((Sf2M3_REG *) 	0xFFFA00A4)
							// (TC2) Interrupt Enable Register
#  define Sf2M3_TC2_RC    ((Sf2M3_REG *) 	0xFFFA009C)
							// (TC2) Register C
#  define Sf2M3_TC2_RA    ((Sf2M3_REG *) 	0xFFFA0094)
							// (TC2) Register A
#  define Sf2M3_TC2_CMR   ((Sf2M3_REG *) 	0xFFFA0084)
							// (TC2) Channel Mode Register (Capture Mode / Waveform Mode)
#  define Sf2M3_TC2_IDR   ((Sf2M3_REG *) 	0xFFFA00A8)
							// (TC2) Interrupt Disable Register
#  define Sf2M3_TC2_SR    ((Sf2M3_REG *) 	0xFFFA00A0)
							// (TC2) Status Register
#  define Sf2M3_TC2_RB    ((Sf2M3_REG *) 	0xFFFA0098)
							// (TC2) Register B
#  define Sf2M3_TC2_CV    ((Sf2M3_REG *) 	0xFFFA0090)
							// (TC2) Counter Value
#  define Sf2M3_TC2_CCR   ((Sf2M3_REG *) 	0xFFFA0080)
							// (TC2) Channel Control Register
// ========== Register definition for TC1 peripheral ==========
#  define Sf2M3_TC1_IMR   ((Sf2M3_REG *) 	0xFFFA006C)
							// (TC1) Interrupt Mask Register
#  define Sf2M3_TC1_IER   ((Sf2M3_REG *) 	0xFFFA0064)
							// (TC1) Interrupt Enable Register
#  define Sf2M3_TC1_RC    ((Sf2M3_REG *) 	0xFFFA005C)
							// (TC1) Register C
#  define Sf2M3_TC1_RA    ((Sf2M3_REG *) 	0xFFFA0054)
							// (TC1) Register A
#  define Sf2M3_TC1_CMR   ((Sf2M3_REG *) 	0xFFFA0044)
							// (TC1) Channel Mode Register (Capture Mode / Waveform Mode)
#  define Sf2M3_TC1_IDR   ((Sf2M3_REG *) 	0xFFFA0068)
							// (TC1) Interrupt Disable Register
#  define Sf2M3_TC1_SR    ((Sf2M3_REG *) 	0xFFFA0060)
							// (TC1) Status Register
#  define Sf2M3_TC1_RB    ((Sf2M3_REG *) 	0xFFFA0058)
							// (TC1) Register B
#  define Sf2M3_TC1_CV    ((Sf2M3_REG *) 	0xFFFA0050)
							// (TC1) Counter Value
#  define Sf2M3_TC1_CCR   ((Sf2M3_REG *) 	0xFFFA0040)
							// (TC1) Channel Control Register
// ========== Register definition for TC0 peripheral ==========
#  define Sf2M3_TC0_IMR   ((Sf2M3_REG *) 	0xFFFA002C)
							// (TC0) Interrupt Mask Register
#  define Sf2M3_TC0_IER   ((Sf2M3_REG *) 	0xFFFA0024)
							// (TC0) Interrupt Enable Register
#  define Sf2M3_TC0_RC    ((Sf2M3_REG *) 	0xFFFA001C)
							// (TC0) Register C
#  define Sf2M3_TC0_RA    ((Sf2M3_REG *) 	0xFFFA0014)
							// (TC0) Register A
#  define Sf2M3_TC0_CMR   ((Sf2M3_REG *) 	0xFFFA0004)
							// (TC0) Channel Mode Register (Capture Mode / Waveform Mode)
#  define Sf2M3_TC0_IDR   ((Sf2M3_REG *) 	0xFFFA0028)
							// (TC0) Interrupt Disable Register
#  define Sf2M3_TC0_SR    ((Sf2M3_REG *) 	0xFFFA0020)
							// (TC0) Status Register
#  define Sf2M3_TC0_RB    ((Sf2M3_REG *) 	0xFFFA0018)
							// (TC0) Register B
#  define Sf2M3_TC0_CV    ((Sf2M3_REG *) 	0xFFFA0010)
							// (TC0) Counter Value
#  define Sf2M3_TC0_CCR   ((Sf2M3_REG *) 	0xFFFA0000)
							// (TC0) Channel Control Register
// ========== Register definition for TCB peripheral ==========
#  define Sf2M3_TCB_BMR   ((Sf2M3_REG *) 	0xFFFA00C4)
							// (TCB) TC Block Mode Register
#  define Sf2M3_TCB_BCR   ((Sf2M3_REG *) 	0xFFFA00C0)
							// (TCB) TC Block Control Register
// ========== Register definition for PWMC_CH3 peripheral ==========
#  define Sf2M3_CH3_CUPDR ((Sf2M3_REG *) 	0xFFFCC270)
							// (PWMC_CH3) Channel Update Register
#  define Sf2M3_CH3_CPRDR ((Sf2M3_REG *) 	0xFFFCC268)
							// (PWMC_CH3) Channel Period Register
#  define Sf2M3_CH3_CMR   ((Sf2M3_REG *) 	0xFFFCC260)
							// (PWMC_CH3) Channel Mode Register
#  define Sf2M3_CH3_Reserved ((Sf2M3_REG *) 	0xFFFCC274)	// (PWMC_CH3) Reserved
#  define Sf2M3_CH3_CCNTR ((Sf2M3_REG *) 	0xFFFCC26C)
							// (PWMC_CH3) Channel Counter Register
#  define Sf2M3_CH3_CDTYR ((Sf2M3_REG *) 	0xFFFCC264)
							// (PWMC_CH3) Channel Duty Cycle Register
// ========== Register definition for PWMC_CH2 peripheral ==========
#  define Sf2M3_CH2_CUPDR ((Sf2M3_REG *) 	0xFFFCC250)
							// (PWMC_CH2) Channel Update Register
#  define Sf2M3_CH2_CPRDR ((Sf2M3_REG *) 	0xFFFCC248)
							// (PWMC_CH2) Channel Period Register
#  define Sf2M3_CH2_CMR   ((Sf2M3_REG *) 	0xFFFCC240)
							// (PWMC_CH2) Channel Mode Register
#  define Sf2M3_CH2_Reserved ((Sf2M3_REG *) 	0xFFFCC254)	// (PWMC_CH2) Reserved
#  define Sf2M3_CH2_CCNTR ((Sf2M3_REG *) 	0xFFFCC24C)
							// (PWMC_CH2) Channel Counter Register
#  define Sf2M3_CH2_CDTYR ((Sf2M3_REG *) 	0xFFFCC244)
							// (PWMC_CH2) Channel Duty Cycle Register
// ========== Register definition for PWMC_CH1 peripheral ==========
#  define Sf2M3_CH1_CUPDR ((Sf2M3_REG *) 	0xFFFCC230)
							// (PWMC_CH1) Channel Update Register
#  define Sf2M3_CH1_CPRDR ((Sf2M3_REG *) 	0xFFFCC228)
							// (PWMC_CH1) Channel Period Register
#  define Sf2M3_CH1_CMR   ((Sf2M3_REG *) 	0xFFFCC220)
							// (PWMC_CH1) Channel Mode Register
#  define Sf2M3_CH1_Reserved ((Sf2M3_REG *) 	0xFFFCC234)	// (PWMC_CH1) Reserved
#  define Sf2M3_CH1_CCNTR ((Sf2M3_REG *) 	0xFFFCC22C)
							// (PWMC_CH1) Channel Counter Register
#  define Sf2M3_CH1_CDTYR ((Sf2M3_REG *) 	0xFFFCC224)
							// (PWMC_CH1) Channel Duty Cycle Register
// ========== Register definition for PWMC_CH0 peripheral ==========
#  define Sf2M3_CH0_CUPDR ((Sf2M3_REG *) 	0xFFFCC210)
							// (PWMC_CH0) Channel Update Register
#  define Sf2M3_CH0_CPRDR ((Sf2M3_REG *) 	0xFFFCC208)
							// (PWMC_CH0) Channel Period Register
#  define Sf2M3_CH0_CMR   ((Sf2M3_REG *) 	0xFFFCC200)
							// (PWMC_CH0) Channel Mode Register
#  define Sf2M3_CH0_Reserved ((Sf2M3_REG *) 	0xFFFCC214)	// (PWMC_CH0) Reserved
#  define Sf2M3_CH0_CCNTR ((Sf2M3_REG *) 	0xFFFCC20C)
							// (PWMC_CH0) Channel Counter Register
#  define Sf2M3_CH0_CDTYR ((Sf2M3_REG *) 	0xFFFCC204)
							// (PWMC_CH0) Channel Duty Cycle Register
// ========== Register definition for PWMC peripheral ==========
#  define Sf2M3_PWMC_VR   ((Sf2M3_REG *) 	0xFFFCC0FC)
							// (PWMC) PWMC Version Register
#  define Sf2M3_PWMC_ISR  ((Sf2M3_REG *) 	0xFFFCC01C)
							// (PWMC) PWMC Interrupt Status Register
#  define Sf2M3_PWMC_IDR  ((Sf2M3_REG *) 	0xFFFCC014)
							// (PWMC) PWMC Interrupt Disable Register
#  define Sf2M3_PWMC_SR   ((Sf2M3_REG *) 	0xFFFCC00C)
							// (PWMC) PWMC Status Register
#  define Sf2M3_PWMC_ENA  ((Sf2M3_REG *) 	0xFFFCC004)
							// (PWMC) PWMC Enable Register
#  define Sf2M3_PWMC_IMR  ((Sf2M3_REG *) 	0xFFFCC018)
							// (PWMC) PWMC Interrupt Mask Register
#  define Sf2M3_PWMC_MR   ((Sf2M3_REG *) 	0xFFFCC000)
							// (PWMC) PWMC Mode Register
#  define Sf2M3_PWMC_DIS  ((Sf2M3_REG *) 	0xFFFCC008)
							// (PWMC) PWMC Disable Register
#  define Sf2M3_PWMC_IER  ((Sf2M3_REG *) 	0xFFFCC010)
							// (PWMC) PWMC Interrupt Enable Register
// ========== Register definition for UDP peripheral ==========
#  define Sf2M3_UDP_ISR   ((Sf2M3_REG *) 	0xFFFB001C)
							// (UDP) Interrupt Status Register
#  define Sf2M3_UDP_IDR   ((Sf2M3_REG *) 	0xFFFB0014)
							// (UDP) Interrupt Disable Register
#  define Sf2M3_UDP_GLBSTATE ((Sf2M3_REG *) 	0xFFFB0004)	// (UDP) Global State Register
#  define Sf2M3_UDP_FDR   ((Sf2M3_REG *) 	0xFFFB0050)
							// (UDP) Endpoint FIFO Data Register
#  define Sf2M3_UDP_CSR   ((Sf2M3_REG *) 	0xFFFB0030)
							// (UDP) Endpoint Control and Status Register
#  define Sf2M3_UDP_RSTEP ((Sf2M3_REG *) 	0xFFFB0028)
							// (UDP) Reset Endpoint Register
#  define Sf2M3_UDP_ICR   ((Sf2M3_REG *) 	0xFFFB0020)
							// (UDP) Interrupt Clear Register
#  define Sf2M3_UDP_IMR   ((Sf2M3_REG *) 	0xFFFB0018)
							// (UDP) Interrupt Mask Register
#  define Sf2M3_UDP_IER   ((Sf2M3_REG *) 	0xFFFB0010)
							// (UDP) Interrupt Enable Register
#  define Sf2M3_UDP_FADDR ((Sf2M3_REG *) 	0xFFFB0008)
							// (UDP) Function Address Register
#  define Sf2M3_UDP_NUM   ((Sf2M3_REG *) 	0xFFFB0000)
							// (UDP) Frame Number Register

// *****************************************************************************
//               PIO DEFINITIONS FOR Sf2M3SAM7S64
// *****************************************************************************
#  define Sf2M3_PIO_PA0        ((unsigned int) 1 <<  0)	// Pin Controlled by PA0
#  define Sf2M3_PA0_PWM0     ((unsigned int) Sf2M3_PIO_PA0)	//  PWM Channel 0
#  define Sf2M3_PA0_TIOA0    ((unsigned int) Sf2M3_PIO_PA0)	//  Timer Counter 0 Multipurpose Timer I/O Pin A
#  define Sf2M3_PIO_PA1        ((unsigned int) 1 <<  1)	// Pin Controlled by PA1
#  define Sf2M3_PA1_PWM1     ((unsigned int) Sf2M3_PIO_PA1)	//  PWM Channel 1
#  define Sf2M3_PA1_TIOB0    ((unsigned int) Sf2M3_PIO_PA1)	//  Timer Counter 0 Multipurpose Timer I/O Pin B
#  define Sf2M3_PIO_PA10       ((unsigned int) 1 << 10)	// Pin Controlled by PA10
#  define Sf2M3_PA10_DTXD     ((unsigned int) Sf2M3_PIO_PA10)	//  DBGU Debug Transmit Data
#  define Sf2M3_PA10_NPCS2    ((unsigned int) Sf2M3_PIO_PA10)	//  SPI Peripheral Chip Select 2
#  define Sf2M3_PIO_PA11       ((unsigned int) 1 << 11)	// Pin Controlled by PA11
#  define Sf2M3_PA11_NPCS0    ((unsigned int) Sf2M3_PIO_PA11)	//  SPI Peripheral Chip Select 0
#  define Sf2M3_PA11_PWM0     ((unsigned int) Sf2M3_PIO_PA11)	//  PWM Channel 0
#  define Sf2M3_PIO_PA12       ((unsigned int) 1 << 12)	// Pin Controlled by PA12
#  define Sf2M3_PA12_MISO     ((unsigned int) Sf2M3_PIO_PA12)	//  SPI Master In Slave
#  define Sf2M3_PA12_PWM1     ((unsigned int) Sf2M3_PIO_PA12)	//  PWM Channel 1
#  define Sf2M3_PIO_PA13       ((unsigned int) 1 << 13)	// Pin Controlled by PA13
#  define Sf2M3_PA13_MOSI     ((unsigned int) Sf2M3_PIO_PA13)	//  SPI Master Out Slave
#  define Sf2M3_PA13_PWM2     ((unsigned int) Sf2M3_PIO_PA13)	//  PWM Channel 2
#  define Sf2M3_PIO_PA14       ((unsigned int) 1 << 14)	// Pin Controlled by PA14
#  define Sf2M3_PA14_SPCK     ((unsigned int) Sf2M3_PIO_PA14)	//  SPI Serial Clock
#  define Sf2M3_PA14_PWM3     ((unsigned int) Sf2M3_PIO_PA14)	//  PWM Channel 3
#  define Sf2M3_PIO_PA15       ((unsigned int) 1 << 15)	// Pin Controlled by PA15
#  define Sf2M3_PA15_TF       ((unsigned int) Sf2M3_PIO_PA15)	//  SSC Transmit Frame Sync
#  define Sf2M3_PA15_TIOA1    ((unsigned int) Sf2M3_PIO_PA15)	//  Timer Counter 1 Multipurpose Timer I/O Pin A
#  define Sf2M3_PIO_PA16       ((unsigned int) 1 << 16)	// Pin Controlled by PA16
#  define Sf2M3_PA16_TK       ((unsigned int) Sf2M3_PIO_PA16)	//  SSC Transmit Clock
#  define Sf2M3_PA16_TIOB1    ((unsigned int) Sf2M3_PIO_PA16)	//  Timer Counter 1 Multipurpose Timer I/O Pin B
#  define Sf2M3_PIO_PA17       ((unsigned int) 1 << 17)	// Pin Controlled by PA17
#  define Sf2M3_PA17_TD       ((unsigned int) Sf2M3_PIO_PA17)	//  SSC Transmit data
#  define Sf2M3_PA17_PCK1     ((unsigned int) Sf2M3_PIO_PA17)	//  PMC Programmable Clock Output 1
#  define Sf2M3_PIO_PA18       ((unsigned int) 1 << 18)	// Pin Controlled by PA18
#  define Sf2M3_PA18_RD       ((unsigned int) Sf2M3_PIO_PA18)	//  SSC Receive Data
#  define Sf2M3_PA18_PCK2     ((unsigned int) Sf2M3_PIO_PA18)	//  PMC Programmable Clock Output 2
#  define Sf2M3_PIO_PA19       ((unsigned int) 1 << 19)	// Pin Controlled by PA19
#  define Sf2M3_PA19_RK       ((unsigned int) Sf2M3_PIO_PA19)	//  SSC Receive Clock
#  define Sf2M3_PA19_FIQ      ((unsigned int) Sf2M3_PIO_PA19)	//  AIC Fast Interrupt Input
#  define Sf2M3_PIO_PA2        ((unsigned int) 1 <<  2)	// Pin Controlled by PA2
#  define Sf2M3_PA2_PWM2     ((unsigned int) Sf2M3_PIO_PA2)	//  PWM Channel 2
#  define Sf2M3_PA2_SCK0     ((unsigned int) Sf2M3_PIO_PA2)	//  USART 0 Serial Clock
#  define Sf2M3_PIO_PA20       ((unsigned int) 1 << 20)	// Pin Controlled by PA20
#  define Sf2M3_PA20_RF       ((unsigned int) Sf2M3_PIO_PA20)	//  SSC Receive Frame Sync
#  define Sf2M3_PA20_IRQ0     ((unsigned int) Sf2M3_PIO_PA20)	//  External Interrupt 0
#  define Sf2M3_PIO_PA21       ((unsigned int) 1 << 21)	// Pin Controlled by PA21
#  define Sf2M3_PA21_RXD1     ((unsigned int) Sf2M3_PIO_PA21)	//  USART 1 Receive Data
#  define Sf2M3_PA21_PCK1     ((unsigned int) Sf2M3_PIO_PA21)	//  PMC Programmable Clock Output 1
#  define Sf2M3_PIO_PA22       ((unsigned int) 1 << 22)	// Pin Controlled by PA22
#  define Sf2M3_PA22_TXD1     ((unsigned int) Sf2M3_PIO_PA22)	//  USART 1 Transmit Data
#  define Sf2M3_PA22_NPCS3    ((unsigned int) Sf2M3_PIO_PA22)	//  SPI Peripheral Chip Select 3
#  define Sf2M3_PIO_PA23       ((unsigned int) 1 << 23)	// Pin Controlled by PA23
#  define Sf2M3_PA23_SCK1     ((unsigned int) Sf2M3_PIO_PA23)	//  USART 1 Serial Clock
#  define Sf2M3_PA23_PWM0     ((unsigned int) Sf2M3_PIO_PA23)	//  PWM Channel 0
#  define Sf2M3_PIO_PA24       ((unsigned int) 1 << 24)	// Pin Controlled by PA24
#  define Sf2M3_PA24_RTS1     ((unsigned int) Sf2M3_PIO_PA24)	//  USART 1 Ready To Send
#  define Sf2M3_PA24_PWM1     ((unsigned int) Sf2M3_PIO_PA24)	//  PWM Channel 1
#  define Sf2M3_PIO_PA25       ((unsigned int) 1 << 25)	// Pin Controlled by PA25
#  define Sf2M3_PA25_CTS1     ((unsigned int) Sf2M3_PIO_PA25)	//  USART 1 Clear To Send
#  define Sf2M3_PA25_PWM2     ((unsigned int) Sf2M3_PIO_PA25)	//  PWM Channel 2
#  define Sf2M3_PIO_PA26       ((unsigned int) 1 << 26)	// Pin Controlled by PA26
#  define Sf2M3_PA26_DCD1     ((unsigned int) Sf2M3_PIO_PA26)	//  USART 1 Data Carrier Detect
#  define Sf2M3_PA26_TIOA2    ((unsigned int) Sf2M3_PIO_PA26)	//  Timer Counter 2 Multipurpose Timer I/O Pin A
#  define Sf2M3_PIO_PA27       ((unsigned int) 1 << 27)	// Pin Controlled by PA27
#  define Sf2M3_PA27_DTR1     ((unsigned int) Sf2M3_PIO_PA27)	//  USART 1 Data Terminal ready
#  define Sf2M3_PA27_TIOB2    ((unsigned int) Sf2M3_PIO_PA27)	//  Timer Counter 2 Multipurpose Timer I/O Pin B
#  define Sf2M3_PIO_PA28       ((unsigned int) 1 << 28)	// Pin Controlled by PA28
#  define Sf2M3_PA28_DSR1     ((unsigned int) Sf2M3_PIO_PA28)	//  USART 1 Data Set ready
#  define Sf2M3_PA28_TCLK1    ((unsigned int) Sf2M3_PIO_PA28)	//  Timer Counter 1 external clock input
#  define Sf2M3_PIO_PA29       ((unsigned int) 1 << 29)	// Pin Controlled by PA29
#  define Sf2M3_PA29_RI1      ((unsigned int) Sf2M3_PIO_PA29)	//  USART 1 Ring Indicator
#  define Sf2M3_PA29_TCLK2    ((unsigned int) Sf2M3_PIO_PA29)	//  Timer Counter 2 external clock input
#  define Sf2M3_PIO_PA3        ((unsigned int) 1 <<  3)	// Pin Controlled by PA3
#  define Sf2M3_PA3_TWD      ((unsigned int) Sf2M3_PIO_PA3)	//  TWI Two-wire Serial Data
#  define Sf2M3_PA3_NPCS3    ((unsigned int) Sf2M3_PIO_PA3)	//  SPI Peripheral Chip Select 3
#  define Sf2M3_PIO_PA30       ((unsigned int) 1 << 30)	// Pin Controlled by PA30
#  define Sf2M3_PA30_IRQ1     ((unsigned int) Sf2M3_PIO_PA30)	//  External Interrupt 1
#  define Sf2M3_PA30_NPCS2    ((unsigned int) Sf2M3_PIO_PA30)	//  SPI Peripheral Chip Select 2
#  define Sf2M3_PIO_PA31       ((unsigned int) 1 << 31)	// Pin Controlled by PA31
#  define Sf2M3_PA31_NPCS1    ((unsigned int) Sf2M3_PIO_PA31)	//  SPI Peripheral Chip Select 1
#  define Sf2M3_PA31_PCK2     ((unsigned int) Sf2M3_PIO_PA31)	//  PMC Programmable Clock Output 2
#  define Sf2M3_PIO_PA4        ((unsigned int) 1 <<  4)	// Pin Controlled by PA4
#  define Sf2M3_PA4_TWCK     ((unsigned int) Sf2M3_PIO_PA4)	//  TWI Two-wire Serial Clock
#  define Sf2M3_PA4_TCLK0    ((unsigned int) Sf2M3_PIO_PA4)	//  Timer Counter 0 external clock input
#  define Sf2M3_PIO_PA5        ((unsigned int) 1 <<  5)	// Pin Controlled by PA5
#  define Sf2M3_PA5_RXD0     ((unsigned int) Sf2M3_PIO_PA5)	//  USART 0 Receive Data
#  define Sf2M3_PA5_NPCS3    ((unsigned int) Sf2M3_PIO_PA5)	//  SPI Peripheral Chip Select 3
#  define Sf2M3_PIO_PA6        ((unsigned int) 1 <<  6)	// Pin Controlled by PA6
#  define Sf2M3_PA6_TXD0     ((unsigned int) Sf2M3_PIO_PA6)	//  USART 0 Transmit Data
#  define Sf2M3_PA6_PCK0     ((unsigned int) Sf2M3_PIO_PA6)	//  PMC Programmable Clock Output 0
#  define Sf2M3_PIO_PA7        ((unsigned int) 1 <<  7)	// Pin Controlled by PA7
#  define Sf2M3_PA7_RTS0     ((unsigned int) Sf2M3_PIO_PA7)	//  USART 0 Ready To Send
#  define Sf2M3_PA7_PWM3     ((unsigned int) Sf2M3_PIO_PA7)	//  PWM Channel 3
#  define Sf2M3_PIO_PA8        ((unsigned int) 1 <<  8)	// Pin Controlled by PA8
#  define Sf2M3_PA8_CTS0     ((unsigned int) Sf2M3_PIO_PA8)	//  USART 0 Clear To Send
#  define Sf2M3_PA8_ADTRG    ((unsigned int) Sf2M3_PIO_PA8)	//  ADC External Trigger
#  define Sf2M3_PIO_PA9        ((unsigned int) 1 <<  9)	// Pin Controlled by PA9
#  define Sf2M3_PA9_DRXD     ((unsigned int) Sf2M3_PIO_PA9)	//  DBGU Debug Receive Data
#  define Sf2M3_PA9_NPCS1    ((unsigned int) Sf2M3_PIO_PA9)	//  SPI Peripheral Chip Select 1

// *****************************************************************************
//               PERIPHERAL ID DEFINITIONS FOR Sf2M3SAM7S64
// *****************************************************************************
#  define Sf2M3_ID_FIQ    ((unsigned int)  0)	// Advanced Interrupt Controller (FIQ)
#  define Sf2M3_ID_SYS    ((unsigned int)  1)	// System Peripheral
#  define Sf2M3_ID_PIOA   ((unsigned int)  2)	// Parallel IO Controller
#  define Sf2M3_ID_3_Reserved ((unsigned int)  3)
						// Reserved
#  define Sf2M3_ID_ADC    ((unsigned int)  4)	// Analog-to-Digital Converter
#  define Sf2M3_ID_SPI    ((unsigned int)  5)	// Serial Peripheral Interface
#  define Sf2M3_ID_US0    ((unsigned int)  6)	// USART 0
#  define Sf2M3_ID_US1    ((unsigned int)  7)	// USART 1
#  define Sf2M3_ID_SSC    ((unsigned int)  8)	// Serial Synchronous Controller
#  define Sf2M3_ID_TWI    ((unsigned int)  9)	// Two-Wire Interface
#  define Sf2M3_ID_PWMC   ((unsigned int) 10)	// PWM Controller
#  define Sf2M3_ID_UDP    ((unsigned int) 11)	// USB Device Port
#  define Sf2M3_ID_TC0    ((unsigned int) 12)	// Timer Counter 0
#  define Sf2M3_ID_TC1    ((unsigned int) 13)	// Timer Counter 1
#  define Sf2M3_ID_TC2    ((unsigned int) 14)	// Timer Counter 2
#  define Sf2M3_ID_15_Reserved ((unsigned int) 15)	// Reserved
#  define Sf2M3_ID_16_Reserved ((unsigned int) 16)	// Reserved
#  define Sf2M3_ID_17_Reserved ((unsigned int) 17)	// Reserved
#  define Sf2M3_ID_18_Reserved ((unsigned int) 18)	// Reserved
#  define Sf2M3_ID_19_Reserved ((unsigned int) 19)	// Reserved
#  define Sf2M3_ID_20_Reserved ((unsigned int) 20)	// Reserved
#  define Sf2M3_ID_21_Reserved ((unsigned int) 21)	// Reserved
#  define Sf2M3_ID_22_Reserved ((unsigned int) 22)	// Reserved
#  define Sf2M3_ID_23_Reserved ((unsigned int) 23)	// Reserved
#  define Sf2M3_ID_24_Reserved ((unsigned int) 24)	// Reserved
#  define Sf2M3_ID_25_Reserved ((unsigned int) 25)	// Reserved
#  define Sf2M3_ID_26_Reserved ((unsigned int) 26)	// Reserved
#  define Sf2M3_ID_27_Reserved ((unsigned int) 27)	// Reserved
#  define Sf2M3_ID_28_Reserved ((unsigned int) 28)	// Reserved
#  define Sf2M3_ID_29_Reserved ((unsigned int) 29)	// Reserved
#  define Sf2M3_ID_IRQ0   ((unsigned int) 30)	// Advanced Interrupt Controller (IRQ0)
#  define Sf2M3_ID_IRQ1   ((unsigned int) 31)	// Advanced Interrupt Controller (IRQ1)

// *****************************************************************************
//               BASE ADDRESS DEFINITIONS FOR Sf2M3SAM7S64
// *****************************************************************************
#  define Sf2M3_BASE_SYSC      ((Sf2M3PS_SYSC) 	0xFFFFF000)	// (SYSC) Base Address
#  define Sf2M3_BASE_AIC       ((Sf2M3PS_AIC) 	0xFFFFF000)	// (AIC) Base Address
#  define Sf2M3_BASE_DBGU      ((Sf2M3PS_DBGU) 	0xFFFFF200)	// (DBGU) Base Address
#  define Sf2M3_BASE_PDC_DBGU  ((Sf2M3PS_PDC) 	0xFFFFF300)	// (PDC_DBGU) Base Address
#  define Sf2M3_BASE_PIOA      ((Sf2M3PS_PIO) 	0xFFFFF400)	// (PIOA) Base Address
#  define Sf2M3_BASE_CKGR      ((Sf2M3PS_CKGR) 	0xFFFFFC20)	// (CKGR) Base Address
#  define Sf2M3_BASE_PMC       ((Sf2M3PS_PMC) 	0xFFFFFC00)	// (PMC) Base Address
#  define Sf2M3_BASE_RSTC      ((Sf2M3PS_RSTC) 	0xFFFFFD00)	// (RSTC) Base Address
#  define Sf2M3_BASE_RTTC      ((Sf2M3PS_RTTC) 	0xFFFFFD20)	// (RTTC) Base Address
#  define Sf2M3_BASE_PITC      ((Sf2M3PS_PITC) 	0xFFFFFD30)	// (PITC) Base Address
#  define Sf2M3_BASE_WDTC      ((Sf2M3PS_WDTC) 	0xFFFFFD40)	// (WDTC) Base Address
#  define Sf2M3_BASE_MC        ((Sf2M3PS_MC) 	0xFFFFFF00)	// (MC) Base Address
#  define Sf2M3_BASE_PDC_SPI   ((Sf2M3PS_PDC) 	0xFFFE0100)	// (PDC_SPI) Base Address
#  define Sf2M3_BASE_SPI       ((Sf2M3PS_SPI) 	0xFFFE0000)	// (SPI) Base Address
#  define Sf2M3_BASE_PDC_ADC   ((Sf2M3PS_PDC) 	0xFFFD8100)	// (PDC_ADC) Base Address
#  define Sf2M3_BASE_ADC       ((Sf2M3PS_ADC) 	0xFFFD8000)	// (ADC) Base Address
#  define Sf2M3_BASE_PDC_SSC   ((Sf2M3PS_PDC) 	0xFFFD4100)	// (PDC_SSC) Base Address
#  define Sf2M3_BASE_SSC       ((Sf2M3PS_SSC) 	0xFFFD4000)	// (SSC) Base Address
#  define Sf2M3_BASE_PDC_US1   ((Sf2M3PS_PDC) 	0xFFFC4100)	// (PDC_US1) Base Address
#  define Sf2M3_BASE_US1       ((Sf2M3PS_USART) 	0xFFFC4000)	// (US1) Base Address
#  define Sf2M3_BASE_PDC_US0   ((Sf2M3PS_PDC) 	0xFFFC0100)	// (PDC_US0) Base Address
#  define Sf2M3_BASE_US0       ((Sf2M3PS_USART) 	0xFFFC0000)	// (US0) Base Address
#  define Sf2M3_BASE_TWI       ((Sf2M3PS_TWI) 	0xFFFB8000)	// (TWI) Base Address
#  define Sf2M3_BASE_TC2       ((Sf2M3PS_TC) 	0xFFFA0080)	// (TC2) Base Address
#  define Sf2M3_BASE_TC1       ((Sf2M3PS_TC) 	0xFFFA0040)	// (TC1) Base Address
#  define Sf2M3_BASE_TC0       ((Sf2M3PS_TC) 	0xFFFA0000)	// (TC0) Base Address
#  define Sf2M3_BASE_TCB       ((Sf2M3PS_TCB) 	0xFFFA0000)	// (TCB) Base Address
#  define Sf2M3_BASE_PWMC_CH3  ((Sf2M3PS_PWMC_CH) 	0xFFFCC260)
								// (PWMC_CH3) Base Address
#  define Sf2M3_BASE_PWMC_CH2  ((Sf2M3PS_PWMC_CH) 	0xFFFCC240)
								// (PWMC_CH2) Base Address
#  define Sf2M3_BASE_PWMC_CH1  ((Sf2M3PS_PWMC_CH) 	0xFFFCC220)
								// (PWMC_CH1) Base Address
#  define Sf2M3_BASE_PWMC_CH0  ((Sf2M3PS_PWMC_CH) 	0xFFFCC200)
								// (PWMC_CH0) Base Address
#  define Sf2M3_BASE_PWMC      ((Sf2M3PS_PWMC) 	0xFFFCC000)	// (PWMC) Base Address
#  define Sf2M3_BASE_UDP       ((Sf2M3PS_UDP) 	0xFFFB0000)	// (UDP) Base Address

// *****************************************************************************
//               MEMORY MAPPING DEFINITIONS FOR Sf2M3SAM7S64
// *****************************************************************************
#  define Sf2M3_ISRAM	 ((char *) 	0x00200000)	// Internal SRAM base address
#  define Sf2M3_ISRAM_SIZE	 ((unsigned int) 0x00004000)	// Internal SRAM size in byte (16 Kbyte)
#  define Sf2M3_IFLASH	 ((char *) 	0x00100000)	// Internal ROM base address
#  define Sf2M3_IFLASH_SIZE	 ((unsigned int) 0x00010000)	// Internal ROM size in byte (64 Kbyte)

#endif
