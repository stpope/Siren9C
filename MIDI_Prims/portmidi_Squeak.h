/*
 * portmidi_Squeak.h -- minimal interface to the portmidi library for Siren
 *
 * Made for use with Visualworks Smalltalk DLLCC and Squeak FFI
 * Stephen Pope -- stephen@heaveneverywhere.com -- 2003.07.18 -- updated for Squeak 6 on 2022.12.08
 *
 * The design is based on Siren's earlier (OMS-based) MIDI APIs for use with Smalltalk
 * external primitive calls. The basic functions are initialize/terminate, open/close,
 * and read/poll/write, with special calls for MIDI device and stream management.
 * The driver maintains a list of open streams onto portMIDI interfaces.
 * In-coming controller values can be cached in the driver and bulk-up-loaded to Smalltalk.
 */

#ifndef SQ_PORTMIDI_h			// for modern compilers you can use pragma once
#define SQ_PORTMIDI_h

#include <stdio.h>             // UNIX includes
#include <string.h>            // for memcpy, bzero, etc.
#include "stdlib.h"            // UNIX includes
#include "stdio.h"
#include "string.h"
#include "assert.h"

// Logging macros

//#define DEBUG_SIREN            // verbose debugging to stdout

#ifdef DEBUG_SIREN
    #define DEBUG_P(aaa) printf(aaa)
    #define DEBUG_P2(aaa, bbb) printf(aaa, bbb)
    #define DEBUG_P3(aaa, bbb, ccc) printf(aaa, bbb, ccc)
#else
    #define DEBUG_P(aaa)       // no-op
    #define DEBUG_P2(aaa, bbb)
    #define DEBUG_P3(aaa, bbb, ccc)
#endif

// Function Prototypes - most return 0 or a negative error code

// initialize/terminate portmidi

int32_t pm_initialize(void);
int32_t pm_terminate(void);
int32_t pm_has_error(int32_t which);

// open/close the I/O stream 

int32_t pm_open(int32_t device, int32_t direction);	// open a stream and answer its index in the table
int32_t pm_close(int32_t which);

// devices 

int32_t pm_count_devices(void);		// answer # devices

char * pm_get_name(int32_t which);	// answer string name
int32_t pm_dev_dir(int32_t which);	// answer direction 0 = out, 1 = in

int32_t pm_default_input_device(void);	// answer index of default in or out
int32_t pm_default_output_device(void);

// I/O 

int32_t pm_poll(int32_t which);	    // return 1 if data available
int32_t pm_read(int32_t which);	    // blocking read (will be run in a separate thread)
int32_t pm_get(int32_t which);		// non-blocking read (return nil if no input)

int32_t pm_write_short(int32_t which, int32_t msg);	// 3 bytes encoded into a int32_t
int32_t pm_write_data3(int32_t which, unsigned char d1, unsigned char d2, unsigned char d3);
int32_t pm_write_data2(int32_t which, unsigned char d1, unsigned char d2);
int32_t pm_write_long(int32_t which, int32_t * msg, int32_t length);

// special calls for cached controllers 

int32_t pm_start_controller_cacheing(void);
int32_t pm_stop_controller_cacheing(void);
int32_t pm_read_controllers(int32_t which, int32_t fromController, int32_t toController, int32_t * data);

// test -- play a note on the selected device

int32_t pm_test(int32_t which);

#endif
