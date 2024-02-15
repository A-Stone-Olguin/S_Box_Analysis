/*
    This file is part of the ChipWhisperer Example Targets
    Copyright (C) 2012-2017 NewAE Technology Inc.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "hal.h"
#include <stdint.h>
#include <stdlib.h>
#include <limits.h>
#include "stm32f0/stm32f0xx_hal_gpio.h"
#include <lean/lean.h>

#include "simpleserial.h"

uint8_t get_key(uint8_t* k, uint8_t len)
{
    return 0x00;
}

void short_nops(void) {{
    asm(".rept 1000 ; nop ; .endr");
    }}

const long int x = 2;
const long int y = 1;
const long int z = 4;

const long int scale_val = 1000;

volatile long int A = x * scale_val;
volatile long int B = y * scale_val;
volatile long int C = z * scale_val;



const int DELAY = 300;
// state_one 
// the famous inverse square root approximation
void red_light(long int A) {
    for (int i = 0; i < A; i++) {
        HAL_GPIO_WritePin(GPIOA, GPIO_PIN_2, RESET);
        HAL_Delay(DELAY);
        HAL_GPIO_WritePin(GPIOA, GPIO_PIN_2, SET);
    }
    
}

void yellow_light(long int y) {

    for (int i = 0; i < B; i++) {
        HAL_GPIO_WritePin(GPIOA, GPIO_PIN_4, RESET);
        HAL_Delay(DELAY);
        HAL_GPIO_WritePin(GPIOA, GPIO_PIN_4, SET);
        HAL_GPIO_WritePin(GPIOA, GPIO_PIN_2, RESET);
        HAL_Delay(DELAY);
        HAL_GPIO_WritePin(GPIOA, GPIO_PIN_2, SET);
    }
    
}

void green_light(long int C) {

    for (int i = 0; i < C; i++) {
        HAL_GPIO_WritePin(GPIOA, GPIO_PIN_4, RESET);
        HAL_Delay(DELAY);
        HAL_GPIO_WritePin(GPIOA, GPIO_PIN_4, SET);
    }
    
}

// the fallback is also the default in the switch so that we can attempt
// to gracefully exit if we somehow overwrite the state variable
void fallback(long x) {
    putch('F');
    putch('A');
    putch('I');
    putch('L');
}


#if SS_VER == SS_VER_2_1
uint8_t get_pt(uint8_t cmd, uint8_t scmd, uint8_t len, uint8_t* pt)
#else
uint8_t get_pt(uint8_t* pt, uint8_t len)
#endif

{
	/**********************************
	* Start user-specific code here. */

    trigger_high();


    for(uint16_t i = 0; i < 3; i++ ) {
        green_light(C);
        short_nops();
        yellow_light(B);
        short_nops();
        red_light(A);
        short_nops();
        short_nops();
    }
    HAL_GPIO_WritePin(GPIOA, GPIO_PIN_2, RESET);
    HAL_GPIO_WritePin(GPIOA, GPIO_PIN_4, RESET);

	trigger_low();
	/* End user-specific code here. *
	********************************/
	simpleserial_put('r', 16, pt);
	return 0x00;
}

uint8_t reset(uint8_t* x, uint8_t len)
{
	// Reset key here if needed
	return 0x00;
}


int main(void)
{
    platform_init();
	init_uart();
	trigger_setup();

	simpleserial_init();
	simpleserial_addcmd('p', 16, get_pt);

	simpleserial_addcmd('x', 0, reset);
	while(1)
		simpleserial_get();
}
