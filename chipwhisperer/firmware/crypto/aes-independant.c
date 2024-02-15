/*
    This file is part of the AESExplorer Example Targets
    Copyright (C) 2012 Colin O'Flynn <coflynn@newae.com>

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

#include "aes-independant.h"
#include "hal.h"


#if defined(TINYAES128C)

#include "aes.h"

uint8_t enckey[16];
struct AES_ctx ctx;

void aes_indep_init(void)
{
	;
}

void aes_indep_key(uint8_t * key)
{
    AES_init_ctx(&ctx, key);
}

void aes_indep_enc(uint8_t * pt)
{
	AES_ECB_encrypt(&ctx, pt);
}

void aes_indep_enc_pretrigger(uint8_t * pt)
{
    ;
}

void aes_indep_enc_posttrigger(uint8_t * pt)
{
    ;
}

void aes_indep_mask(uint8_t * m, uint8_t len)
{
}

#else

#error "No Crypto Lib Defined?"

#endif


