/* 
 ** Unix-style hassle free memory allocation
 ** via Intuition's Alloc/FreeRemember functions.
 ** Copyright (C) 1998 David Benn
 ** 
 ** This program is free software; you can redistribute it and/or
 ** modify it under the terms of the GNU General Public License
 ** as published by the Free Software Foundation; either version 2
 ** of the License, or (at your option) any later version.
 **
 ** This program is distributed in the hope that it will be useful,
 ** but WITHOUT ANY WARRANTY; without even the implied warranty of
 ** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 ** GNU General Public License for more details.
 **
 ** You should have received a copy of the GNU General Public License
 ** along with this program; if not, write to the Free Software
 ** Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 **
 ** Call alloc/sym_alloc for each memory allocation request.
 **
 ** Call free_alloc/free_sym_alloc once at the end of a program
 ** run or when the symbol table for the current level is finished
 ** with.
 **
 ** This module also contains functions for allocating and freeing 
 ** memory for ACE's assembly code generation.
 **
 ** Author: David J Benn
 **   Date: 30th June 1993, 
 **      1st July 1993, 
 **              16th December 1993,
 **      7th,8th January 1994
 */

#include "acedef.h"
//#include <intuition/intuition.h>
#include <stdlib.h>
#include <string.h>
//#include <clib/intuition_protos.h>

/* external variables */
extern int lev;

struct Remember {
	APTR alloc;
	struct Remember *next;
};

/* local variables */
struct Remember *GenRememberList = NULL;
struct Remember *SymRememberList[2] = { NULL, NULL };

APTR AllocRemember(struct Remember ** RememberKey, ULONG Size, ULONG Flags)
{
	struct Remember *head = *RememberKey;
	struct Remember *add, *cur, *last;
	if (head == NULL) {
		head = malloc(sizeof(struct Remember));
		head->alloc = (APTR) malloc(Size);
		head->next = NULL;
        return head->alloc;
	} else {
		for (cur = head; cur; cur = cur->next)
			last = cur;
		add = malloc(sizeof(struct Remember));
		last->next = add;
		add->alloc = (APTR) malloc(Size);
		add->next = NULL;
        return add->alloc;
	}
}

void FreeRemember(struct Remember **RememberKey, BOOL ReallyForget)
{
	struct Remember *cur, *prev = NULL;

	for (cur = *RememberKey; cur; cur = prev->next) {
		free(cur->alloc);
		prev = cur;
        free(cur);
	}
}

void *alloc(ULONG bytes, ULONG flags)
{
	return ((void *) AllocRemember(&SymRememberList[lev], bytes, flags));
}

void *sym_alloc(ULONG bytes, ULONG flags)
{
	return ((void *) AllocRemember(&SymRememberList[lev], bytes, flags));
}

void free_alloc(void)
{
	/* free all memory allocated by alloc */

	if (GenRememberList != NULL) {
		puts("freeing memory...");
		FreeRemember(&GenRememberList, TRUE);
		GenRememberList = NULL;
	}
}

void free_sym_alloc(void)
{
	/* free all memory allocated by sym_alloc for current level */

	if (SymRememberList[lev] != NULL) {
		FreeRemember(&SymRememberList[lev], TRUE);
		SymRememberList[lev] = NULL;
	}
}

CODE *alloc_code(char *opcode, char *srcopr, char *destopr)
{
	/*
	 ** Allocate memory for a CODE node plus its 
	 ** opcode, srcopr and destopr fields.
	 **
	 ** If an allocation fails, any memory 
	 ** allocated is freed and a NULL CODE 
	 ** pointer is returned.
	 */
	CODE *cnode;
	size_t opcode_size, srcopr_size, destopr_size;

	/* node */
	if ((cnode = (CODE *) malloc(sizeof(CODE))) == NULL)
		return (NULL);

	/* opcode,srcopr,destopr */
	opcode_size = strlen(opcode) + 1;
	srcopr_size = strlen(srcopr) + 1;
	destopr_size = strlen(destopr) + 1;

	if ((cnode->opcode = (char *) malloc(
			(opcode_size + srcopr_size + destopr_size))) == NULL) {
		/* unsuccessful -> free node and abort! */
		free(cnode);
		return (NULL);
	}

	/* set src and dest operand pointers */
	cnode->srcopr = cnode->opcode + opcode_size;
	cnode->destopr = cnode->srcopr + srcopr_size;

	/* return a pointer to the CODE node! */
	return (cnode);
}

void free_code(CODE * cnode)
{
	/* 
	 ** Frees all the memory associated with
	 ** a CODE node, including its members.
	 */

	if (cnode) {
		if (cnode->opcode)
			free(cnode->opcode);
		free(cnode);
	}
}

BOOL alloc_code_members(CODE * cnode, char *opcode, char *srcopr, char *destopr)
{
	/*
	 ** Allocate memory for a CODE node's
	 ** opcode, srcopr and destopr fields.
	 **
	 ** If the allocation fails, a boolean 
	 ** FALSE value is returned.
	 */
	size_t opcode_size, srcopr_size, destopr_size;

	/* is the CODE node non-NULL? */
	if (cnode == NULL)
		return (FALSE);

	/* allocate opcode,srcopr,destopr */
	opcode_size = strlen(opcode) + 1;
	srcopr_size = strlen(srcopr) + 1;
	destopr_size = strlen(destopr) + 1;

	if ((cnode->opcode = (char *) malloc(
			(opcode_size + srcopr_size + destopr_size))) == NULL)
		return (FALSE);

	/* set src and dest operand pointers */
	cnode->srcopr = cnode->opcode + opcode_size;
	cnode->destopr = cnode->srcopr + srcopr_size;

	/* we got this far so return TRUE to indicate success! */
	return (TRUE);
}

void free_code_members(CODE * cnode)
{
	/* 
	 ** Frees all the memory associated with
	 ** a CODE node's members.
	 */

	if (cnode) {
		if (cnode->opcode)
			free(cnode->opcode);
		cnode->opcode = NULL;
		cnode->srcopr = NULL;
		cnode->destopr = NULL;
	}
}
