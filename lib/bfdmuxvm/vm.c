/**
 * \file
 * \brief Implements a virtual machine for executing compiled intermediate language byte code
 *
 */
/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <sys/endian.h>
#include <bfdmuxvm/vm.h>
#include <bfdmuxtools/bfdmux.h>

#include <barrelfish/barrelfish.h>

/*
 * Take compiled filter and run data through it.
 * Result gets stored in the filterresult_t*
 *
 */
/**
 * \brief Performs recursive execution of a subtree of the filter code
 * @param filter_code Points to the begining of the filter code
 * @param filter_len Specifies the length of the filter code in bytes
 * @param packet_data Points to the packet data to run the filter on
 * @param packet_len Specifies the length of the packet data in bytes
 * @param[out] result_value Return value of the subtree execution
 * @param[in] result_offset Initially specifies the offset of the next byte to be executed in the filter code
 * @param[out] result_offset Specifies the next code byte to be executed, after the entire subtree code was executed
 * @return ERR_OK on success, other error values on failure; see header file for error types.
 */

err_t
calc(uint8_t * filter_code, int filter_len, uint8_t * packet_data,
	 int packet_len, uint64_t * result_value, size_t * result_offset);
err_t
calc(uint8_t * filter_code, int filter_len, uint8_t * packet_data,
	 int packet_len, uint64_t * result_value, size_t * result_offset)
{
	uint64_t        res;
	size_t          start = *result_offset;
	if (start > filter_len){
	    return ERR_UNKNOWN;
        }

	op_t            op = filter_code[*result_offset];
	err_t           err;
	switch (op) {
		/*
		 * ---------------------
		 */
		/*
		 * Single argument calls
		 */
		/*
		 * ---------------------
		 */


		// Immediates
	case OP_INT8:
		*result_value = *(uint8_t *) (filter_code + start + 1);
		*result_offset += 1;
		return ERR_OK;
	case OP_INT16:
		*result_value = *(uint16_t *) (filter_code + start + 1);
		*result_offset += 2;
		return ERR_OK;
	case OP_INT32:
		*result_value = *(uint32_t *) (filter_code + start + 1);
		*result_offset += 4;
		return ERR_OK;
	case OP_INT64:
		*result_value = *(uint64_t *) (filter_code + start + 1);
		*result_offset += 8;
		return ERR_OK;

		/*
		 * -----------------------------------------------------------------------------------
		 */
		/*
		 * Special two argument calls (maybe we can break after evaluating
		 * the first argument)
		 */
		/*
		 * -----------------------------------------------------------------------------------
		 */

		// Logical
		//
		// case OP_AND and OP_OR could be merged toghether
	case OP_AND:				// Has a 32bit argument describing the
		// whole treesize
		*result_offset += 5;
		if ((err =
			 calc(filter_code, filter_len, packet_data, packet_len,
				  result_value, result_offset)) < 0)
			return err;
		// Return false if first tree returned false
		if (!(*result_value)) {
			uint64_t       *subtreesize =
				(uint64_t *) ((filter_code) + start + 1);
			*result_offset = start + 4 + *subtreesize;
			// False is already in result->value
			return ERR_OK;
		}
		// Fetch 2nd argument
		*result_offset += 1;
		if ((err =
			 calc(filter_code, filter_len, packet_data, packet_len,
				  result_value, result_offset)) < 0)
			return err;

		// if (result->value) return true; // Result is already in
		// result->value
		// else return false;
		return ERR_OK;

	case OP_OR:				// Has a 32bit argument describing the
		// whole treesize
		*result_offset += 5;
		if ((err =
			 calc(filter_code, filter_len, packet_data, packet_len,
				  result_value, result_offset)) < 0)
			return err;
		// Return true if first subtree returned true
		if (*result_value) {
			uint64_t       *subtreesize =
				(uint64_t *) ((filter_code) + start + 1);
			*result_offset = start + 4 + *subtreesize;
			// True is already in result->value
			return ERR_OK;
		}
		// Fetch 2nd argument
		*result_offset += 1;
		if ((err =
			 calc(filter_code, filter_len, packet_data, packet_len,
				  result_value, result_offset)) < 0)
			return err;

		// if (result->value) return true; // Result is already in
		// result->value
		// else return false;
		return ERR_OK;




	default:
		/*
		 * -------------------
		 */
		/*
		 * One argument calls
		 */
		/*
		 * -------------------
		 */
		*result_offset += 1;
		if ((err =
			 calc(filter_code, filter_len, packet_data, packet_len,
				  result_value, result_offset)) < 0)
			return err;

		switch (op) {			/* We need only the first argument */
		case OP_NOT:
			if (*result_value) {
				*result_value = 0;
			} else {
				*result_value = 1;
			}
			return ERR_OK;
		case OP_LOAD8:
			// Packet access
			if ((*result_value) >= packet_len)
				return ERR_BAD_ACCESS;	// Access not withing packet
			*result_value = *(uint8_t *) (packet_data + (*result_value));
			return ERR_OK;
		case OP_LOAD16:
			// Packet access
			if ((*result_value) + 1 >= packet_len)
				return ERR_BAD_ACCESS;	// Access not withing packet
			(*result_value) =
				ntohs(*(uint16_t *) (packet_data + (*result_value)));
			return ERR_OK;
		case OP_LOAD32:
			// Packet access
			if ((*result_value) + 3 >= packet_len)
				return ERR_BAD_ACCESS;	// Access not withing packet
			*result_value =
			      ntohl(*(uint32_t *) (packet_data + (*result_value)));
			return ERR_OK;
		case OP_LOAD64:
			// Packet access
			if ((*result_value + 7) >= packet_len)
				return ERR_BAD_ACCESS;	// Access not withing packet
			// If we are on a littleendian machine, translate from network
			//
			//
			// order (bigendian) to hostorder
		        res =
				*(uint64_t *) (packet_data + (*result_value));
			short           word = 0x0001;
			char           *byte = (char *) &word;
			if (byte[0])		// Little endian
				res = be64toh(res);
			(*result_value) = res;
			return ERR_OK;
		}

		/*
		 * -------------------
		 */
		/*
		 * two argument calls
		 */
		/*
		 * -------------------
		 */

		/*
		 * We also need the second argument
		 */
		// Fetch 2nd argument
		res = *result_value;
		*result_offset += 1;
		if ((err =
			 calc(filter_code, filter_len, packet_data, packet_len,
				  result_value, result_offset)) < 0)
			return err;

		switch (op) {
			// Comparision
		case OP_EQUAL:
			if (res == (*result_value))
				*result_value = 1;
			else
				*result_value = 0;
			break;
		case OP_UNEQUAL:
			if (res != *result_value)
				*result_value = 1;
			else
				*result_value = 0;
			break;
		case OP_SGREATER:
			if ((signed) res > (signed) (*result_value))
				*result_value = 1;
			else
				*result_value = 0;
			break;
		case OP_SLESS:
			if ((signed) res < (signed) (*result_value))
				*result_value = 1;
			else
				*result_value = 0;
			break;
		case OP_UGREATER:
			if ((unsigned) res > (unsigned) (*result_value))
				*result_value = 1;
			else
				*result_value = 0;
			break;
		case OP_ULESS:
			if ((unsigned) res < (unsigned) (*result_value))
				*result_value = 1;
			else
				*result_value = 0;
			break;
		case OP_SGREATEREQUAL:
			if ((signed) res >= (signed) (*result_value))
				*result_value = 1;
			else
				*result_value = 0;
			break;
		case OP_SLESSEQUAL:
			if ((signed) res <= (signed) (*result_value))
				*result_value = 1;
			else
				*result_value = 0;
			break;
		case OP_UGREATEREQUAL:
			if ((unsigned) res >= (unsigned) (*result_value))
				*result_value = 1;
			else
				*result_value = 0;
			break;
		case OP_ULESSEQUAL:
			if ((unsigned) res <= (unsigned) (*result_value))
				*result_value = 1;
			else
				*result_value = 0;
			break;

			// Arithmetic
		case OP_ADD:
			(*result_value) += res;
			break;
		case OP_SUB:
			(*result_value) = res - (*result_value);
			break;
		case OP_MULT:
			(*result_value) *= res;
			break;
		case OP_IDIV:
			(*result_value) = res / (*result_value);
			break;
		case OP_MOD:
			(*result_value) = res % (*result_value);
			break;

			// Bitwise
		case OP_BAND:
			(*result_value) &= res;
			break;
		case OP_BOR:
			(*result_value) |= res;
			break;
		case OP_BXOR:
			(*result_value) ^= res;
			break;
			// case OP_BNOT: // This case gets handled on top (one
			// argument call)

			// Error
		default:
			return ERR_BAD_OP;
		}
		return ERR_OK;
		/*
		 * End of default case
		 */
	}
}

/**
 * \brief Executes the specified filter on the given packet.
 * @param filter_code Points to the filters byte code
 * @param filter_len Length of the byte code
 * @param packet_data Points to the packet data to run the filter on
 * @param packet_len Length of packet data in bytes
 * @param[out] error_out Error information upon failure during execution
 * @return true, if the filter executed successfully and the result was not zero. false otherwise.
 */
bool
execute_filter(uint8_t * filter_code, int filter_len,
			   uint8_t * packet_data, int packet_len, int *error_out)
{
	err_t           err;
	uint64_t        result_value;
	size_t          result_offset;
	result_value = 0;
	result_offset = 0;
	err =
		calc(filter_code, filter_len, packet_data, packet_len,
			 &result_value, &result_offset);
	if (error_out) {
		*error_out = err;
	}
	if (err < 0 || !(result_value)) {
		return false;
	} else {
		return true;
	}
}
