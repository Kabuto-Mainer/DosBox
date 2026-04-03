#ifndef HASH_FUNC_H
#define HASH_FUNC_H

#include "type.h"

// ---------------------------------------------------------------------------------------------------
/**
 * @brief Compare, are hashes equal
 *
 * @param a First hash
 * @param b Second hash
 * @return int  1 - if equal; 0 - if not equal
 */
int hash_equal(hash_t a, hash_t b);

// ---------------------------------------------------------------------------------------------------
/**
 * @brief Get the hash object
 *
 * @param buffer Buffer with chars
 * @param size Size of buffer
 * @return hash_t
 */
hash_t get_hash(const char *buffer, int size);



#endif /* HASH_FUNC_H */
