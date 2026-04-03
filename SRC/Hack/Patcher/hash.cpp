#include "type.h"
#include "assert.h"

#include "hash_func.h"

// =======================================================================================
// CONSTATS
// =======================================================================================

static const hash_t INIT_HASH = {5381, 5381, 5381, 5381};
static const hash_t MULTIPLIER_HASH = {
    0x123456789ABCDEF0ULL,
    0xFEDCBA9876543210ULL,
    0x0123456789ABCDEFULL,
    0xF0E1D2C3B4A59687ULL };

// =======================================================================================
// CODE
// =======================================================================================

// ---------------------------------------------------------------------------------------------------
/**
 * @brief Compare, are hashes equal
 *
 * @param a First hash
 * @param b Second hash
 * @return int  1 - if equal; 0 - if not equal
 */
int hash_equal(hash_t a, hash_t b) {
    for (int i = 0; i < SIZE_HASH / 8; i++) {
        if (a[i] != b[i]) return 0;
    }
    return 1;
}

// ---------------------------------------------------------------------------------------------------
/**
 * @brief Get the hash object
 *
 * @param buffer Buffer with chars
 * @param size Size of buffer
 * @return hash_t
 */
hash_t get_hash(const char *buffer, int size) {
    assert(buffer);

    hash_t state = INIT_HASH;
    const uint8_t *data = (const uint8_t *)buffer;
    int i = 0;

    for (; i + SIZE_HASH - 1 < size; i += SIZE_HASH) {
        const hash_t block = *( (const hash_t *)  (&(data[i])) );
        state = state * MULTIPLIER_HASH + block;
    }

    const uint64_t prime = 37;
    for (; i < size; i++) {
        state[0] = state[0] * prime + data[i];
        state[1] = state[1] * prime + data[i] * 2;
        state[2] = state[2] * prime + data[i] * 3;
        state[3] = state[3] * prime + data[i] * 4;
    }

    return state;
}
