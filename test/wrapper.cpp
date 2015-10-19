#include "pcg_random.hpp"

using pcg_detail::engine;
using pcg_detail::specific_stream;
using pcg_detail::xsh_rr_mixin;
using pcg_detail::rxs_m_xs_mixin;

#define MAKE_CALLS_FOR(variant, result_type, state_type)                       \
    variant *init_##variant(state_type state) { return new variant(state); }     \
                                                                               \
    result_type next_##variant(variant *gen) { return (*gen)(); }              \
                                                                               \
    void free_##variant(variant *gen) { delete gen; }

extern "C" {

typedef engine<uint32_t /* result type */, uint64_t /* state type */,
               xsh_rr_mixin<uint32_t, uint64_t>, true,
               specific_stream<uint64_t>> xsh_rr;

MAKE_CALLS_FOR(xsh_rr, uint32_t, uint64_t)

typedef engine<uint32_t, uint64_t, rxs_m_xs_mixin<uint32_t, uint64_t>, true,
               specific_stream<uint64_t>> rxs_m_xs;

MAKE_CALLS_FOR(rxs_m_xs, uint32_t, uint64_t)
}
