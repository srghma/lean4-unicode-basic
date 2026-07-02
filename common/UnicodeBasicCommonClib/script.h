/*
Copyright © 2025 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
*/
#pragma once
#include <stdint.h>
#include <lean/lean.h>

#ifdef __cplusplus
extern "C" {
#endif

LEAN_EXPORT uint32_t unicode_basic_common__script_of_abbrev(b_lean_obj_arg abbr);

LEAN_EXPORT lean_obj_res unicode_basic_common__script_to_abbrev(uint32_t scr);

#ifdef __cplusplus
}
#endif
