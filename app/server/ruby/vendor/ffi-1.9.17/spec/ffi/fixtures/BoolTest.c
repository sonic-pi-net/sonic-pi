/*
 * Copyright (c) 2007 Wayne Meissner.
 * Copyright (c) 2009 Aman Gupta.
 *
 * All rights reserved.
 *
 * For licensing, see LICENSE.SPECS
 */

#include <stdbool.h>

bool
bool_return_true()
{
    return true;
}

bool
bool_return_false()
{
    return false;
}

bool
bool_return_val(bool value)
{
    return value;
}

bool
bool_reverse_val(bool value)
{
    return value ? false : true;
}
