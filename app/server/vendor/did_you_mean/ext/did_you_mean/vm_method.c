#include "vm_core.h"

static inline rb_method_entry_t *
lookup_method_table(VALUE klass, ID id)
{
  st_data_t body;
  st_table *m_tbl = RCLASS_M_TBL(klass);
  if (st_lookup(m_tbl, id, &body)) {
    return (rb_method_entry_t *) body;
  }
  else {
    return 0;
  }
}

static inline rb_method_entry_t*
search_method(VALUE klass, ID id, VALUE *defined_class_ptr)
{
  rb_method_entry_t *me;

  for (me = 0; klass; klass = RCLASS_SUPER(klass)) {
    if ((me = lookup_method_table(klass, id)) != 0) break;
  }

  if (defined_class_ptr)
    *defined_class_ptr = klass;
  return me;
}
