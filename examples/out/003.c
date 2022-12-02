#include "runtime.h"
rt_ty_unit *main(rt_ty_unit *);
rt_ty_unit *main(rt_ty_unit *_1) {
  rt_ty_cont *tmp_0 = rt_new_label();
  rt_ty_union *tmp_1;
  if (rt_mark(tmp_0)) {
    tmp_1 = rt_get_data_from_cont(tmp_0);
  } else {
    {
      rt_goto(tmp_0, rt_make_handle(0, (void *[0]){}));
      tmp_1 = rt_unreachable();
    }
  }
  rt_ty_unit *tmp_2;
  switch (rt_extract_tag(tmp_1)) {
  case 3ll: {
    tmp_2 = rt_make_unit();
    break;
  }
  }
  return tmp_2;
}
