#include "runtime.h"
rt_ty_unit *pinger(rt_ty_cont *);
rt_ty_unit *ponger(rt_ty_cont *);
rt_ty_unit *pinger(rt_ty_cont *hpi) {
  rt_ty_cont *tmp_0 = rt_new_label();
  rt_ty_union *tmp_1;
  if (rt_mark(tmp_0)) {
    tmp_1 = rt_get_data_from_cont(tmp_0);
  } else {
    {
      rt_ty_cont *tmp_2 = rt_new_label();
      rt_ty_union *tmp_3;
      if (rt_mark(tmp_2)) {
        tmp_3 = rt_get_data_from_cont(tmp_2);
      } else {
        {
          rt_goto(hpi, rt_make_handle(3, (void *[3]){hpi, tmp_0, tmp_2}));
          tmp_3 = rt_unreachable();
        }
      }
      rt_ty_unit *tmp_4;
      switch (rt_extract_tag(tmp_3)) {
      case 3ll: {
        rt_ty_cont *tmp_5 = rt_extract_field(tmp_3, 0ll);
        rt_ty_cont *tmp_6 = rt_extract_field(tmp_3, 1ll);
        rt_ty_cont *tmp_7 = rt_extract_field(tmp_3, 2ll);
        tmp_4 = rt_make_unit();
        break;
      }
      }
      tmp_1 = tmp_4;
    }
  }
  rt_ty_unit *tmp_8;
  switch (rt_extract_tag(tmp_1)) {
  case 3ll: {
    rt_ty_cont *tmp_9 = rt_extract_field(tmp_1, 0ll);
    rt_ty_cont *tmp_10 = rt_extract_field(tmp_1, 1ll);
    rt_ty_cont *tmp_11 = rt_extract_field(tmp_1, 2ll);
    rt_ty_unit *tmp_13 = pinger(tmp_9);
    rt_ty_unit *tmp_12 = tmp_13;
    rt_goto(tmp_11, rt_make_handle(3, (void *[3]){tmp_9, tmp_10, tmp_11}));
    void *tmp_14 = rt_unreachable();
    tmp_8 = tmp_14;
    break;
  }
  case 2ll: {
    rt_ty_unit *tmp_15 = rt_extract_field(tmp_1, 0ll);
    tmp_8 = rt_make_unit();
    break;
  }
  }
  return tmp_8;
}
rt_ty_unit *ponger(rt_ty_cont *hpo) {
  rt_ty_cont *tmp_0 = rt_new_label();
  rt_ty_union *tmp_1;
  if (rt_mark(tmp_0)) {
    tmp_1 = rt_get_data_from_cont(tmp_0);
  } else {
    {
      rt_ty_cont *tmp_2 = rt_new_label();
      rt_ty_union *tmp_3;
      if (rt_mark(tmp_2)) {
        tmp_3 = rt_get_data_from_cont(tmp_2);
      } else {
        {
          rt_goto(hpo, rt_make_handle(3, (void *[3]){tmp_0, hpo, tmp_2}));
          tmp_3 = rt_unreachable();
        }
      }
      rt_ty_unit *tmp_4;
      switch (rt_extract_tag(tmp_3)) {
      case 3ll: {
        rt_ty_cont *tmp_5 = rt_extract_field(tmp_3, 0ll);
        rt_ty_cont *tmp_6 = rt_extract_field(tmp_3, 1ll);
        rt_ty_cont *tmp_7 = rt_extract_field(tmp_3, 2ll);
        tmp_4 = rt_make_unit();
        break;
      }
      }
      tmp_1 = tmp_4;
    }
  }
  rt_ty_unit *tmp_8;
  switch (rt_extract_tag(tmp_1)) {
  case 3ll: {
    rt_ty_cont *tmp_9 = rt_extract_field(tmp_1, 0ll);
    rt_ty_cont *tmp_10 = rt_extract_field(tmp_1, 1ll);
    rt_ty_cont *tmp_11 = rt_extract_field(tmp_1, 2ll);
    rt_ty_unit *tmp_13 = pinger(tmp_9);
    rt_ty_unit *tmp_12 = tmp_13;
    rt_goto(tmp_11, rt_make_handle(3, (void *[3]){tmp_9, tmp_10, tmp_11}));
    void *tmp_14 = rt_unreachable();
    tmp_8 = tmp_14;
    break;
  }
  case 2ll: {
    rt_ty_unit *tmp_15 = rt_extract_field(tmp_1, 0ll);
    tmp_8 = rt_make_unit();
    break;
  }
  }
  return tmp_8;
}
