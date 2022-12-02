#include "runtime.h"
int main(rt_ty_unit *);
int main_with_tro(rt_ty_unit *);
int main(rt_ty_unit *_1) {
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
          rt_goto(tmp_0, rt_make_handle(2, (void *[2]){1ll, tmp_2}));
          tmp_3 = rt_unreachable();
        }
      }
      int tmp_4;
      switch (rt_extract_tag(tmp_3)) {
      case 3ll: {
        int tmp_5 = rt_extract_field(tmp_3, 0ll);
        tmp_4 = tmp_5;
        break;
      }
      }
      tmp_1 = tmp_4;
    }
  }
  int tmp_6;
  switch (rt_extract_tag(tmp_1)) {
  case 2ll: {
    int tmp_7 = rt_extract_field(tmp_1, 0ll);
    tmp_6 = tmp_7;
    break;
  }
  case 3ll: {
    int tmp_8 = rt_extract_field(tmp_1, 0ll);
    rt_ty_cont *tmp_9 = rt_extract_field(tmp_1, 1ll);
    int tmp_10 = tmp_8;
    rt_goto(tmp_9, rt_make_handle(1, (void *[1]){1}));
    void *tmp_11 = rt_unreachable();
    tmp_6 = tmp_11;
    break;
  }
  }
  return tmp_6;
}
int main_with_tro(rt_ty_unit *_1) {
  rt_ty_cont *tmp_0 = rt_new_label();
  rt_ty_union *tmp_1;
  if (rt_mark(tmp_0)) {
    tmp_1 = rt_get_data_from_cont(tmp_0);
  } else {
    {
      int tmp_2 = 1ll;
      int tmp_3 = 1;
      tmp_1 = tmp_3;
    }
  }
  int tmp_4;
  switch (rt_extract_tag(tmp_1)) {
  case 2ll: {
    int tmp_5 = rt_extract_field(tmp_1, 0ll);
    tmp_4 = tmp_5;
    break;
  }
  case 3ll: {
    int tmp_6 = rt_extract_field(tmp_1, 0ll);
    rt_ty_cont *tmp_7 = rt_extract_field(tmp_1, 1ll);
    int tmp_8 = tmp_6;
    rt_goto(tmp_7, rt_make_handle(1, (void *[1]){1}));
    void *tmp_9 = rt_unreachable();
    tmp_4 = tmp_9;
    break;
  }
  }
  return tmp_4;
}
