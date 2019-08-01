*"* local class implementation for public class
*"* use this source file for the implementation part of
*"* local helper classes

DEFINE escape_json.
  move &1 to &2.
  replace all occurrences of `\` in &2 with `\\`.
  replace all occurrences of `"` in &2 with `\"`.
END-OF-DEFINITION.

DEFINE dump_type.

  case &2->type_kind.
    when cl_abap_typedescr=>typekind_float or cl_abap_typedescr=>typekind_int or cl_abap_typedescr=>typekind_int1 or
         cl_abap_typedescr=>typekind_int2 or cl_abap_typedescr=>typekind_packed or cl_abap_typedescr=>typekind_num or
         cl_abap_typedescr=>typekind_numeric.
      if &1 is initial.
        &3 = `0`.
      else.
        move &1 to &3.
        condense &3.
      endif.
    when cl_abap_typedescr=>typekind_string or cl_abap_typedescr=>typekind_csequence or cl_abap_typedescr=>typekind_clike.
      if &1 is initial.
        &3 = `""`.
      else.
        escape_json &1 &3.
        concatenate `"` &3 `"` into &3.
      endif.
    when cl_abap_typedescr=>typekind_char.
      if &2->output_length eq 1 and  ( &2->absolute_name eq `\TYPE-POOL=ABAP\TYPE=ABAP_BOOL` or
                                       &2->absolute_name eq `\TYPE=BOOLEAN` or
                                       &2->absolute_name eq `\TYPE=BOOLE_D` ).
        if &1 eq abap_true.
          &3 = `true`.
        else.
          &3 = `false`.
        endif.
      else.
        escape_json &1 &3.
        concatenate `"` &3 `"` into &3.
      endif.
    when cl_abap_typedescr=>typekind_date or cl_abap_typedescr=>typekind_time.
      move &1 to &3.
    when others.
      if &1 is initial.
        &3 = `null`.
      else.
        move &1 to &3.
      endif.
  endcase.
END-OF-DEFINITION.

DEFINE throw_error.
  create object exp.
  raise exception exp.
END-OF-DEFINITION.

DEFINE eat_white.
  while json+offset(1) eq ` `.
    offset = offset + 1.
  endwhile.
END-OF-DEFINITION.

DEFINE eat_string.
  if json+offset(1) eq `"`.
    mark   = offset + 1.
    offset = mark.
    while json+offset(1) ne `"`.
      if json+offset(1) eq `\`.
        offset = offset + 1.
      endif.
      offset = offset + 1.
    endwhile.
    match = offset - mark.
    &1 = json+mark(match).
    offset = offset + 1.
  else.
    throw_error.
  endif.
END-OF-DEFINITION.

DEFINE eat_number.
  mark   = offset.
  while `0123456789-eE.` cs json+offset(1).
    offset = offset + 1.
  endwhile.
  match = offset - mark.
  &1 = json+mark(match).
END-OF-DEFINITION.

DEFINE eat_bool.
  mark   = offset.
  while `aeflnrstu` cs json+offset(1).
    offset = offset + 1.
  endwhile.
  match = offset - mark.
  if json+mark(match) eq `true`.
    &1 = abap_true.
  elseif json+mark(match) eq `false`.
    &1 = abap_false.
  endif.
END-OF-DEFINITION.

DEFINE eat_char.
  if json+offset(1) eq &1.
    offset = offset + 1.
  else.
    throw_error.
  endif.
END-OF-DEFINITION.
