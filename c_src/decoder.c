// Copyright 2013 Yuriy Iskra <iskra.yw@gmail.com>

#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <assert.h>
#include "jsonx.h"
#include "jsonx_str.h"

#define  JS_OFFSET (8 * sizeof(ERL_NIF_TERM))

typedef struct{
  ErlNifEnv*    env;
  PrivData      *priv;
  size_t        buf_size;
  unsigned char *buf;
  unsigned char *cur;
  size_t        offset;
  ERL_NIF_TERM  input;
  ERL_NIF_TERM  format;  //struct, eep18, proplist
  ERL_NIF_TERM  number_format;  //float, decimal
  ERL_NIF_TERM  error;
  ERL_NIF_TERM  *stack_top;
  ERL_NIF_TERM  *stack_down;
  DecEntry      *resource;
  int           strict_flag;
} State;

static inline ERL_NIF_TERM parse_json(State* st);
static inline ERL_NIF_TERM parse_array(State* st);
static inline ERL_NIF_TERM parse_object(State* st);
static inline ERL_NIF_TERM parse_object_to_record(State* st);
static inline ERL_NIF_TERM parse_string(State* st);
static inline ERL_NIF_TERM parse_string_as_existing_atom(State* st);
static inline ERL_NIF_TERM parse_number(State* st);
static inline ERL_NIF_TERM parse_true(State* st);
static inline ERL_NIF_TERM parse_false(State* st);
static inline ERL_NIF_TERM parse_null(State* st);

static inline void
grow_stack(State *st){
  size_t new_offset = 4 * st->offset;
  size_t new_size = st->buf_size - st->offset + new_offset;
  unsigned char *new_buf = enif_alloc(new_size);
  unsigned char *new_cur = (new_buf + new_size) - ((st->buf + st->buf_size) - st->cur);
  ERL_NIF_TERM *new_down = (ERL_NIF_TERM*)new_buf; 
  ERL_NIF_TERM *new_top = new_down + (st->stack_top - st->stack_down);
  memcpy(new_cur, st->cur, (st->buf + st->buf_size) - st->cur);
  memcpy(new_down, st->stack_down, (void*)st->stack_top - (void*)st->stack_down);

  enif_free(st->buf);
  st->buf_size = new_size;
  st->offset = new_offset;
  st->cur = new_cur;
  st->buf = new_buf;
  st->stack_down = new_down;
  st->stack_top = new_top;
}

static inline unsigned char
look_ah(State *st){
  while(isspace(*st->cur))
    st->cur++;
  return *(st->cur);
}
static inline void
push_term(State *st, ERL_NIF_TERM val){
  if(((unsigned char*)st->stack_top + sizeof(ERL_NIF_TERM)) > (st->cur)){
    grow_stack(st);
  }
  *(st->stack_top++) = val;
}

static inline void
reserve_stack(State* st, unsigned sz){
  if(((unsigned char*)st->stack_top + sizeof(ERL_NIF_TERM) * sz) > (st->cur)){
    grow_stack(st);
  }
  st->stack_top += sz;
}

static inline ERL_NIF_TERM
parse_array(State* st){
  ERL_NIF_TERM term;

  st->cur++;
  if(look_ah(st) == ']'){
    st->cur++;
    return enif_make_list(st->env, 0);
  }

  size_t stack_off =  st->stack_top - st->stack_down;
  for(;;){
    if((term = parse_json(st))){
      push_term(st, term);
      unsigned char c = look_ah(st);
      st->cur++;
      if(c == ','){
	continue;
      }else if(c == ']'){
	ERL_NIF_TERM *down =  st->stack_down + stack_off;
	term = enif_make_list_from_array(st->env, down, st->stack_top - down);
	st->stack_top = down;
	return term;
      }else{
	st->error = st->priv->am_esyntax;
	return (ERL_NIF_TERM)0;
      }
    }else{
      return (ERL_NIF_TERM)0;
    }
  }
  return enif_make_atom(st->env, "array");
}

static inline ERL_NIF_TERM
parse_object(State* st){
  ERL_NIF_TERM plist;
  ERL_NIF_TERM key, val, pair;
  unsigned char c;

  st->cur++;
  if(look_ah(st) == '}'){
    st->cur++;
    plist = enif_make_list(st->env, 0);
    goto ret;
  }
  size_t stack_off =  st->stack_top - st->stack_down;
  for(;;){
    if(look_ah(st) == '"'){
      if((key = parse_string(st))){
	if(look_ah(st) == ':'){
	  st->cur++;
	  if((val = parse_json(st))){
	    pair = enif_make_tuple2(st->env, key, val);
	    push_term(st, pair);
	    c = look_ah(st);
	    st->cur++;
	    if(c == ','){
	      continue;
	    }else if(c == '}'){
	      ERL_NIF_TERM *down =  st->stack_down + stack_off;
	      plist = enif_make_list_from_array(st->env, down, st->stack_top - down);
	      st->stack_top = down;
	      goto ret;
	    }
	  }
	}
      }
    }
    if(!st->error){
      st->error = st->priv->am_esyntax;
    }
    return (ERL_NIF_TERM)0;
  }
 ret:
  if(st->format == st->priv->am_struct){
    return enif_make_tuple2(st->env, st->priv->am_struct, plist);
  }else if(st->format == st->priv->am_eep18){
    return enif_make_tuple1(st->env, plist);
  }else if(st->format == st->priv->am_proplist){
    return plist;
  }
  assert(0);
}

static inline ERL_NIF_TERM
parse_object_to_record(State* st){
  ERL_NIF_TERM record;
  ERL_NIF_TERM key, val;
  ERL_NIF_TERM *pairs;
  unsigned char c;
  unsigned arity;
  int  record_num,i,k;
  size_t results_off;
  size_t stack_off =  st->stack_top - st->stack_down;
  size_t cur_off =  st->cur - st->buf;
  ERL_NIF_TERM* ukeys =  ukeys_base(st->resource);
  unsigned* keys = keys_base(st->resource, st->resource->ukeys_cnt);
  DecRecord* records =  records_base(st->resource, st->resource->ukeys_cnt, st->resource->keys_cnt);
  long  *masks_base =  bit_mask_base(st->resource, st->resource->ukeys_cnt, st->resource->keys_cnt, st->resource->records_cnt);
  size_t mask_size = BITS_TO_ETERM(st->resource->ukeys_cnt);
  size_t mask_off = stack_off;
  size_t pairs_off = stack_off + mask_size;
  unsigned pair_cnt = 0;
  reserve_stack(st, mask_size); // for mask
  for(i = 0; i < mask_size; i++){
    (st->stack_down + mask_off)[i] = 0;
  }
  st->cur++;
  if(look_ah(st) == '}'){
    st->cur++;
    goto ret;
  }
  for(;;){
    if(look_ah(st) == '"'){
      if((key = parse_string_as_existing_atom(st))){
	//FIXME search in sorted array
	int key_num = -1;
	for(i = 0; i < st->resource->ukeys_cnt; i++){
	  if(enif_is_identical(ukeys[i], key)){
	    key_num = i;
	    break;
	  }
	}
	if(key_num == -1)
	  goto undefrec;
	set_bit(key_num, (long*)st->stack_down + mask_off);
	push_term(st, key);
	if(look_ah(st) == ':'){
	  st->cur++;
	  if((val = parse_json(st))){
	    push_term(st, val);
	    pair_cnt++;
	    c = look_ah(st);
	    st->cur++;
	    if(c == ','){
	      continue;
	    }else if(c == '}'){
	      goto ret;
	    }
	  }
	}
      }else{
	if(st->error){
	  return (ERL_NIF_TERM)0;
	}else{
	  goto undefrec;
	}
      }
    }
    if(!st->error)
      st->error = st->priv->am_esyntax;
    return (ERL_NIF_TERM)0;
  }
 ret:
  record_num = find_mask(st->resource->ukeys_cnt, (long *)(st->stack_down + mask_off),
			 st->resource->records_cnt, masks_base);
  if(record_num < 0)
    goto undefrec;
  results_off =  st->stack_top - st->stack_down;
  push_term(st, records[record_num].tag);
  unsigned pos = records[record_num].keys_off;
  arity = records[record_num].arity;
  if(arity != pair_cnt)
    goto undefrec;
  for(i = 0; i < arity; i++){
    unsigned knum = keys[pos + i];
    for(k = 0; k < arity; k++){
      pairs = st->stack_down + pairs_off;
      if(enif_is_identical(pairs[2 * k], ukeys[knum])){
	push_term(st, pairs[2 * k + 1]);
      }
    }
  }
  record = enif_make_tuple_from_array(st->env, st->stack_down + results_off, arity + 1);
  st->stack_top =  st->stack_down + stack_off;
  return record;
 undefrec:
  if(st->strict_flag){
    st->error = st->priv->am_undefined_record;
    return (ERL_NIF_TERM)0;
  }else{
    st->cur =  st->buf + cur_off;
    st->stack_top =  st->stack_down + stack_off;
    return parse_object(st);
  }
}

static inline ERL_NIF_TERM
parse_string(State* st){
  unsigned char *endptr;
  unsigned char *endstr;
  ERL_NIF_TERM ret;
  if(check_noescaped_jstr(st->cur, &endptr)){
    if(*endptr == '"'){
      ret = enif_make_sub_binary(st->env, st->input, st->cur - (st->buf + st->offset) + 1, endptr - st->cur - 1);
      st->cur = endptr + 1;
      return ret;
    }else if(*endptr == '\\'){
      if(check_with_unescape_jstr(endptr, &endstr, &endptr)){
	unsigned char *dst = enif_make_new_binary(st->env, endstr - st->cur - 1, &ret);
	memcpy(dst, st->cur + 1, endstr - st->cur - 1);
	st->cur = endptr + 1;
	return ret;
      }
    }
  }
  st->error = st->priv->am_estr;
  return (ERL_NIF_TERM)0;
}
static inline ERL_NIF_TERM
parse_string_as_existing_atom(State* st){
  unsigned char *endptr;
  unsigned char *endstr;
  ERL_NIF_TERM atom;
  if(check_noescaped_jstr(st->cur, &endptr)){
    if(*endptr == '"'){
      if(!enif_make_existing_atom_len(st->env, (const char*)(st->cur + 1), endptr - st->cur - 1,  &atom, ERL_NIF_LATIN1)){
	  return (ERL_NIF_TERM)0;
	}
      st->cur = endptr + 1;
      return atom;
    }else if(*endptr == '\\'){
      if(check_with_unescape_jstr(endptr, &endstr, &endptr)){
	if(!enif_make_existing_atom_len(st->env, (const char*)(st->cur + 1), endstr - st->cur - 1,  &atom, ERL_NIF_LATIN1)){
	  return (ERL_NIF_TERM)0;
	}
	st->cur = endptr + 1;
	return atom;
      }
    }
  }
  st->error = st->priv->am_estr;
  return (ERL_NIF_TERM)0;
}

static inline ERL_NIF_TERM
parse_decimal(State *st){
  long long value;
  long long scale;
  long long sign;
  char *endptr;

  char *sub_start;
  long long sub;
  long long i;

  value = strtoll((char *)st->cur, &endptr, 10);
  if (value < 0) {
    sign = 1;
    value = -value;
  } else
    sign = 0;

  if (*endptr == '.') {
    sub_start = endptr + 1;
    if(*sub_start == '+' || *sub_start == '-')
      return (ERL_NIF_TERM)0;

    sub = strtoll(sub_start, &endptr, 10);
    if(sub_start == endptr){
      return (ERL_NIF_TERM)0;
    }else if(errno == ERANGE){
      st->error = st->priv->am_erange;
      return (ERL_NIF_TERM)0;
    }

    scale = sub_start - endptr;
    for (i = scale; i < 0; i++)
      value *= 10;
    value += sub;

    if(value < 0) {
      st->error = st->priv->am_erange;
      return (ERL_NIF_TERM)0;
    }
  } else {
    scale = 0;
  }

  if ((*endptr | 0x20) == 'e') {
    sub_start = endptr + 1;
    sub = strtoll(sub_start, &endptr, 10);
    if(sub_start == endptr){
      return (ERL_NIF_TERM)0;
    }else if(errno == ERANGE){
      st->error = st->priv->am_erange;
      return (ERL_NIF_TERM)0;
    }
    scale += sub;
  }

  st->cur = (unsigned char*)endptr;
  return enif_make_tuple3(
    st->env,
    enif_make_int64(st->env, sign),
    enif_make_int64(st->env, value),
    enif_make_int64(st->env, scale)
  );
}

static inline ERL_NIF_TERM
parse_number(State *st){
  long long int_num;
  double float_num;
  char *endptr;
  errno = 0;

  int_num = strtoll((char *)st->cur, &endptr, 10);
  if((char*)st->cur == endptr){
    return (ERL_NIF_TERM)0;
  }else if(errno == ERANGE){
    st->error = st->priv->am_erange;
    return (ERL_NIF_TERM)0;
  }

  if(*endptr == '.' || *endptr == 'e' || *endptr == 'E'){
    if(st->number_format == st->priv->am_decimal){
      return parse_decimal(st);  
    }
    float_num = strtod((char *)st->cur, &endptr);
    if(errno != ERANGE){
      st->cur = (unsigned char*)endptr;
      return enif_make_double(st->env, float_num);
     }
    else{
      st->error = st->priv->am_erange;
      return (ERL_NIF_TERM)0;
    }
  }
  else{
    st->cur = (unsigned char*)endptr;
    return enif_make_int64(st->env, int_num);
  }
}

static inline ERL_NIF_TERM
parse_true(State* st){
  if(!(strncmp("rue", (char*)(++st->cur), 3))){
    st->cur = st->cur + 3;
    return st->priv->am_true;
  }
  st->error = st->priv->am_esyntax;
  return (ERL_NIF_TERM)0;
}

static inline ERL_NIF_TERM
parse_false(State* st){
  if(!(strncmp("alse", (char*)(++st->cur), 4))){
    st->cur = st->cur + 4;
    return st->priv->am_false;
  }
  st->error = st->priv->am_esyntax;
  return (ERL_NIF_TERM)0;
}

static inline ERL_NIF_TERM
parse_null(State* st){
  if(!(strncmp("ull", (char*)(++st->cur), 3))){
    st->cur = st->cur + 3;
    return st->priv->am_null;
  }
  st->error = st->priv->am_esyntax;
  return (ERL_NIF_TERM)0;
}

static inline ERL_NIF_TERM
parse_json(State *st){
  ERL_NIF_TERM num;
  switch(look_ah(st)){
  case '\"' : return parse_string(st);
  case '{'  : return (st->resource ? parse_object_to_record(st) : parse_object(st));
  case '['  : return parse_array(st);
  case 't'  : return parse_true(st);
  case 'f'  : return parse_false(st);
  case 'n'  : return parse_null(st); 
  default:
    if((num = parse_number(st))){
      return num;
    }
    if(!st->error){
      st->error = st->priv->am_esyntax;
    }
    return (ERL_NIF_TERM)0;
  }
}

ERL_NIF_TERM
decode_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
  ErlNifBinary input;
  if(!enif_inspect_binary(env, argv[0], &input)){
    return enif_make_badarg(env);
  }
  assert(argc == 3 || argc == 5 );
  State st;
  st.priv = (PrivData*)enif_priv_data(env);
  st.resource = NULL;
  st.input = argv[0];
  st.format = argv[1];
  st.number_format = argv[2];
  if (argc == 5){ // whith resource
    assert(enif_get_resource(env, argv[3], st.priv->decoder_RSTYPE, (void**)&st.resource));
    st.strict_flag = enif_is_identical(st.priv->am_true, argv[4]) ? 1 : 0;
  }
  st.offset = JS_OFFSET;
  st.buf_size = st.offset + input.size + 4;
  st.buf = enif_alloc(st.buf_size);
  st.env = env;
  st.stack_top = st.stack_down = (ERL_NIF_TERM*)st.buf;
  st.cur = st.buf + st.offset;
  st.error = (ERL_NIF_TERM)0;
  memcpy(st.cur, input.data, input.size);
  st.buf[st.buf_size - 1] = 0U;
  st.buf[st.buf_size - 2] = 0U;
  st.buf[st.buf_size - 3] = 0U;
  st.buf[st.buf_size - 4] = 0U;

  ERL_NIF_TERM ret = parse_json(&st);
  if(ret){
    if(look_ah(&st) != 0U){
      ret = 0;
      st.error = st.priv->am_etrailing;
    }
  }
  enif_free(st.buf);
  if(!ret){
    return enif_make_tuple3(env, st.priv->am_error, st.error,
			    enif_make_ulong(env, st.cur - (st.buf + st.offset)));
  }
  return ret;
}
