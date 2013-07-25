// Copyright 2013 Yuriy Iskra <iskra.yw@gmail.com>

#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "jsonx.h"
#include "jsonx_str.h"

#define FIRST_BIN_SZ (2 * 1024)

typedef struct state_t{
  PrivData* priv;
  ERL_NIF_TERM no_match;
  ERL_NIF_TERM ret;
  ErlNifBinary bin;
  unsigned char *cur;
  EncEntry *records;
}State;

static inline int match_atom(ErlNifEnv* env, ERL_NIF_TERM term, State *st);
static inline int match_binary(ErlNifEnv* env, ERL_NIF_TERM term, State *st);
static inline int match_int(ErlNifEnv* env, ERL_NIF_TERM term, State *st);
static inline int match_int64(ErlNifEnv* env, ERL_NIF_TERM term, State *st);
static inline int match_double(ErlNifEnv* env, ERL_NIF_TERM term, State *st);
static inline int match_string(ErlNifEnv* env, ERL_NIF_TERM term, State *st);
static inline int match_empty_list(ErlNifEnv* env, ERL_NIF_TERM term, State *st);
static inline int match_pair(ErlNifEnv* env, ERL_NIF_TERM term, State *st);
static inline int match_proplist(ErlNifEnv* env, ERL_NIF_TERM term, State *st);
static inline int match_list(ErlNifEnv* env, ERL_NIF_TERM term, State *st);
static inline int match_json(ErlNifEnv* env, ERL_NIF_TERM term, State *st);
static inline int match_tuple(ErlNifEnv* env, ERL_NIF_TERM term, State *st);
static inline int match_term(ErlNifEnv* env, ERL_NIF_TERM term, State *st);

static void
do_reserve(size_t sz, State *st){
    size_t used = st->cur - st->bin.data;
    size_t t = st->bin.size * 2;
    while( t < used + sz ){
      t *= 2;
    }
    enif_realloc_binary(&st->bin, t);
    st->cur = st->bin.data + used;
}

#define b_reserve(sz, st)    if(sz > st->bin.data + st->bin.size - st->cur){do_reserve(sz, st);}
#define b_putc(c, st)        b_reserve(1, st);*(st->cur++) = c;
#define b_putc2(c1, c2, st)  b_reserve(2, st); st->cur[0]=c1; st->cur[1]=c2; st->cur+=2;
#define b_unputc(st)         st->cur--;
#define b_seek(len, st)      st->cur += (len);
#define b_puts(len, str, st) b_reserve(len, st); memcpy(st->cur, str, len); b_seek(len, st);

static inline int
match_atom(ErlNifEnv* env, ERL_NIF_TERM term, State *st){
  if(!enif_is_atom(env, term)){
    return 0;
  }
  
  if(enif_is_identical(term, st->priv->am_true)){
    b_puts(4, "true", st);
    return 1;
  }else if(enif_is_identical(term, st->priv->am_false)){
    b_puts(5, "false", st);
    return 1;
  }else if(enif_is_identical(term, st->priv->am_null)){
    b_puts(4, "null", st);
    return 1;
  }
  
  unsigned len, reserve;
  b_reserve(256 + 2, st);
  unsigned char *p = st->cur;
  if((len = enif_get_atom(env, term, (char*)p + 1, 256U, ERL_NIF_LATIN1))){
    *p = '"';
    if(!check_str_for_json((unsigned char*)p + 1, len-1, &reserve)){
      return 0;
    }
    if(reserve > 0){
      b_reserve(len + reserve + 2, st);
      extend_str_to_jstr((unsigned char*)p + 1, len-1, reserve);
    }
    st->cur += (len + reserve);
    b_putc('"', st);
    return 1;
  }
  return 0;
}
static inline int
match_atom_as_string(ErlNifEnv* env, ERL_NIF_TERM term, State *st){
  unsigned len, reserve;
  b_reserve(256 + 2, st);
  unsigned char *p = st->cur;
  if((len = enif_get_atom(env, term, (char*)p + 1, 256U, ERL_NIF_LATIN1))){
    *p = '"';
    if(!check_str_for_json((unsigned char*)p + 1, len-1, &reserve)){
      return 0;
    }
    if(reserve > 0){
      b_reserve(len + reserve + 2, st);
      extend_str_to_jstr((unsigned char*)p + 1, len-1, reserve);
    }
    st->cur += (len + reserve);
    b_putc('"', st);
    return 1;
  }
  return 0;
}

static inline int
match_binary(ErlNifEnv* env, ERL_NIF_TERM term, State *st){
  ErlNifBinary bin;
  unsigned reserve;
  if(!enif_inspect_binary(env, term, &bin))
    return 0;
  if(!check_str_for_json(bin.data, bin.size, &reserve)){
    //FIXME handle error
    return 0;
  }
  size_t len = bin.size + reserve;
   b_reserve(len + 2, st);
  *(st->cur++) = '"';
  if(reserve > 0){
    copy_str_to_jstr(st->cur, bin.data, bin.size);
  }else{
    memcpy(st->cur, bin.data, len);
  }
  st->cur += len;
  *(st->cur++) = '"';
  return 1;
}

static inline int
match_int(ErlNifEnv* env, ERL_NIF_TERM term, State *st){
  int ip, n;
  if(!enif_get_int(env, term, &ip))
    return 0;
  b_reserve(24, st);
  n = sprintf((char*)(st->cur), "%d", ip);
  b_seek(n, st);
  return 1;  
}

static inline int
match_int64(ErlNifEnv* env, ERL_NIF_TERM term, State *st){
  ErlNifSInt64 ip;
  int n;
  if(!enif_get_int64(env, term, &ip))
    return 0;
  b_reserve(24, st);
#if (defined(__WIN32__) || defined(_WIN32) || defined(_WIN32_))
  n = sprintf((char*)st->cur, "%ld", ip);
#elif SIZEOF_LONG == 8
  n = sprintf((char*)st->cur, "%ld", ip);
#else
  n = sprintf((char*)st->cur, "%lld", ip);
#endif
  b_seek(n, st);
  return 1;  
}

static inline int
match_double(ErlNifEnv* env, ERL_NIF_TERM term, State *st){
  double dp;
  int n;
  if(!enif_get_double(env, term, &dp))
    return 0;
  b_reserve(24, st);
  n = sprintf((char *)st->cur, "%.15g", dp);
  b_seek(n, st);
  return 1;  
}

static inline int
match_empty_list(ErlNifEnv* env, ERL_NIF_TERM term, State *st){
  if(!enif_is_empty_list(env, term)){
    return 0;
  }
  b_putc2('[', ']', st);
  return 1;  
}

static inline int
match_string(ErlNifEnv* env, ERL_NIF_TERM term, State *st){
  if(match_binary(env, term, st)){
    return 1;
  }else if(match_atom_as_string(env, term, st)){
    return 1;
  }
  return 0;
}

static inline int
match_pair(ErlNifEnv* env, ERL_NIF_TERM term, State *st){
  const ERL_NIF_TERM *tuple;
  int arity;
  if(enif_get_tuple(env, term, &arity, &tuple)){
    if(arity == 2){
      if(match_string(env, tuple[0], st)){
	b_putc(':', st);
	if(match_term(env, tuple[1], st)){
	  return 1;
	}
      }
    }
  }
  return 0;
}

static inline int
match_proplist(ErlNifEnv* env, ERL_NIF_TERM term, State *st){
  ERL_NIF_TERM list, head, tail;
  unsigned len, i;
  if(!enif_get_list_length(env, term, &len)){
    return 0;
  }
  b_putc('{',  st);
  list = term;
  for(i = 0; i < len; i++){
    enif_get_list_cell(env, list, &head, &tail);
    if(i > 0){
      b_putc(',',  st);
    }
    if(!match_pair(env, head, st)){
      return 0;
    }
    list = tail;
  }
  b_putc('}',  st);
  return 1;
}

static inline int
match_list(ErlNifEnv* env, ERL_NIF_TERM term, State *st){
  ERL_NIF_TERM list, head, tail;
  unsigned len;
  if(!enif_get_list_length(env, term, &len))
    return 0;
  b_putc('[',  st);
  list = term;
  enif_get_list_cell(env, list, &head, &tail);
  if(!match_term(env, head, st)){
    b_unputc(st); // delete '[';
    return 0;
  }
  list = tail;   
  while(enif_get_list_cell(env, list, &head, &tail)){
    b_putc(',',  st);
    if(!match_term(env, head, st)){
      return 0;
    }
    list = tail;
  }
  b_putc(']',  st);
  return 1;
}


static inline int
match_json(ErlNifEnv* env, ERL_NIF_TERM term, State *st){
  ErlNifBinary bin;
  if(enif_inspect_iolist_as_binary(env, term, &bin)){
    b_reserve(bin.size, st);
    memcpy(st->cur, bin.data, bin.size);
    b_seek(bin.size, st);
    return 1;
  }
  return 0;
}

static inline int
enc_should_be_ignored(ErlNifEnv* env, ERL_NIF_TERM atom, const EncEntry* entry){
  if(!enif_is_atom(env, atom)){
    return 0;
  }
  unsigned i;
  for(i=0; i<entry->ignored_len; ++i){
    if(enif_is_identical(atom, entry->ignored[i])){
      return 1;
    }
  }
  return 0;
}


static inline int
match_record(ErlNifEnv* env, int arity, const ERL_NIF_TERM *tuple, State *st){

  EncRecord *records = enc_records_base(st->records);
  EncField *fields = enc_fields_base(st->records);
  int i, k;
  for(i = 0; i < st->records->records_cnt; i++){
    if(records[i].tag == tuple[0] &&  records[i].arity == (arity -1)){
      unsigned fds_offset = records[i].fds_offset;
      unsigned bin_size = 0;
      b_putc('{', st);
      for(k = 0; k < records[i].arity; k++){

        if(!enc_should_be_ignored(env, tuple[k+1], st->records)){
          EncField field = fields[fds_offset + k];
          bin_size += field.size;
	//FIXME {
          b_puts(field.size, st->records->bin.data + field.offset, st);
          if(!match_term(env, tuple[k+1], st))
            return 0;
        }
      }
      b_putc('}', st);
      return 1;
    }
  }
  return 0;
}

static inline int
match_tuple(ErlNifEnv* env, ERL_NIF_TERM term, State *st){ 
  const ERL_NIF_TERM *tuple;
  int arity;
  if(!enif_get_tuple(env, term, &arity, &tuple))
    return 0;
  if(arity > 0){ 
    if(enif_is_atom(env, tuple[0])){
      if(arity == 2){
	if(enif_is_identical(tuple[0], st->priv->am_struct)){
	  //struct
	  return match_proplist(env, tuple[1], st);
	}else   if(enif_is_identical(tuple[0], st->priv->am_json)){
	  //json
	  return  match_json(env, tuple[1], st);
	}
      }
      //records
      if(st->records){
	return match_record(env, arity, tuple, st);
      }
    }
    if(arity == 1){
      //eep18
      return (match_proplist(env, tuple[0], st));
    }
  }
  return 0;  
}

  static int
    match_term(ErlNifEnv* env, ERL_NIF_TERM term, State *st){
    if      (match_binary(env, term, st)){
      return 1;
    }else if(match_atom(env, term, st)){
      return 1;
    }else if(match_int(env, term, st)){
      return 1;
    }else if(match_empty_list(env, term, st)){
      return 1;
    }else if(match_list(env, term, st)){
      return 1;
    }else if(match_proplist(env, term, st)){
      return 1;
    }else if(match_tuple(env, term, st)){
      return 1;
    }else if(match_double(env, term, st)){
      return 1;
    }else if(match_int64(env, term, st)){
      return 1;
    }
    if(!st->no_match){
      st->no_match = term;
    }
    return 0;
  }

  ERL_NIF_TERM
    encode_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
    State st = {
      .priv = (PrivData*)enif_priv_data(env),
      .no_match =  0,
      .ret =  0,
      .bin = {0, NULL},
      .cur = NULL,
      .records = NULL
    };
    if(argc == 2){
      EncEntry *entry_rs;
      assert(enif_get_resource(env, argv[1], st.priv->encoder_RSTYPE, (void**)&entry_rs));
      st.records = entry_rs;
    }
    assert(enif_alloc_binary(FIRST_BIN_SZ, &st.bin));
    st.cur = st.bin.data;

    if(match_term(env, argv[0], &st)){
      enif_realloc_binary(&st.bin, st.cur - st.bin.data);
      return enif_make_binary(env, &st.bin);
    }else{
      return enif_make_tuple2(env, st.priv->am_no_match, st.no_match);
    }
  }
