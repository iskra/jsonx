// Copyright 2013 Yuriy Iskra <iskra.yw@gmail.com>

#include <string.h>
#include <ctype.h>
#include <assert.h>
#include <errno.h>
#include "erl_nif.h"
#include "jsonx_str.h"

typedef struct{
  ERL_NIF_TERM am_true;
  ERL_NIF_TERM am_false;
  ERL_NIF_TERM am_ok;

  ERL_NIF_TERM am_null;
  ERL_NIF_TERM am_start_map;
  ERL_NIF_TERM am_map_key;
  ERL_NIF_TERM am_end_map;
  ERL_NIF_TERM am_start_array;
  ERL_NIF_TERM am_end_array;
  ERL_NIF_TERM am_parse_buf;
  ERL_NIF_TERM am_parse_end;

  ERL_NIF_TERM am_error;
  ERL_NIF_TERM am_erange;
  ERL_NIF_TERM am_estr;
  ERL_NIF_TERM am_esyntax;
}PrivData;

#define ERROR         0
#define START         1
#define COMPLETTE     2
#define COMMA         3
#define KEY_COMPLETTE 4

typedef struct{
  ErlNifEnv     *env;
  ErlNifBinary  bin;
  unsigned char *cur;
  unsigned char *top;
  PrivData      *priv;
  int           m_state;
}State;

ErlNifResourceType* stream_RSTYPE;

static void
stream_rt_dtor(ErlNifEnv* env, void* obj){
  State *entry = (void*)obj;
  enif_release_binary(&entry->bin);
  entry->bin.data = NULL;
  entry->bin.size = 0;
}

static int
load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info){

  PrivData *pdata = enif_alloc(sizeof(PrivData));
  if(pdata == NULL) return 1;

  stream_RSTYPE = enif_open_resource_type(env, NULL,
					  "stream_RSTYPE",
					  stream_rt_dtor,
					  ERL_NIF_RT_CREATE, NULL);
  if (stream_RSTYPE == NULL) return 1;

  if(!enif_make_existing_atom(env, "true",     	  &(pdata->am_true),        ERL_NIF_LATIN1)) return 1;
  if(!enif_make_existing_atom(env, "false",    	  &(pdata->am_false),       ERL_NIF_LATIN1)) return 1;
  if(!enif_make_existing_atom(env, "ok",    	  &(pdata->am_ok),       ERL_NIF_LATIN1)) return 1;

  if(!enif_make_existing_atom(env, "null",     	  &(pdata->am_null),        ERL_NIF_LATIN1)) return 1;
  if(!enif_make_existing_atom(env, "start_map",   &(pdata->am_start_map),   ERL_NIF_LATIN1)) return 1;
  if(!enif_make_existing_atom(env, "map_key",     &(pdata->am_map_key),     ERL_NIF_LATIN1)) return 1;
  if(!enif_make_existing_atom(env, "end_map",     &(pdata->am_end_map),     ERL_NIF_LATIN1)) return 1;
  if(!enif_make_existing_atom(env, "start_array", &(pdata->am_start_array), ERL_NIF_LATIN1)) return 1;
  if(!enif_make_existing_atom(env, "end_array",   &(pdata->am_end_array),   ERL_NIF_LATIN1)) return 1;
  if(!enif_make_existing_atom(env, "parse_buf",   &(pdata->am_parse_buf),   ERL_NIF_LATIN1)) return 1;
  if(!enif_make_existing_atom(env, "parse_end",   &(pdata->am_parse_end),   ERL_NIF_LATIN1)) return 1;

  if(!enif_make_existing_atom(env, "error",             &(pdata->am_error),    ERL_NIF_LATIN1)) return 1;
  if(!enif_make_existing_atom(env, "big_num",           &(pdata->am_erange),  	      ERL_NIF_LATIN1)) return 1;
  if(!enif_make_existing_atom(env, "invalid_string",    &(pdata->am_estr),    	      ERL_NIF_LATIN1)) return 1;
  if(!enif_make_existing_atom(env, "invalid_json",      &(pdata->am_esyntax), 	      ERL_NIF_LATIN1)) return 1;

  *priv_data = (void*)pdata;

  return 0;
}

static int
reload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info){
  return 0;
}

static int
upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info){
  return 0;
}
static void
unload(ErlNifEnv* env, void* priv_data){
  return;
}


static inline unsigned char
look_ah(State *st){
  while(isspace(*st->cur))
    st->cur++;
  return *(st->cur);
}

static inline ERL_NIF_TERM
make_error(State* st, ERL_NIF_TERM resume){
  st->m_state = ERROR;
  *st->top = 'e';
  return enif_make_tuple2(st->env, st->priv->am_error, resume);
}

static inline ERL_NIF_TERM
parse_string(State* st){
  unsigned char *endptr;
  unsigned char *endstr;
  unsigned char *dst;
  ERL_NIF_TERM ret;
  if(check_noescaped_jstr(st->cur, &endptr)){
    if(*endptr == '"'){
	dst = enif_make_new_binary(st->env, endptr - st->cur - 1, &ret);
	memcpy(dst, st->cur + 1, endptr - st->cur - 1);
      st->cur = endptr + 1;
      return ret;
    }else if(*endptr == '\\'){
      if(check_with_unescape_jstr(endptr, &endstr, &endptr)){
	dst = enif_make_new_binary(st->env, endstr - st->cur - 1, &ret);
	memcpy(dst, st->cur + 1, endstr - st->cur - 1);
	st->cur = endptr + 1;
	return ret;
      }
    }
  }
  return make_error(st, st->priv->am_estr);
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
    return make_error(st, st->priv->am_erange);
  }

  if(*endptr == '.' || *endptr == 'e' || *endptr == 'E'){
    float_num = strtod((char *)st->cur, &endptr);
    if(errno != ERANGE){
      st->cur = (unsigned char*)endptr;
      return enif_make_double(st->env, float_num);
    }
    else{
      return make_error(st, st->priv->am_erange);
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
  return make_error(st, st->priv->am_esyntax);
}

static inline ERL_NIF_TERM
parse_false(State* st){
  if(!(strncmp("alse", (char*)(++st->cur), 4))){
    st->cur = st->cur + 4;
    return st->priv->am_false;
  }
  return make_error(st, st->priv->am_esyntax);
}

static inline ERL_NIF_TERM
parse_null(State* st){
  if(!(strncmp("ull", (char*)(++st->cur), 3))){
    st->cur = st->cur + 3;
    return st->priv->am_null;
  }
  return make_error(st, st->priv->am_esyntax);
}


static inline ERL_NIF_TERM
parse_key(State *st){
  ERL_NIF_TERM key;
  switch(look_ah(st)){
  case '\"' :
    key = parse_string(st);
    if(st->m_state == ERROR)
      return key;
    if(look_ah(st) == ':'){
      st->m_state =  KEY_COMPLETTE;
      st->cur++;
      return enif_make_tuple2(st->env, st->priv->am_map_key, key);
    }
  case '\0' :
    return st->priv->am_parse_buf;
  default   :
    return make_error(st, st->priv->am_esyntax);
  }
}

static inline ERL_NIF_TERM
parse0(State *st){
  switch(look_ah(st)){
  case '\"' : st->m_state = COMPLETTE; return parse_string(st);
  case 't'  : st->m_state = COMPLETTE; return parse_true(st);
  case 'f'  : st->m_state = COMPLETTE; return parse_false(st);
  case 'n'  : st->m_state = COMPLETTE; return parse_null(st);
  case '-'  :  case '0'  :  case '1'  :  case '2'  :
  case '3'  :  case '4'  :  case '5'  :  case '6'  :
  case '7'  :  case '8'  :  case '9'  :
    st->m_state = COMPLETTE; return  parse_number(st);
  case '['  :
    st->m_state = START;
    *(++st->top) = '['; st->cur++;
    return st->priv->am_start_array;
  case '{'  :
    st->m_state = START;
    *(++st->top) = '{'; st->cur++;
    return st->priv->am_start_map;
  case '\0' :
    return st->priv->am_parse_buf;
  default:
    st->m_state = ERROR;
    *(st->top) = 'e';
    return make_error(st, st->priv->am_esyntax);
  }
}

ERL_NIF_TERM
get_event_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
  State *st;
  assert(enif_get_resource(env, argv[0], stream_RSTYPE, (void**)&st));
  switch(*st->top){
  case '[' :
    if(st->m_state == COMPLETTE){
      switch(look_ah(st)){
      case ',' : st->m_state = COMMA; st->cur++; return parse0(st);
      case '\0': st->cur++; return st->priv->am_parse_buf;
      case ']' : st->m_state = COMPLETTE; st->cur++; st->top--; return st->priv->am_end_array;
      }
    }else if(st->m_state == COMMA){
      return parse0(st);
    }else if(st->m_state == START){
      switch(look_ah(st)){
      case '\0': st->cur++; return st->priv->am_parse_buf;
      case ']' : st->m_state = COMPLETTE; st->cur++; st->top--; return st->priv->am_end_array;
      default:   return parse0(st);
      }
    }
    break;
  case '{' :
    if(st->m_state == COMPLETTE){
      switch(look_ah(st)){
      case ',' :  st->m_state = COMMA; st->cur++; return parse_key(st);
      case '}' : st->m_state = COMPLETTE; st->cur++; st->top--; return st->priv->am_end_map;
      case '\0': st->cur++; return st->priv->am_parse_buf;
      }
    }else  if(st->m_state == KEY_COMPLETTE){
      return parse0(st);
    }else if(st->m_state == COMMA){
      return parse_key(st);
    }else if(st->m_state == START){
      switch(look_ah(st)){
      case '\0': st->cur++; return st->priv->am_parse_buf;
      case '}' : st->m_state = COMPLETTE; st->cur++; st->top--; return st->priv->am_end_map;
      default: return parse_key(st);
      }
    } 
    break;
  case '\0':
    if(st->m_state == COMPLETTE){
      ERL_NIF_TERM rest;
      look_ah(st);
      size_t size =  strlen((char*)st->cur);
      unsigned char *nb = enif_make_new_binary(env, size, &rest);
      memcpy(nb, st->cur, size);
      return enif_make_tuple2(env, st->priv->am_parse_end, rest);
      return st->priv->am_parse_end;
    }else{
      return parse0(st);
    }
    break;
  case 'e':
  default :
    break;
  }
  return make_error(st, st->priv->am_esyntax);
}

ERL_NIF_TERM
update_decoder_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
  State *st;
  ErlNifBinary input;
  if(!enif_inspect_binary(env, argv[1], &input)){
    return enif_make_badarg(env);
  }
  assert(enif_get_resource(env, argv[0], stream_RSTYPE, (void**)&st));
  size_t stack_size = (st->top + 1 - st->bin.data);

  size_t free = st->bin.size - stack_size - sizeof(ERL_NIF_TERM);
  if(input.size > free){
    enif_realloc_binary(&st->bin, input.size + stack_size + sizeof(ERL_NIF_TERM));
    st->top = st->bin.data + stack_size - 1;
    st->cur = st->top + 1;
    memcpy(st->cur, input.data, input.size);
    *((ERL_NIF_TERM*)(st->cur + input.size)) = 0U;
  }else{
    st->cur = st->top + 1;
    memcpy(st->cur, input.data, input.size);
    *((ERL_NIF_TERM*)(st->cur + input.size)) = 0U;
  }
  return st->priv->am_ok; 
}

ERL_NIF_TERM
make_stream_resource_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
  ErlNifBinary input;
  if(!enif_inspect_binary(env, argv[0], &input)){
    return enif_make_badarg(env);
  }
  size_t alloc_size = input.size + 2 * sizeof(ERL_NIF_TERM);
  State *st = (State*)enif_alloc_resource(stream_RSTYPE, sizeof(State));
  st->env = env;
  st->priv = (PrivData*)enif_priv_data(env);
  st->m_state = START;

  enif_alloc_binary(alloc_size , &st->bin);
  st->cur = st->bin.data + sizeof(ERL_NIF_TERM);
  memcpy(st->cur, input.data, input.size);
  *((ERL_NIF_TERM*)st->bin.data) = 0U;
  *((ERL_NIF_TERM*)(st->bin.data + input.size + sizeof(ERL_NIF_TERM))) = 0U;
  st->top = st->cur - 1;

  ERL_NIF_TERM ret = enif_make_resource(env, (void *)st);
  enif_release_resource(st);
  return ret;
}

static ErlNifFunc
nif_funcs[] = {
  {"new_decoder",    1, make_stream_resource_nif},
  {"update_decoder", 2, update_decoder_nif},
  {"get_event",      1, get_event_nif}
};

ERL_NIF_INIT(jstream, nif_funcs, load, reload, upgrade, unload);

