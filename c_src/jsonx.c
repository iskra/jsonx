// Copyright 2013 Yuriy Iskra <iskra.yw@gmail.com>

#include "jsonx.h"
#include "jsonx_resource.h"

static void
rt_dtor(ErlNifEnv* env, void* obj){
  //assert(obj);
  Entry *entry = (Entry*)obj;
  enif_release_binary(&entry->bin);
  entry->bin.data = NULL;
  entry = NULL;
}

static int
load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info){

  PrivData *pdata = enif_alloc(sizeof(PrivData));
  if(pdata == NULL) return 1;

  pdata->records_RSTYPE = enif_open_resource_type(env, NULL,
					   "records_RSTYPE",
					   rt_dtor,
					   ERL_NIF_RT_CREATE, NULL);
  if (pdata->records_RSTYPE == NULL) return -1;

  if(!enif_make_existing_atom(env, "true",     &(pdata->am_true),     ERL_NIF_LATIN1)) return 1;
  if(!enif_make_existing_atom(env, "false",    &(pdata->am_false),    ERL_NIF_LATIN1)) return 1;
  if(!enif_make_existing_atom(env, "null",     &(pdata->am_null),     ERL_NIF_LATIN1)) return 1;

  if(!enif_make_existing_atom(env, "error",          &(pdata->am_error),   ERL_NIF_LATIN1)) return 1;
  if(!enif_make_existing_atom(env, "big_num",   &(pdata->am_erange),  ERL_NIF_LATIN1)) return 1;
  if(!enif_make_existing_atom(env, "invalid_string", &(pdata->am_estr),    ERL_NIF_LATIN1)) return 1;
  if(!enif_make_existing_atom(env, "invalid_json",   &(pdata->am_esyntax), ERL_NIF_LATIN1)) return 1;
  if(!enif_make_existing_atom(env, "trailing_data",  &(pdata->am_etrailing),ERL_NIF_LATIN1)) return 1;

  if(!enif_make_existing_atom(env, "json",     &(pdata->am_json),     ERL_NIF_LATIN1)) return 1;
  if(!enif_make_existing_atom(env, "struct",   &(pdata->am_struct),   ERL_NIF_LATIN1)) return 1;
  if(!enif_make_existing_atom(env, "proplist", &(pdata->am_proplist), ERL_NIF_LATIN1)) return 1;
  if(!enif_make_existing_atom(env, "eep18",    &(pdata->am_eep18),    ERL_NIF_LATIN1)) return 1;
  if(!enif_make_existing_atom(env, "no_match", &(pdata->am_no_match), ERL_NIF_LATIN1)) return 1;

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

static ErlNifFunc
nif_funcs[] = {
  {"decode",  1, decode_nif},
  {"decode2", 2, decode_nif},
  {"encode",  1, encode_nif}
};

ERL_NIF_INIT(jsonx, nif_funcs, load, reload, upgrade, unload);

