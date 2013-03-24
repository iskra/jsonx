// Copyright 2013 Yuriy Iskra <iskra.yw@gmail.com>

#include "erl_nif.h"
#include "jsonx.h"

static int
load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info){

  Atoms *atoms = enif_alloc(sizeof(Atoms));
  if(atoms == NULL) return 1;

  if(!enif_make_existing_atom(env, "true",     &(atoms->am_true),     ERL_NIF_LATIN1)) return 1;
  if(!enif_make_existing_atom(env, "false",    &(atoms->am_false),    ERL_NIF_LATIN1)) return 1;
  if(!enif_make_existing_atom(env, "null",     &(atoms->am_null),     ERL_NIF_LATIN1)) return 1;

  if(!enif_make_existing_atom(env, "error",          &(atoms->am_error),   ERL_NIF_LATIN1)) return 1;
  if(!enif_make_existing_atom(env, "big_num",   &(atoms->am_erange),  ERL_NIF_LATIN1)) return 1;
  if(!enif_make_existing_atom(env, "invalid_string", &(atoms->am_estr),    ERL_NIF_LATIN1)) return 1;
  if(!enif_make_existing_atom(env, "invalid_json",   &(atoms->am_esyntax), ERL_NIF_LATIN1)) return 1;
  if(!enif_make_existing_atom(env, "trailing_data",  &(atoms->am_etrailing),ERL_NIF_LATIN1)) return 1;

  if(!enif_make_existing_atom(env, "json",     &(atoms->am_json),     ERL_NIF_LATIN1)) return 1;
  if(!enif_make_existing_atom(env, "struct",   &(atoms->am_struct),   ERL_NIF_LATIN1)) return 1;
  if(!enif_make_existing_atom(env, "proplist", &(atoms->am_proplist), ERL_NIF_LATIN1)) return 1;
  if(!enif_make_existing_atom(env, "eep18",    &(atoms->am_eep18),    ERL_NIF_LATIN1)) return 1;
  if(!enif_make_existing_atom(env, "no_match", &(atoms->am_no_match), ERL_NIF_LATIN1)) return 1;

  *priv_data = (void*)atoms;

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

