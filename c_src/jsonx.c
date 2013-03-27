// Copyright 2013 Yuriy Iskra <iskra.yw@gmail.com>

#include <string.h>
#include <assert.h>
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


// %% %% Records descriptions
// %% {
// %%   Rcnt                                 %% argv[0], Records count
// %%  ,Fcnt                                 %% argv[1], Counter all fields in records
// %%  ,Records = [{Tag, Fields_off, Arity}] %% argv[2], List of records tag, position and length fields
// %%  ,Fields  = [{Name_off, Size}]         %% argv[3], List of position and size fields names in binary storage
// %%  ,Bsz                                  %% argv[4], Binary data size
// %%  ,Bin                                  %% argv[5], Binary storage for names of fields, format - <,"name": >
// %% }

static ERL_NIF_TERM
make_records_resource_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
  unsigned rs_len, fs_len, bin_sz;
  enif_get_uint(env, argv[0], &rs_len);
  enif_get_uint(env, argv[1], &fs_len);
  enif_get_uint(env, argv[4], &bin_sz);
  PrivData* priv = (PrivData*)enif_priv_data(env);
  unsigned resource_sz = sizeof(Entry) + sizeof(Record)*rs_len + sizeof(Field)*fs_len;
  Entry *entry_rs = (Entry*)enif_alloc_resource(priv->records_RSTYPE, resource_sz); 
  //fprintf(stderr, "Alloc Resource: %p\r\n", entry_rs);
  assert(entry_rs);
  memset(entry_rs, 0, resource_sz);
  entry_rs->records_cnt = rs_len;
  entry_rs->fields_cnt = fs_len;
  if(!enif_alloc_binary(bin_sz + 1, &entry_rs->bin))
    goto error;
  memset(entry_rs->bin.data, 0, bin_sz + 1);
  ErlNifBinary ebin;
  assert(enif_inspect_binary(env, argv[5], &ebin));
  assert(ebin.size == (entry_rs->bin.size - 1));
  memcpy(entry_rs->bin.data, ebin.data , ebin.size);

  ERL_NIF_TERM list, head, tail;
  list = argv[2];
  int i = 0;
  while(enif_get_list_cell(env, list, &head, &tail)){
    const ERL_NIF_TERM *tuple;
    int arity;
    unsigned ip;
    assert(enif_get_tuple(env, head, &arity, &tuple));
    assert(arity == 3);
    Record *records = records_offset(entry_rs);
    records[i].tag = tuple[0];
    assert(enif_get_uint(env, tuple[1], &ip));
    records[i].fds_offset = ip;
    assert(enif_get_uint(env, tuple[2], &ip));
    records[i].arity = ip;
    i++;
    list = tail;
  }
  list = argv[3];
  i = 0;
  while(enif_get_list_cell(env, list, &head, &tail)){
    const ERL_NIF_TERM *tuple;
    int arity;
    unsigned ip;
    assert(enif_get_tuple(env, head, &arity, &tuple));
    assert(arity == 2);
    Field *fields = fields_offset(entry_rs);
    assert(enif_get_uint(env, tuple[0], &ip));
    fields[i].offset = ip;
    assert(enif_get_uint(env, tuple[1], &ip));
    fields[i].size = ip;
    i++;
    list = tail;
  }
  ERL_NIF_TERM ret = enif_make_resource(env, (void *)entry_rs);
  enif_release_resource(entry_rs);
  return ret;
 error:
  enif_release_resource(entry_rs);
  return enif_make_badarg(env);
}

static ErlNifFunc
nif_funcs[] = {
  {"decode",  1, decode_nif},
  {"decode2", 2, decode_nif},
  {"encode",  1, encode_nif},
  {"encode",  2, encode_nif},
  {"make_records_resource", 6, make_records_resource_nif}
};

ERL_NIF_INIT(jsonx, nif_funcs, load, reload, upgrade, unload);

