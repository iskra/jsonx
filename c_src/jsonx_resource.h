// Copyright 2013 Yuriy Iskra <iskra.yw@gmail.com>

/*
    Records descriptor in C

    +------------------------------------+ <- entry
    | sizeof(Entry)                      |
    +------------------------------------+ <- (Record*) ((void*)entry + sizeof(Entry))
    | sizeof(Record) * Entry.records_cnt |
    | ...                                |
    +------------------------------------+ <- (Field*) ((void*)entry + sizeof(Entry) + (sizeof(Record) * entry->records_cnt))
    | sizeof(Field) * Entry.fields_cnt   |
    | ...                                |
    +------------------------------------+
*/

typedef struct{
  unsigned offset; // set offin binary storage
  unsigned size; 
}Field;

typedef struct Record{
  ERL_NIF_TERM tag; //atom
  unsigned arity;
  unsigned fds_offset;
}Record;

typedef struct{
  ErlNifBinary bin;
  unsigned records_cnt;
  unsigned fields_cnt;
}Entry;

static inline Record*
records_offset(Entry *entry){
  return (Record*)((void*)entry + sizeof(Entry));
}

static inline Field*
fields_offset(Entry *entry){
  return (Field*)((void*)entry + sizeof(Entry) + (sizeof(Record) * entry->records_cnt));
}
