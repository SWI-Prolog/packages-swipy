/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2023, SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

#define ANCHOR_TABLE_SIZE 16

typedef struct hcell
{ struct hcell *next;
  atom_t        name;
  PyObject     *object;
} hcell;

typedef struct htable
{ size_t count;
  size_t size;
  hcell **entries;
} htable;

static htable *
py_new_hashmap(void)
{ htable *t;
  size_t ebytes = sizeof(*t->entries) * ANCHOR_TABLE_SIZE;

  if ( (t = malloc(sizeof(*t))) &&
       (t->entries = malloc(ebytes)) )
  { t->count = 0;
    t->size = ANCHOR_TABLE_SIZE;
    memset(t->entries, 0, ebytes);
    return t;
  }

  if ( t )
    free(t);

  PL_resource_error("memory");
  return NULL;
}

static void
py_free_hashmap(htable *t)
{ if ( t )
  { size_t i;

    for(i=0; i<t->size; i++)
    { hcell *c = t->entries[i];
      hcell *n;

      for(; c; c=n)
      { n = c->next;
	PL_unregister_atom(c->name);
	free(c);
      }
    }

    free(t->entries);
    free(t);
  }
}

#define SEED 0x6263533a
#define MIX(h,k,m) { k *= m; k ^= k >> r; k *= m; h *= m; h ^= k; }

static unsigned int
MurmurHashAligned2(const void * key, size_t len, unsigned int seed)
{ const unsigned int m = 0x5bd1e995;
  const int r = 24;
  unsigned int h = seed ^ (unsigned int)len;
  const unsigned char * data = (const unsigned char *)key;

  while( len >= 4 )
  { unsigned int k;

    k  = data[0];
    k |= data[1] << 8;
    k |= data[2] << 16;
    k |= data[3] << 24;

    MIX(h,k,m);

    data += 4;
    len -= 4;
  }

  switch( len )
  { case 3: h ^= data[2] << 16;
    case 2: h ^= data[1] << 8;
    case 1: h ^= data[0];
      h *= m;
  };

  h ^= h >> 13;
  h *= m;
  h ^= h >> 15;

  return h;
}

static int
rehash(htable *t)
{ size_t newsize = t->size*2;
  size_t ebytes = sizeof(*t->entries) * newsize;
  hcell **new;

  if ( (new=malloc(ebytes)) )
  { size_t i;

    memset(new, 0, ebytes);
    for(i=0; i<t->size; i++)
    { hcell *c = t->entries[i];
      hcell *n;

      for(; c; c=n)
      { int k = MurmurHashAligned2(&c->name, sizeof(c->name), SEED) % newsize;

	n = c->next;
	c->next = new[k];
	new[k] = c;
      }
    }

    free(t->entries);
    t->size = newsize;
    t->entries = new;
    return FALSE;
  }

  return PL_resource_error("memory");
}


static int
py_add_hashmap(htable *t, atom_t name, PyObject *obj, PyObject **old)
{ unsigned int k;
  hcell *c;

  if ( t->count > t->size )
  { if ( !rehash(t) )
      return FALSE;
  }

  k = MurmurHashAligned2(&name, sizeof(name), SEED) % t->size;
  for(c=t->entries[k]; c; c=c->next)
  { if ( c->name == name )
    { if ( c->object != obj )
      { if ( old )
	  *old = c->object;
	c->object = obj;
      }
      return TRUE;
    }
  }

  if ( (c=malloc(sizeof(*c))) )
  { c->name       = name;
    c->object     = obj;
    c->next       = t->entries[k];
    t->entries[k] = c;
    t->count++;

    PL_register_atom(name);

    return TRUE;
  }

  return PL_resource_error("memory");
}

static PyObject *
py_lookup_hashmap(htable *t, atom_t name)
{ unsigned int k = MurmurHashAligned2(&name, sizeof(name), SEED) % t->size;
  hcell *c;

  for(c=t->entries[k]; c; c=c->next)
  { if ( c->name == name )
      return c->object;
  }

  return NULL;
}
