/*
     LIFO
     Last In First Out
 */

exception Empty;

module type Stack = {
  type t('a);
  exception Empty;
  let create: unit => t('a);
  let push: ('a, t('a)) => unit;
  let pop: t('a) => 'a;
  let top: t('a) => 'a;
  let clear: t('a) => unit;
  let copy: t('a) => t('a);
  let isEmpty: t('a) => bool;
  let length: t('a) => int;
  let iter: ('a => unit, t('a));
  let fold: (('b, 'a) => 'b, 'b, t('a)) => 'b;
};

module Stack = {
  type t('a) = {
    mutable c: 'a,
    mutable len: int,
  };

  let create = () => {c: [], len: 0};

  /* Append the first element on the head of the list */
  let push = (x, s) => {c: [x, ...s.c], len: List.length(s.c)};

  /* Remove the last element coming to the list*/
  let pop = s =>
    switch (s.c) {
    | [hd, ...tl] =>
      s.c = tl;
      s.len = s.len - 1;
      hd;
    | [] => raise(Empty)
    };

  let popOpt = s =>
    switch (s.c) {
    | [hd, ...tl] =>
      s.c = tl;
      s.len = s.len - 1;
      Some(hd);
    | [] => None
    };

  let top = s =>
    switch (s.c) {
    | [hd, ..._tl] => hd
    | [] => raise(Empty)
    };

  let topOpt = s =>
    switch (s.c) {
    | [hd, ..._tl] => Some(hd)
    | [] => None
    };

  let clear = s => {
    s.c = [];
    s.len = 0;
  };

  let copy = s => {c: s.c, len: s.len};

  let isEmpty = s =>
    switch (s.c) {
    | [] => false
    | _ => true
    };

  let length = s => s.len;

  let iter = (fn, s) => List.iter(fn, s.c);

  let fold = (fn, acc, s) => List.fold_left(fn, acc, s.c);
};