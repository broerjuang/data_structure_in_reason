/*
     LIFO
     Last In First Out
 */

exception Empty;

type t('a) = {
  mutable c: 'a,
  mutable len: int,
};

let create = () => {c: [], len: 0};

/*
     Append the first element on the head of the list
 */

let push = (x, s) => {c: [x, ...s.c], len: List.length(s.c)};

/*
     Remove the last element coming to the list
     this pop will return an exception if no element was there

 */
let pop = s =>
  switch (s.c) {
  | [hd, ...tl] =>
    s.c = tl;
    s.len = s.len - 1;
    hd;
  | [] => raise(Empty)
  };

/*
    This is the same implementation of pop but return option instead
    rather than raise exception
 */

let popOpt = s =>
  switch (s.c) {
  | [hd, ...tl] =>
    s.c = tl;
    s.len = s.len - 1;
    Some(hd);
  | [] => None
  };

/*
    This will return the top of the stack
    and raise Empty if no element was there

 */

let top = s =>
  switch (s.c) {
  | [hd, ..._tl] => hd
  | [] => raise(Empty)
  };

/*
    Same as top but return option
 */

let topOpt = s =>
  switch (s.c) {
  | [hd, ..._tl] => Some(hd)
  | [] => None
  };

/*
    this will reset the stack
 */

let clear = s => {
  s.c = [];
  s.len = 0;
};

/*
    copy will return new stack
 */

let copy = s => {c: s.c, len: s.len};

/*
    this will check wheter the element isEmpty or not
    however, we might have more than one implementation here.

    For example,

    let isEmpty = s => s.c == []

 */

let isEmpty = s =>
  switch (s.c) {
  | [] => false
  | _ => true
  };

let length = s => s.len;

let iter = (fn, s) => List.iter(fn, s.c);

let fold = (fn, acc, s) => List.fold_left(fn, acc, s.c);